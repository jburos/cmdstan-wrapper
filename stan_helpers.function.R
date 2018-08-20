library(rstan)
library(doParallel)
library(foreach)
library(tidyverse)
library(bayesplot)
library(stringr)
library(purrr)
library(ggplot2)
library(lubridate)
library(glue)
library(digest)
library(loo)

make_standata <- function(data, file, metadata_file) {
  rstan::stan_rdump(ls(data), file, envir = list2env(data))
  if (!missing(metadata_file))
    saveRDS(object = attributes(data), file = metadata_file)
}

prep_control_sublist <- function(control) {
    if (!is.list(control))
        stop('control items must be a named list.')
    control_names <- names(control)
    if (length(control_names) != length(control))
        stop('control items must be a named list.')
    stringr::str_c(stringr::str_c(control_names, control, sep = '='),
                   collapse = ' ')
}

prep_control_inputs <- function(control) {
    if (is.list(control) && all(purrr::map_lgl(control, ~ !is.list(.)) == TRUE))
        prep_control_sublist(control)
    else {
        if (!is.list(control))
            stop('control items must be a named list.')
        control_names <- names(control)
        if (length(control_names) != length(control))
            stop('control items must be a named list.')
        stringr::str_c(stringr::str_c(control_names,
                                      map_chr(control, prep_control_inputs),
                                      sep=' '),
                       collapse = ' ')
    }
}

write_init_file <- function(data, file) {
    nms <- names(data)
    vals <- data
    lines <- nms %>% purrr::map_chr(~ str_c(.x, str_c(deparse(data[[.x]]), collapse=''), sep = ' <- '))
    writeLines(text = lines, con = file)
}

fit_stan_model <- function(stan_data, stan_model=stan_model, label='default',
                           chains = 4,
                           iter = 1000,
                           init = 1,
                           init_f = NULL,
                           warmup = iter/2,
                           samples = iter/2,
                           seed = 12345,
                           cores = chains,
                           skip_if_exists = TRUE,
                           adapt_delta = 0.99,
                           max_treedepth = 15,
                           with_predict = TRUE,
                           survprob_fun = predict_survival,
                           progprob_fun = predict_recist_pd,
                           control = list(adapt = list(delta = adapt_delta),
                                          list(algorithm = 'hmc', engine = 'nuts', max_depth = max_treedepth),
                                          list(num_samples = samples, num_warmup = warmup, init = init)),
                           survprob_args = list(time = c(0, seq_len(10))*70),
                           progprob_args = list(time = c(0, seq_len(10))*10)
                           ) {
    set.seed(seed)
    seeds <- ceiling(runif(chains, 0, 10^6))

    # filenames for generated outputs
    # (hash stan_data without attributes)
    naked_data <- stan_data
    attributes(naked_data) <- attributes(naked_data)[1] # only names attribute
    datahash <- digest::digest(naked_data, algo = 'md5', serialize = T)
    str(naked_data)
    print(glue::glue('datahash is {datahash}'))
    config <- stringr::str_c(adapt_delta, max_treedepth, init, samples, warmup, seed, sep = '-')
    stan_data_file <- paste(c(stan_model, 'data', label, datahash, 'Rds'), collapse='.')
    stan_metadata_file <- paste(c(stan_model, 'metadata', label, datahash, 'Rds'), collapse = '.')
    stan_results_file <- paste(c(stan_model, 'chain{i}', label, datahash, config, '{chain_seed}', 'txt'), collapse='.')
    stan_summary_file <- paste(c(stan_model, 'summary', label, datahash, config, 'txt'), collapse='.')
    stan_fit_file <-  paste(c(stan_model, 'fit', label, datahash, config, 'Rds'), collapse='.')
    stan_init_file <- paste(c(stan_model, 'chain{i}-init', label, datahash, config, 'Rds'), collapse='.')
    survprob_file <- NULL
    progprob_file <- NULL
    if (with_predict) {
      if (!is.null(survprob_fun)) {
        survprob_hash <- digest::digest(survprob_args, algo = 'md5', serialize = T)
        survprob_file <- paste(c(stan_model, 'survprob', label, datahash, config, survprob_hash, 'Rds'), collapse='.')
      }
      if (!is.null(progprob_fun)) {
        progprob_hash <- digest::digest(progprob_args, algo = 'md5', serialize = T)
        progprob_file <- paste(c(stan_model, 'progprob', label, datahash, config, progprob_hash, 'Rds'), collapse='.')
      }
    }

    # write rdump of data to disk
    make_standata(stan_data, file = stan_data_file, metadata_file = stan_metadata_file)

    # update control if init_f given (even though not yet populated)
    if (!is.null(init_f))
        control[[3]]$init <- stan_init_file
    # construct control parameters
    if (!is.null(control) & length(control)>0)
        control_str <- prep_control_inputs(control)
    else
        control_str <- ''

    # compile stan model
    cmdstan <- './cmdstan'
    compiled_model <- stringr::str_replace(normalizePath(stan_model), pattern='\\.stan$', repl='')
    rc <- system(glue::glue('cd {cmdstan} && make -j{cores} {compiled_model}'))
    stopifnot(rc == 0)

    # fit stan model using cmdstan
    registerDoParallel(cores = cores)
    cluster <- makeCluster(cores)
    on.exit(expr = {stopCluster(cluster)}
            , add = TRUE)
    output_files <- foreach(i = 1:chains,
                            .export = c("compiled_model", "samples",
                                        "stan_data_file", "stan_init_file",
                                        "output_file", "skip_if_exists",
                                        "new_file", "control_str", "seeds", "init_f",
                                        "write_init_file", "make_standata")
                            ) %dopar% {
        library(glue)
        chain_seed = seeds[i]
        # update inits
        set.seed(seeds[i])
        control_str <- glue(control_str)
        if (!is.null(init_f)) {
          init_file <- glue(stan_init_file)
          if (!skip_if_exists || !file.exists(init_file)) {
            init_data <- init_f()
            make_standata(init_data, file = init_file)
          }
        }
        output_file = glue(stan_results_file)
        new_file <- FALSE
        if (!skip_if_exists || !file.exists(output_file)) {
            cmd <- glue('{compiled_model} sample {control_str} random seed={chain_seed} data file={stan_data_file} output file={output_file}.tmp')
            rc <- system(cmd)
            stopifnot(rc == 0)
            file.rename(glue::glue("{output_file}.tmp"), glue::glue("{output_file}"))
            new_file <- TRUE
        }
        structure(output_file, new = new_file)
    }
    new_file <- any(purrr::map_lgl(output_files, attr, "new"))

    if (skip_if_exists && !new_file)
        print("Skipped executing stan fits because output files exist. Re-run with `skip_if_exists = FALSE` to re-execute")
    else
        print('results completed')

    # summarize results
    output_file_list = paste(unlist(output_files), collapse=' ')
    if (new_file || !skip_if_exists || !file.exists(stan_summary_file)) {
        rc = system(glue('./{cmdstan}/bin/stansummary {output_file_list} --csv_file={stan_summary_file} > /dev/null'))
        rm(rc)
    }

    if (new_file || !skip_if_exists || !file.exists(stan_fit_file)) {
        fit <- read_stan_csv(unlist(output_files), col_major = TRUE)
        saveRDS(fit, stan_fit_file)
    } else {
        fit <- readRDS(stan_fit_file)
    }

    if (with_predict && !is.null(survprob_fun)) {
      if (new_file || !skip_if_exists || !file.exists(survprob_file)) {
        print("Starting predict-survprob")
        survprob <- purrr::lift_dl(survprob_fun, fit = fit, metadata = attributes(stan_data), stan_data = stan_data)(survprob_args)
        saveRDS(survprob, survprob_file)
      } else {
        survprob <- readRDS(survprob_file)
      }
    }
    if (with_predict && !is.null(progprob_fun)) {
      if (new_file || !skip_if_exists || !file.exists(progprob_file)) {
        print("Starting predict-progprob")
        progprob <- purrr::lift_dl(progprob_fun, fit = fit, metadata = attributes(stan_data), stan_data = stan_data)(progprob_args)
        saveRDS(progprob, progprob_file)
      } else {
        progprob <- readRDS(progprob_file)
      }
    }
    
    rstan:::throw_sampler_warnings(fit)
    attr(fit, 'stan_data_file') <- stan_data_file
    attr(fit, 'stan_fit_file') <- stan_fit_file
    attr(fit, 'stan_metadata_file') <- stan_metadata_file
    attr(fit, 'survprob_file') <- survprob_file
    attr(fit, 'progprob_file') <- progprob_file
    return(fit)
}

all_matching_pars <- function(fit, pars) {
    all_pars <- dimnames(as.array(fit))$parameters
    matching_pars <- pars %>%
        purrr::map(~ all_pars[stringr::str_detect(all_pars, pattern = .x)]) %>% unlist()
    as.character(matching_pars)
}

plot_coefs <- function(fit, pars = c('mean_alpha', 'beta_beta', 'theta\\[1,', 'merged_beta'), ...) {
  pars <- all_matching_pars(fit, pars)
  if (length(pars) == 0)
    return(NULL)
  bayesplot::mcmc_areas(as.array(fit),
                        pars = pars,
                        prob = 0.8, # 80% intervals
                        prob_outer = 0.99, # 99%
                        point_est = "mean")
}

plot_rhats <- function(fit, ...) {
  rhats <- bayesplot::rhat(fit, ...)
  rhats <- rhats[!is.nan(rhats)]
  rhats <- rhats[!duplicated(rhats)]
  names(rhats) <- NULL
  bayesplot::mcmc_rhat(rhats)
}

plot_neff_ratio <- function(fit, ...) {
  ratios <- bayesplot::neff_ratio(fit, ...)
  bayesplot::mcmc_neff(ratios, size = 2)
}

plot_divergences <- function(fit) {
  bayesplot::mcmc_nuts_divergence(
    bayesplot::nuts_params(fit),
    bayesplot::log_posterior(fit)
  )
}

plot_energy <- function(fit) {
  bayesplot::mcmc_nuts_energy(bayesplot::nuts_params(fit))
}

par_exists <- function(fit, par) {
    par %in% fit@model_pars
}

all_pars_exist <- function(fit, pars) {
   all(purrr::map_lgl(pars, ~ par_exists(fit, .x)))
}

par_if_exists <- function(fit, par) {
    if (par_exists(fit, par))
        par
    else
        NULL
}

only_existing_pars <- function(fit, pars) {
    if (all_pars_exist(fit, pars))
        pars
    else
        purrr::map(pars, ~ par_if_exists(fit, .x)) %>% purrr::compact()
}

fit_diagnostics <- function(fit, pdf_file = NULL,
                            pars = c('log_tumor_diam_hat', 'sigma'),
                            coefs = c('mean_alpha', 'beta_beta', 'merged_beta', 'merged2_beta'),
                            cores = min(c(length(pars), 4)),
                            ...) {
  color_scheme_set("brightblue")
  # limit to pars in the model
  pars <- only_existing_pars(fit, pars)
  if (length(pars) == 0)
      return(NULL)
  rhat_all_plot <- purrr::possibly(~ plot_rhats(.x) + ggplot2::ggtitle(glue::glue('All Rhat values')), NULL)(fit)
  rhat_plot <- purrr::possibly(~ plot_rhats(.x, pars = pars) + ggplot2::ggtitle(glue::glue('Rhat values for {pars}')), NULL)(fit)
  div_plot <- purrr::possibly(plot_divergences, NULL)(fit)
  energy_plot <- purrr::possibly(~ plot_energy(.x) + ggtitle('Histograms of marginal energy'), NULL)(fit)
  if (length(coefs) > 0) {
    coef_plot <- purrr::possibly(~ plot_coefs(.x, pars = coefs) + ggtitle('Posterior estimates of parameters'), NULL)(fit)
  } else {
    coef_plot <- NULL
  }
  all_plots <- list(rhat_all_plot, rhat_plot, div_plot, energy_plot, coef_plot) %>% purrr::compact()
  if (length(all_plots) == 0)
      return(NULL)
  if (!is.null(pdf_file))
    pdf(pdf_file, ...)
  lapply(all_plots, plot)
  if (!is.null(pdf_file))
    dev.off()
}

fit_ppchecks <- function(fit, stan_data, pars, pdf_file = NULL, ...) {
  if (!is.null(pdf_file))
    pdf(pdf_file)

  names(pars) %>%
      purrr::map(function(.x, ...)
                 {
                     plot_ppcheck(fit = fit, stan_data = stan_data,
                                true = .x, ppred = pars[.x],
                                ...)
                 }, ...) %>%
      compact() %>%
      walk(plot)
  if (!is.null(pdf_file))
    dev.off()
}

summarize_calibration <- function(fit, stan_data,
                              true = 'tumor_diam', ppred = 'tumor_diam_rep',
                              by = 'p', x = 'ts', prob = c(0.5, 0.8)) {
    true_vals <- stan_data[[true]]
    group_by <- stan_data[[by]]
    if (!is.null(x))
        x <- stan_data[[x]]
    else
        x <- NULL
    ppmat <- rstan::extract(fit, ppred)[[ppred]]
    ppint <- purrr::map_dfr(prob, ~ bayesplot::ppc_intervals_data(y = true_vals, yrep = ppmat,
                                           group = group_by, x = x, prob = .x)) %>%
      dplyr::bind_rows(
        purrr::map_dfr(prob, ~ bayesplot::ppc_intervals_data(y = true_vals, yrep = ppmat,
                                                                          prob = .x)) %>%
        dplyr::mutate(group = '(overall)')
    )

    ppint %>%
        dplyr::mutate(within_interval = ifelse(y_obs <= hi & y_obs >= lo, 1, 0)) %>%
        dplyr::group_by(group, prob) %>%
        dplyr::summarise(within_interval = mean(within_interval)) %>%
        dplyr::rename(postprob = prob) %>%
        tidyr::spread(postprob, within_interval, sep = '-')
}

plot_loo_interval2 <- function(fit, stan_data, by = 'p',
			       true = 'tumor_diam', ppred = 'tumor_diam_rep',
			       prob = 0.5, log_lik_param = 'log_lik', compare = 'normal',
			       prefix = 'emp') {
  with_prefix <- function(x) {
    stringr::str_c(prefix, x, sep = '_')
  }
  by <- with_prefix(by)
  true <- with_prefix(true)
  ppred <- with_prefix(ppred)
  log_lik_param <- with_prefix(log_lik_param)
  lw <- try(loo::extract_log_lik(fit, parameter_name = log_lik_param, merge_chains = F))
  if (inherits(lw, 'try-error'))
    stop('log-lik not defined for this model; not producing LOO')
  true_vals <- stan_data[[true]]
  ppmat <- rstan::extract(fit, ppred)[[ppred]]
  loores <- loo::loo(x = lw, r_eff = loo::relative_eff(exp(lw)), save_psis = TRUE, cores = 4)
  plot(loores)
  plots <- list(
    bayesplot::ppc_loo_pit_overlay(y = true_vals,
                                   yrep = ppmat,
                                   lw = weights(loores$psis_object)
                                   )
    )
  bayesplot::bayesplot_grid(plots = plots)
}
 


plot_loo_interval <- function(fit, stan_data, by = 'p',
                              true = 'tumor_diam', ppred = 'tumor_diam_rep',
                              prob = 0.5, log_lik_param = 'log_lik', compare = 'normal') {
    lw <- try(loo::extract_log_lik(fit, parameter_name = log_lik_param))
    if (inherits(lw, 'try-error')) {
        print('log-lik not defined for this model; not producing LOO')
        return(NULL)
    }
    true_vals <- stan_data[[true]]
    ppmat <- rstan::extract(fit, ppred)[[ppred]]
    psis <- loo::psis(lw)
    if (!is.null(group_by)) {
        group_by <- stan_data[[by]]
        plots <- list()
        ## todo: plot for each patient
        ## arrange in a grid
        # bayesplot::bayesplot_grid(plots = plots(), ...)
    }
    plots <- list(
                  bayesplot::ppc_loo_pit(y = true_vals, yrep = ppmat, lw = psis$lw_smooth, compare = compare),
                  purrr::possibly(bayesplot::ppc_loo_intervals, NULL)(y = true_vals, yrep = ppmat, lw = psis$lw_smooth)
                  ) %>% purrr::compact()
    bayesplot::bayesplot_grid(plots = plots)
}

plot_ppcheck <- function(fit, stan_data,
                         true = 'tumor_diam', ppred = 'tumor_diam_rep',
                         by = 'p', x = 'ts', prob = 0.5, max_groups = 30,
                         patientid_mapping = NULL, mapping_id = 'stan_patient_id',
                         facet_args = list(), prefix = 'emp', id_map = NULL,
                         filter_subjects = function(x) {TRUE}, ...
                          ) {
  with_prefix <- function(x) {
    stringr::str_c(prefix, x, sep = '_')
  }
  if (!is.null(id_map)) {
    patientid_mapping <- id_map$id_mapping
    mapping_id <- '_id'
    subject_id <- id_map$id_var
  } else {
    subject_id <- 'usubjid'
  }
  by <- with_prefix(by)
  true <- with_prefix(true)
  ppred <- with_prefix(ppred)
  x <- with_prefix(x)
  if (!(by %in% names(stan_data) && true %in% names(stan_data))) {
      print(glue::glue('Skipping ppcheck because either {by} and/or {true} not in stan_data'))
      return(NULL)
  }
  if (!par_exists(fit, ppred)) {
      print(glue::glue('Skipping ppcheck because {ppred} not in stanfit'))
      return(NULL)
  }
  true_vals = stan_data[[true]]
  plot_by = stan_data[[by]]
  if (!is.null(patientid_mapping)) {
    mapping = patientid_mapping %>%
      dplyr::arrange(!!rlang::sym(mapping_id)) %>%
      dplyr::select(!!rlang::sym(subject_id)) %>%
      unlist()
    plot_by = mapping[plot_by]
  }
  ppmat = try(rstan::extract(fit, ppred)[[ppred]], silent = TRUE)
  if (!is.null(x))
      plot_x = stan_data[[x]]
  else
      plot_x = 1:length(true_vals)
  if (!is.null(max_groups)) {
      tmp <- dplyr::tbl_df(list(by = unique(plot_by))) %>%
        dplyr::mutate(keep = filter_subjects(by)) %>%
        dplyr::filter(keep == TRUE) %>%
        dplyr::select(-keep)
      if (nrow(tmp) > max_groups) {
          tmp <- tmp %>% sample_n(max_groups)
          sel <- which(plot_by %in% tmp$by)
          true_vals <- true_vals[sel]
          plot_by <- plot_by[sel]
          plot_x <- plot_x[sel]
          ppmat <- ppmat[,sel]
      }
  }
  if (inherits(ppmat, 'try-error')) {
      print(glue::glue('Skipping ppcheck because {ppred} not in stan model parameters'))
      return(NULL)
  }
  bayesplot::ppc_intervals_grouped(y = true_vals, yrep = ppmat,
                                   x = plot_x, group = plot_by,
                                   prob = prob) +
        ggtitle(glue('Comparing {true} to estimated {ppred}',
                     subtitle = glue('Showing {sprintf("%3.0f %%", prob*100)} credible intervals')))
}


plot_ppvals <- function(fit, stan_data, prefix = 'emp',
                        ppred = 'tumor_diam_rep',
                        by = 'p', x = 'ts', prob = 0.5, max_groups = 30,
                        patientid_mapping = NULL, mapping_id = '_id',
                        id_map = NULL, facet_args = list(), ...
) {
  with_prefix <- function(x) {
    stringr::str_c(prefix, x, sep = '_')
  }
  if (!is.null(id_map))
    patientid_mapping <- id_map$id_mapping
  if (!(by %in% names(stan_data))) {
    print(glue::glue('Skipping plot_ppvals because {by} not in stan_data'))
    return(NULL)
  }
  if (!par_exists(fit, ppred)) {
    print(glue::glue('Skipping plot_ppvals because {ppred} not in stanfit'))
    return(NULL)
  }
  plot_by = stan_data[[with_prefix(by)]]
  if (!is.null(patientid_mapping)) {
    mapping = patientid_mapping %>%
      dplyr::arrange(!!mapping_id) %>%
      dplyr::select(usubjid) %>%
      unlist()
    plot_by = mapping[plot_by]
  }
  ppmat = try(rstan::extract(fit, with_prefix(ppred))[[with_prefix(ppred)]], silent = TRUE)
  if (!is.null(x))
    plot_x = stan_data[[with_prefix(x)]]
  else
    plot_x = NULL
  if (!is.null(max_groups)) {
    tmp <- dplyr::tbl_df(list(by = unique(plot_by)))
    if (nrow(tmp) > max_groups) {
      tmp <- tmp %>% sample_n(max_groups)
      sel <- which(plot_by %in% tmp$by)
      plot_by <- plot_by[sel]
      if (!is.null(plot_x))
        plot_x <- plot_x[sel]
      ppmat <- ppmat[,sel]
    }
  }
  if (inherits(ppmat, 'try-error')) {
    print(glue::glue('Skipping ppcheck because {ppred} not in stan model parameters'))
    return(NULL)
  }
  plot_intervals_grouped(yrep = ppmat,
                         x = plot_x, group = plot_by,
                         prob = prob, facet_args = facet_args, ...) +
    ggtitle(glue('Posterior-predicted values of {ppred}',
                 subtitle = glue('Showing {sprintf("%3.0f %%", prob*100)} credible intervals')))
}

#' Based heavily on bayesplot:::.ppc_intervals_data, but without the yval
plot_intervals_data <- function(yrep, x = NULL, group = NULL, prob = 0.8) {
  stopifnot(prob > 0 && prob < 1)
  metadata <- list(index = seq_len(ncol(yrep)))
  if (!is.null(x)) {
    stopifnot(ncol(yrep) == length(x))
    metadata <- c(metadata, list(x = x))
  }
  if (!is.null(group)) {
    stopifnot(ncol(yrep) == length(group))
    metadata <- c(metadata, list(group = group))
  }
  metadata <- dplyr::tbl_df(metadata)
  d <- as.data.frame(yrep) %>%
    tibble::rowid_to_column('iter') %>%
    tidyr::gather(index, value, -iter) %>%
    dplyr::mutate(index = as.numeric(stringr::str_extract(index, pattern = '[[:digit:]]+'))) %>%
    dplyr::left_join(metadata, by = 'index')
  alpha <- (1 - prob)/2
  probs <- sort(c(alpha, 0.5, 1 - alpha))
  group_vars <- rlang::syms(names(dplyr::select(metadata, -index)))
  d %>%
    dplyr::group_by(!!!group_vars) %>%
    dplyr::summarise(prob = prob,
                     lo = quantile(value, prob = probs[1]),
                     mid = quantile(value, prob = probs[2]),
                     hi = quantile(value, prob = probs[3])) %>%
    dplyr::ungroup()
}

#' Based heavily on bayesplot:::.ppc_intervals_grouped, but without the yval
.plot_intervals_grouped <- function (data, facet_args = list(),
                                     alpha = 0.33,
                                     fatten = 3,
                                     size = 1,
                                     grouped = FALSE,
                                     style = c("intervals", "ribbon"),
                                     x_lab = NULL)
{
  style <- match.arg(style)
  graph <- ggplot(data = data,
                  mapping = aes_(x = ~x, y = ~mid,
                                 ymin = ~lo, ymax = ~hi))
  if (style == "ribbon") {
    graph <- graph +
      geom_ribbon(aes_(color = "yrep", fill = "yrep"),
                  alpha = alpha, size = size) +
      geom_line(aes_(color = "yrep"),
                size = size/2)
  }
  else {
    graph <- graph +
      geom_pointrange(mapping = aes_(color = "yrep",
                                     fill = "yrep"), shape = 21, size = size, fatten = fatten)
  }
  graph <- graph +
    scale_color_manual(name = "",
                       values = setNames(bayesplot:::get_color(c("lh")), c("yrep")),
                       labels = c(yrep = bayesplot:::yrep_label())
                       ) +
    scale_fill_manual(name = "",
                      values = c(yrep = bayesplot:::get_color("l")),
                      labels = c(yrep = bayesplot:::yrep_label()))
  if (grouped) {
    facet_args[["facets"]] <- "group"
    if (is.null(facet_args[["scales"]])) {
      facet_args[["scales"]] <- "free"
    }
    graph <- graph + do.call("facet_wrap", facet_args)
  }
  graph + labs(y = NULL, x = x_lab %||% expression(italic(x)))
}

#' Based heavily on bayesplot:::ppc_intervals_grouped, but without the y
plot_intervals_grouped <- function(yrep, x = NULL, group = NULL, prob = 0.8, ...) {
  data <- plot_intervals_data(yrep = yrep, x = x, group = group, prob = prob)
  .plot_intervals_grouped(data = data, grouped = !is.null(group), style = "intervals", x_lab = bayesplot:::label_x(x), ...)
}

find_root_dir <- function(path = getwd(), pattern=c('projects', 'az-tumorsize')) {
  for (pat in pattern) {
    if (grepl(pat, basename(path)) == TRUE) {
      return(path)
    }
  }
  if (basename(path) == path.expand("~")) {
    return(path) # otherwise, if we are one directory within "$HOME", stop looking
  }
  return(find_root_dir(dirname(path)))
}


load_model_results <- function(stan_model, label, datahash, fit_params = '0.99-15-1-500-500-12345',
                               with_predict = TRUE, 
                               survprob_args = list(time = seq(from = 0, to = 700, by = 50)),
                               survprob_fun = NULL,
                               progprob_args = list(time = seq(from = 0, to = 700, by = 50)),
                               progprob_fun = NULL, force = F,
                               bs_surv_fun = brier_score,
                               bs_surv_args = list(),
                               bs_prog_fun = brier_score_pd,
                               bs_prog_args = list(),
                               description = 'JM model',
                               ...) {
  fitfile <- glue::glue('{stan_model}.fit.{label}.{datahash}.{fit_params}.Rds')
  datafile <- glue::glue('{stan_model}.data.{label}.{datahash}.Rds')
  mdfile <- glue::glue('{stan_model}.metadata.{label}.{datahash}.Rds')
  fit <- readRDS(fitfile)
  stan_data <- rstan::read_rdump(datafile)
  metadata <- readRDS(mdfile)
  if (with_predict) {
    predict_results <- precalc_survprob(fit = fit, stan_data = stan_data,
                                        metadata = metadata, stan_model = stan_model,
                                        label = label, datahash = datahash, fit_params = fit_params,
                                        survprob_args = survprob_args, survprob_fun = survprob_fun,
                                        progprob_args = progprob_args, progprob_fun = progprob_fun,
                                        force = force,bs_surv_fun = bs_surv_fun,
                                        bs_surv_args = bs_surv_args,
                                        bs_prog_fun = bs_prog_fun,
                                        bs_prog_args = bs_prog_args, description = description)
  } else {
    predict_results <- list()
  }
  return(c(
    tibble::lst(fit, stan_data, metadata, ...),
    predict_results))
}

tags_from_args <- function(..., .args = list()) {
  if (length(list(...)) > 0) {
    args <- list(...)
  } else {
    args <- list()
  }
  if (!missing(.args)) {
    args <- c(args, .args)
  }
  # filter to atomic char / numeric args
  args <- purrr::keep(args, ~ (is_character(.) || is_numeric(.) || is_logical(.)) && purrr::is_bare_atomic(.))
  tags <- as.character(purrr::map2(args, names(args), ~ stringr::str_c(.y, .x, sep = ':')))
}

precalc_survprob <- function(fit, stan_data, metadata, description,
                             stan_model, label, datahash, fit_params,
                             survprob_args, survprob_fun, progprob_args, progprob_fun,
                             force = F,
                             bs_surv_fun = brier_score, bs_surv_args = list(),
                             bs_prog_fun = brier_score_pd, bs_prog_args = list()) {
  if (!is.null(survprob_fun)) {
    survprob_hash <- digest::digest(survprob_args)
    survprob_file <- paste(c(stan_model, 'survprob', label, datahash, fit_params, survprob_hash, 'Rds'), collapse='.')
    survscore_hash <- digest::digest(c(bs_surv_args, list(description = description)))
    survscore_file <- paste(c(stan_model, 'survscore', label, datahash, fit_params, survprob_hash, survscore_hash, 'Rds'), collapse='.')
  } else {
    survprob_file <- NULL
    survscore_file <- NULL
  }
  if (!is.null(progprob_fun)) {
    progprob_hash <- digest::digest(progprob_args)
    progprob_file <- paste(c(stan_model, 'progprob', label, datahash, fit_params, progprob_hash, 'Rds'), collapse='.')
    progscore_hash <- digest::digest(c(bs_prog_args, list(description = description)))
    progscore_file <- paste(c(stan_model, 'progscore', label, datahash, fit_params, progprob_hash, progscore_hash, 'Rds'), collapse='.')
  } else {
    progprob_file <- NULL
    progscore_file <- NULL
  }
  results <- tibble::lst(progprob_file, survprob_file, progscore_file, survscore_file)

  if (!is.null(survprob_fun)) {
    if (force || !file.exists(survprob_file)) {
      print("Starting predict-survprob")
      survprob <- purrr::lift_dl(survprob_fun, fit = fit, metadata = metadata, stan_data = stan_data)(survprob_args)
      saveRDS(survprob, survprob_file)
    } else {
      survprob <- readRDS(survprob_file)
    }
    #results <- c(results, tibble::lst(survprob))
    
    if (!is.null(bs_surv_fun)) {
      # prepare brier_score data summary for survival
      if ('result_var' %in% names(survprob_args)) {
        surv_result_var <- survprob_args$result_var
      } else {
        surv_result_var <- 'survprob'
      }
      if (force || !file.exists(survscore_file)) {
        print("Starting brier_score calcs for survprob")
        bs <- purrr::lift_dl(bs_surv_fun,
                           at = survprob_args$time,
                           fit = fit,
                           metadata = metadata,
                           stan_data = stan_data,
                           pp_prob = survprob,
                           pp_result_var = surv_result_var)(bs_surv_args) %>%
        dplyr::mutate(model = description)
        saveRDS(bs, file = survscore_file)
      } else {
        bs <- readRDS(survscore_file)
      }
      results$bs <- bs
    }
  }
  
  if (!is.null(progprob_fun)) {
    if (force || !file.exists(progprob_file)) {
      print("Starting predict-progprob")
      progprob <- purrr::lift_dl(progprob_fun, fit = fit, metadata = metadata, stan_data = stan_data)(progprob_args)
      saveRDS(progprob, progprob_file)
    } else {
      progprob <- readRDS(progprob_file)
    }
    #results <- c(results, tibble::lst(progprob))
    
    if (!is.null(bs_prog_fun)) {
      # compute brier_score data summary for progression
      if ('result_var' %in% names(progprob_args)) {
        prog_result_var <- progprob_args$result_var
      } else {
        prog_result_var <- 'recist_pd'
      }
      if (!'invert' %in% names(progprob_args)) {
        stop2('Progprob results need to be inverted for brier_score!')
      }
      if (force || !file.exists(progscore_file)) {
        print("Starting brier_score calcs for progprob")
        bs_pd <- purrr::lift_dl(bs_prog_fun,
                                at = progprob_args$time,
                                fit = fit,
                                metadata = metadata,
                                stan_data = stan_data,
                                pp_prob = progprob,
                                pp_result_var = prog_result_var)(bs_prog_args) %>%
          dplyr::mutate(model = description)
        saveRDS(bs_pd, file = progscore_file)
      } else {
        bs_pd <- readRDS(progscore_file)
      }
      results$bs_pd <- bs_pd
    } # end if (!is.null(bs_prog_fun))
  } # end if (!is.null(progprob_fun))
  results
}

