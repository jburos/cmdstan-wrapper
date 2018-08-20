
local({
  Sys.setenv(TZ="America/New_York")
	options(repos = c(CRAN = "https://cran.rstudio.org/"))

	find_root_dir <- function(path, pattern='az-tumorsize') {
	    if (grepl(pattern, basename(path)) == TRUE) {
		return(path)
	    } else {
		return(find_root_dir(dirname(path)))
	    }
	}

	#' Determine if we are "in" a conda virtualenv
	#' Tests whether the current R executable is within that virtualenv path
	#' @returns boolean
	using_conda <- function() {
	  # test if any conda env is loaded
	  default_env_name <- Sys.getenv('CONDA_DEFAULT_ENV')
	  if (default_env_name == '') {
	    return(FALSE)
	  }
	  # test if path to R is within condaenv path
	  env_path <- system("conda info | grep 'default environment' | awk '{print $4}'", intern=T)
	  path_to_R <- system('which R', intern=T)
	  grepl(path_to_R, pattern=env_path, fixed=T)
	}

	# get ROOT_DIR (useful for lots of things other than libpath
	if (Sys.getenv('R_PROFILE_USER') != '') {
	    ROOT_DIR = dirname(normalizePath(Sys.getenv('R_PROFILE_USER')))
	} else if (suppressMessages(suppressWarnings(require('here')))) {
	    ROOT_DIR = here::here()
	} else {
	    ROOT_DIR = normalizePath(find_root_dir(getwd()))
	}

	# set libpath, unless we're in a conda-managed version of R 
	# in which case, we want to default to conda libpath
	if (using_conda() == TRUE) {
	  .First <- function(){cat('Operating in conda env:', Sys.getenv('CONDA_DEFAULT_ENV'),  "\n")}
	} else {
	  ## local library for project-specific packages
	  dir.create(file.path(ROOT_DIR, 'Rlib2'), showWarnings = FALSE, recursive = TRUE)
	  .libPaths(new = file.path(ROOT_DIR, 'Rlib2'))
	  .First <- function(){cat('ROOT_DIR set to', ROOT_DIR, "\n")}
	}
})
