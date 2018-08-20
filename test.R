schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- try(fit_stan_model(model = "stan/8schools.stan",
                          data = schools_dat, 
                          iter = 10, chains = 1)
