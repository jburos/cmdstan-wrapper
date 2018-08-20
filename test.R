source('stan_helpers.function.R')
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- fit_stan_model(file = "stan/eight_schools.stan",
                      data = schools_dat,
                      iter = 10, chains = 1)

