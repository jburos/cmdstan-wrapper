install.packages('devtools')

# patch for rstan
# per https://github.com/stan-dev/rstan/issues/447
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
#cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION", 
#    file = M, sep = "\n", append = TRUE)

devtools::install_github('igraph/rigraph')

install.packages('rstan')
install.packages('doParallel')
install.packages('tidyverse')
install.packages('bayesplot')
install.packages('deSolve')
install.packages('shinystan')
install.packages('docopt')
install.packages('kimisc')
install.packages('lme4')
install.packages('simsurv')
install.packages('splines2')
install.packages('loo')
install.packages('rstanarm')
install.packages('rlist')
install.packages('formattable')
install.packages('kableExtra')
install.packages('caret')

