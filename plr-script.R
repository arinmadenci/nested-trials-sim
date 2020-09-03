# run this code for pooled logistic regression example
if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(here)

source("plr-function.R")
load(here::here("expanded-dat.Rda"))
formula.outcome <- "mortality ~ 
                                group*ns(t.new, knots=quantile(t.new, probs=c(0.35, 0.65)), Boundary.knots=quantile(t.new, probs=c(0.1, 0.9))) + 
                                gender + icu + 
                                ns(systolic_bl, knots=quantile(systolic_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(systolic_bl, probs=c(0.1, 0.9))) + 
                                ns(t_bl, knots=quantile(t_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(t_bl, probs=c(0.1, 0.9)))" # sample outcome regression

plr_function(dat=expanded.dat, formula.outcome=formula.outcome, followupdays=30)
