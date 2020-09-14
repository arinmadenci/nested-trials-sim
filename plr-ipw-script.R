# run this code for pooled logistic regression example
if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(here)

source("plr-ipw-function.R")
load(here::here("expanded-dat.Rda"))

ipw_formulas <- list()
ipw_formulas$formula.treatment <- "group.binary ~ 
                                gender + 
                                ns(systolic_bl, knots=quantile(systolic_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(systolic_bl, probs=c(0.1, 0.9))) + 
                                ns(t_bl, knots=quantile(t_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(t_bl, probs=c(0.1, 0.9))) +
                                ns(calendar.time_bl, knots=quantile(calendar.time_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(calendar.time_bl, probs=c(0.1, 0.9)))" # sample treatment regression

ipw_formulas$formula.outcome <- "mortality ~ 
                                group.binary*ns(t.new, knots=quantile(t.new, probs=c(0.35, 0.65)), Boundary.knots=quantile(t.new, probs=c(0.1, 0.9)))" # sample outcome regression

plr_ipw_function(dat=expanded.dat, followupdays=30, 
                 formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)
