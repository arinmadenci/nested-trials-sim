# run this code for pooled logistic regression example
if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(here)

source("condensed-plr-ipw-function.R")
if(!exists("condensed.dat")){load(here::here("condensed-dat.Rda"))}

ipw_formulas <- list()
ipw_formulas$formula.treatment <- "intervention ~ 
                                gender + 
                                ns(systolic, knots=quantile(systolic, probs=c(0.35, 0.65)), Boundary.knots=quantile(systolic, probs=c(0.1, 0.9))) + 
                                ns(t_eligible, knots=c(10,30), Boundary.knots=c(5, 45)) +
                                ns(calendar.time, knots=c(90, 270), Boundary.knots=c(45, 320))" # sample treatment regression

ipw_formulas$formula.outcome <- "outcome ~ 
                                intervention*ns(t_outcome, knots=c(10,20), Boundary.knots=c(3,30))" # sample outcome regression

condensed_plr_ipw_function(dat=expanded.dat, followupdays=30, 
                 formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)
