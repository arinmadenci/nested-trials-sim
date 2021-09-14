# run this code for pooled logistic regression example
if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(here)

source("plr-ipw-function.R")
if(!exists("expanded.dat")){load(here::here("expanded-dat.Rda"))}

ipw_formulas <- list()
ipw_formulas$formula.treatment <- "group.binary ~ 
                                gender + 
                                ns(systolic_bl, knots=quantile(systolic_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(systolic_bl, probs=c(0.1, 0.9))) + 
                                ns(t_bl, knots=c(10,30), Boundary.knots=c(5, 45)) +
                                ns(calendar.time_bl, knots=c(90, 270), Boundary.knots=c(45, 320))" # sample treatment regression

ipw_formulas$formula.outcome <- "mortality ~ 
                                group.binary*ns(t.new, knots=c(10,20), Boundary.knots=c(3,30))" # sample outcome regression


