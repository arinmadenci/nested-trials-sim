# empiric bootstrap processing example
# ref: https://ocw.mit.edu/courses/mathematics/18-05-introduction-to-probability-and-statistics-spring-2014/readings/MIT18_05S14_Reading24.pdf

if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(here)

source("plr-ipw-function.R")
load(here::here("expanded-dat.Rda"))

ipw_formulas <- list()
ipw_formulas$formula.treatment <- "group.binary ~ 
                                gender + 
                                ns(systolic_bl, knots=quantile(systolic_bl, probs=c(0.35, 0.65)), Boundary.knots=quantile(systolic_bl, probs=c(0.1, 0.9))) + 
                                ns(t_bl, knots=c(10,30), Boundary.knots=c(5, 45)) +
                                ns(calendar.time_bl, knots=c(90, 270), Boundary.knots=c(45, 320))" # sample treatment regression

ipw_formulas$formula.outcome <- "mortality ~ 
                                group.binary*ns(t.new, knots=c(10,20), Boundary.knots=c(3,30))" # sample outcome regression

empiric_pointest <- plr_ipw_function(dat=expanded.dat, followupdays=30, 
                              formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)

xbar_1 <- empiric_pointest$ci_1
xbar_0 <- empiric_pointest$ci_0

resampled_pointests <- pbapply::pblapply(1:10, function(x){ # 10 resamples only, as illustration
  set.seed(x)
  bs.ids <- sample(unique(expanded.dat$id), size=length(unique(expanded.dat$id)), replace=TRUE) %>% table %>% as.data.frame %>% distinct %>% rename(id = '.') %>% 
    mutate(id=as.character(id)) %>% mutate(id=as.numeric(id)) #avoid issue with factor variable converting to level numbers
  bs.dat <- left_join(bs.ids, expanded.dat, by="id")
  plr_ipw_function(dat=bs.dat, followupdays=30, 
                   formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)
})

ci_0 <- resampled_pointests %>% as.data.frame %>% dplyr::select_at(vars(-starts_with("day."), -starts_with("ci_1"))) #cumulative incidence
lower_0 <- apply(ci_0 %>% dplyr::select(-"day"), 1, quantile, probs=0.025) - xbar_0 # note: monotonic function, so do not have to subtract xbar before taking quantile
upper_0 <- apply(ci_0 %>% dplyr::select(-"day"), 1, quantile, probs=0.975) - xbar_0

ci_1 <- resampled_pointests %>% as.data.frame %>% dplyr::select_at(vars(-starts_with("day."), -starts_with("ci_0")))
lower_1 <- apply(ci_1 %>% dplyr::select(-"day"), 1, quantile, probs=0.025) - xbar_1 
upper_1 <- apply(ci_1 %>% dplyr::select(-"day"), 1, quantile, probs=0.975) - xbar_1




result <- data.frame("day"=0:30, 
           "pointest_1"=xbar_1,
           "lcl_1"=xbar_1-upper_1,
           "ucl_1"=xbar_1-lower_1,
           "pointest_0"=xbar_0,
           "lcl_0"=xbar_0-upper_0,
           "ucl_0"=xbar_0-lower_0)

ggplot(data=result, 
       mapping=aes(x=day, y=pointest_1)) + 
  geom_point() + 
  geom_ribbon(aes(ymin = lcl_1, ymax=ucl_1), alpha=0.6)
      


