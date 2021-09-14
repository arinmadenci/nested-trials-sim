# empiric bootstrap processing example
# ref: https://ocw.mit.edu/courses/mathematics/18-05-introduction-to-probability-and-statistics-spring-2014/readings/MIT18_05S14_Reading24.pdf

empiric_pointest <- plr_ipw_function(dat=bs.dat, followupdays=30, 
                              formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)

xbar_1 <- empiric_pointest$ci_1
xbar_0 <- empiric_pointest$ci_0

resampled_pointests <- pbapply::pblapply(1:10, function(x){ # 10 resamples only, as illustration
  set.seed(x)
  bs.ids <- sample(expanded.dat$id.new, size=length(unique(expanded.dat$id.new))) %>% table %>% as.data.frame %>% distinct %>% rename(id.new = '.')
  bs.dat <- left_join(bs.ids, expanded.dat, by="id.new")
  plr_ipw_function(dat=bs.dat, followupdays=30, 
                   formula.treatment=ipw_formulas$formula.treatment, formula.treatment.numerator=NULL, formula.outcome=ipw_formulas$formula.outcome)
})

ci_0 <- resampled_pointests %>% as.data.frame %>% dplyr::select_at(vars(-starts_with("day."), -starts_with("ci_1")))
lower_0 <- apply(ci_0 %>% dplyr::select(-"day"), 1, quantile, probs=0.025) - xbar_0
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
      


