# notes: assumes no censoring process (apart from truncation at administrative end of follow-up; can be adapted for censoring)

plr_ipw_function <- function(dat, formula.treatment, formula.treatment.numerator=NULL, formula.outcome, followupdays){
  options(warn=2)
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(tidyverse, splines); p_install(survival, force=FALSE)
  
  if(!("Freq" %in% names(dat))){dat$Freq <- 1} #Freq column initialized (1 if non-bootstrap)
  
  assertthat::are_equal(sum(all.vars(as.formula(formula.treatment.numerator)) %in% all.vars(as.formula(formula.outcome))),
                        length(all.vars(as.formula(formula.treatment.numerator)) %in% all.vars(as.formula(formula.outcome)))) # check that variables included in numerator for stabilization are also included in outcome regression
  
  dat <- dat %>% select("id", "intervention", "intervention_lag", "Freq", all.vars(stats::formula(formula.outcome)), 
                        all.vars(stats::formula(formula.treatment.numerator)), all.vars(stats::formula(formula.treatment))) %>% 
    filter(t_eligible < (followupdays + 4)) %>% # truncate an arbitrary duration after desired follow-up time
    {.[complete.cases(.),]} # restricts to individuals with observed values for all variables in the model formula (for this example) - should really be done in pre-processing step
  print(length(unique(dat$id))) # check: number of individuals after restriction (compare to number of individuals prior to restriction)
  
  t_d <- glm(data=dat, 
             family="quasibinomial",
             weights = Freq,
             formula=as.formula(formula.treatment)) # this is the model fit to predict probability of treatment received
  dat$t_d_pred <- predict(object=t_d, newdata=dat, type="response")
  if (!is.null(formula.treatment.numerator)){
  t_n <- glm(data=dat,
             family="quasibinomial",
             weights = Freq,
             formula=as.formula(formula.treatment.numerator)) # this is the model fit to stabilize weights, if desired
  dat$t_n_pred <- predict(object=t_n, newdata=dat, type="response")
  } else{dat$t_n_pred <-dat$intervention} # works because of the way next line is structured
  
  dat <- dat %>% mutate(w = case_when(intervention == 1 ~ t_n_pred / t_d_pred,
                                      intervention == 0 ~ (1-t_n_pred) / (1-t_d_pred),
                                      TRUE ~ NA_real_))
  
  print(c("mean"=mean(dat$w), 
      quantile(dat$w, probs=c(0.99, 0.999))))
  
  # non-parametric risk (could also fit outcome regression)
  m <- survival::survfit(survival::Surv(dat$t_outcome, dat$outcome) ~ intervention, data=dat) #unadjusted
  m.weighted <- survival::survfit(survival::Surv(dat$t_outcome, dat$outcome) ~ intervention, data=dat, weights=dat$w)# ip weighted
  # survminer::ggsurvplot(
  #   fit = m,
  #   xlab = "Days",
  #   ylab = "Overall survival probability", ylim=c(0.5,1))
  # survminer::ggsurvplot(
  #   fit = m.weighted,
  #   xlab = "Days",
  #   ylab = "Overall survival probability", ylim=c(0.5,1))
  
  results <- summary(m.weighted, times=0:followupdays, extend=TRUE)
  results_eof <- summary(m.weighted, times=followupdays, extend=TRUE)
  
  output <- list()
  output$surv <- data.frame(survival=results$surv,
                       group=results$strata)
  output$risk_eof <- data.frame(survival=results_eof$surv,
                                group=results_eof$strata,
                                time=followupdays)
  
  return(output)
}

