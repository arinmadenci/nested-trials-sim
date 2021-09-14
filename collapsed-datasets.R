# notes: assumes no censoring process (apart from truncation at administrative end of follow-up; can be adapted for censoring)

plr_ipw_function <- function(dat, formula.treatment, formula.treatment.numerator=NULL, formula.outcome, followupdays){
  options(warn=2)
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(tidyverse, splines)
  
  assertthat::are_equal(sum(all.vars(as.formula(formula.treatment.numerator)) %in% all.vars(as.formula(formula.outcome))),
                        length(all.vars(as.formula(formula.treatment.numerator)) %in% all.vars(as.formula(formula.outcome)))) # check that variables included in numerator for stabilization are also included in outcome regression
  
  dat <- dat %>% select("id.new", "group", all.vars(stats::formula(formula.outcome)), 
                        all.vars(stats::formula(formula.treatment.numerator)), all.vars(stats::formula(formula.treatment))) %>% 
    filter(t.new < (followupdays + 4)) %>% # truncate just after desired follow-up time
    {.[complete.cases(.),]} # restricts to individuals with observed values for all variables in the model formula
  print(length(unique(dat$id.new))) # check: number of individuals after restriction (compare to number of individuals prior to restriction)
  
  dat.collapsed <- setDT(dat[,temp:=1])[, .(Freq = .N), 
                                 by = c(all.vars(stats::formula(formula.treatment)))]
  
  t_d.collapsed <- glm(data=dat.collapsed %>% filter(t.new==0), # fit restricted to baseline, because point treatment `assigned` at baseline
             family="quasibinomial",
             weights=Freq,
             formula=as.formula(formula.treatment)) # this is the model fit to predict probability of treatment received
  
  t_d <- glm(data=dat %>% filter(t.new==0), # fit restricted to baseline, because point treatment `assigned` at baseline
             family="quasibinomial",
             formula=as.formula(formula.treatment)) # this is the model fit to predict probability of treatment received
  dat$t_d_pred <- predict(object=t_d, newdata=dat, type="response")
  if (!is.null(formula.treatment.numerator)){
    t_n <- glm(data=dat %>% filter(t.new==0),
               family="quasibinomial",
               formula=as.formula(formula.treatment.numerator)) # this is the model fit to stabilize weights, if desired
    dat$t_n_pred <- predict(object=t_n, newdata=dat, type="response")
  } else(dat$t_n_pred = dat$group.binary)
  
  dat <- dat %>% mutate(w = case_when(group.binary == 1 & t.new == 0 ~ t_n_pred / t_d_pred,
                                      group.binary == 0 & t.new == 0 ~ (1-t_n_pred) / (1-t_d_pred),
                                      TRUE ~ NA_real_)) %>% 
    group_by(id.new) %>% 
    fill(w, .direction = "down") %>% # equivalent to cumulative product of weights with all non-baseline treatment weights equal to 1
    ungroup()
  
  dat01 <- dat %>% mutate(w0 = case_when(group.binary == 1 & t.new > 0 ~ 0,
                                      group.binary == 0 & t.new > 0 ~ w, 
                                      TRUE~w),
                          w1 = case_when(group.binary == 1 & t.new > 0 ~ w,
                                         group.binary == 0 & t.new > 0 ~ 0,
                                         TRUE~w))
  
  dat01 <- dat %>% mutate(w1 = case_when(group.binary == 1 & t.new > 0 ~ w,
                                         group.binary == 0 ~ 0, 
                                         TRUE~w),
                          w0 = case_when(group.binary == 1 ~ 0,
                                         group.binary == 0 & t.new > 0 ~ w,
                                         TRUE~w))
  
  print(c("mean"=mean(dat$w), 
          quantile(dat$w, probs=c(0.99, 0.999))))
  
  dat.outcome <- dat01 %>% select(w0, w1, t.new, mortality)
  # 
  dat.new0 <- setDT(dat.outcome)[, .(Freq0 = sum(w0)),
                                 by = .(t.new, mortality)]
  dat.new1 <- setDT(dat.outcome)[, .(Freq1 = sum(w1)),
                                 by = .(t.new, mortality)]
  
  dat.new0[dat.new1, on=.(t.new, mortality)]
  dat.outcome[, .(Freq0 = sum(w0)),
               by = .(t.new, mortality)
              ][dat.outcome[, .(Freq1 = sum(w1)),
                            by = .(t.new, mortality)
              ], on=.(t.new, mortality)]
  
  
  
  cols=c("w0", "w1")
  dat.new.test <- setDT(dat.outcome)[, (cols) := lapply(.SD, function(z) sum(z),
                                 ), .SDcols = cols, on = .(t.new, mortality)]
  
  # dat.new <- setDT(dat.outcome)[, `:=` (Freq1 = sum(w1), Freq0 = sum(w0)), 
  #                                by = .(t.new, mortality)]
  # dat.new <- cbind(dat.outcome)[, Freq1 := sum(w1), by = .(t.new, mortality)][, Freq0 := sum(w0), by = .(t.new, mortality)]
  dat.new0[, Freq1 := dat.outcome[,  .(Freq1 = sum(w1)),
                                  by = .(t.new, mortality)]
           ]
  
  # df <- setDT(dat.outcome)
  # df_freq_table <- df[,.(Freq = .N), by = t.new
  # ][, Prop := Freq / sum(Freq)
  # ][, Cum := cumsum(100 * Prop / sum(Prop))]

  m.new0 <- glm(data=dat.new0, 
           family="quasibinomial",
           formula=mortality ~ ns(t.new, knots = c(10, 20), Boundary.knots = c(3, 30)),
           weights = Freq) # fit weighted outcome regression
  m.new1 <- glm(data=dat.new1, 
                family="quasibinomial",
                formula=mortality ~ ns(t.new, knots = c(10, 20), Boundary.knots = c(3, 30)),
                weights = Freq) # fit weighted outcome regression
  
  m <- glm(data=dat, 
           family="quasibinomial",
           formula=as.formula(formula.outcome),
           weights = w) # fit weighted outcome regression
  m0 <- glm(data=dat01, 
           family="quasibinomial",
           formula=mortality ~ ns(t.new, knots = c(10, 20), Boundary.knots = c(3, 30)),
           weights = w0) # fit weighted outcome regression
  m1 <- glm(data=dat01, 
           family="quasibinomial",
           formula=mortality ~ ns(t.new, knots = c(10, 20), Boundary.knots = c(3, 30)),
           weights = w1) # fit weighted outcome regression
  print(m)
  
  m.unadj <- glm(data=dat, 
                 family="quasibinomial",
                 formula=as.formula(formula.outcome)) # fit unadjusted outcome regression
  
  dat.grid <- (data.frame(t.new=rep(seq(1,followupdays, by=1)), # create grid (including time fixed variables if stabilizing weights) for risk prediction
                                   stringsAsFactors=FALSE))
  dat.grid$probs_1 <- predict(object=m, newdata=dat.grid %>% mutate(group.binary=1), type="response")
  dat.grid$probs_1.new <- predict(object=m1, newdata=dat.grid, type="response")
  dat.grid$probs_0 <- predict(object=m, newdata=dat.grid %>% mutate(group.binary=0), type="response")
  dat.grid$probs_0.new <- predict(object=m0, newdata=dat.grid, type="response")
  output <- dat.grid %>% 
    group_by(id.new) %>% arrange(t.new) %>% mutate(s_1 = cumprod(1-probs_1), s_0 = cumprod(1-probs_0)) %>% ungroup() %>% 
    group_by(t.new) %>% summarise(ci_1 = 1-mean(s_1), ci_0 = 1-mean(s_0)) %>% ungroup() %>% 
    select(ci_1, ci_0) %>% as.data.frame() %>% mutate(day=row_number()) 
  output <- rbind(data.frame(ci_1=0, ci_0=0, day=0), # risk 0% at time zero
                  output)
  return(output)
}

