# notes: assumes no censoring process (apart from truncation at administrative end of follow-up; can be adapted for censoring)

plr_standardization_function <- function(dat, formula.outcome, followupdays){
  options(warn=2)
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(tidyverse, splines)
  dat <- dat %>% select("id.new", "group", all.vars(stats::formula(formula.outcome))) %>% 
    filter(t.new < (followupdays + 4)) %>% # truncate just after desired follow-up time
    {.[complete.cases(.),]} # restricts to individuals with observed values for all variables in the model formula
  print(length(unique(dat$id.new))) # check: number of individuals after restriction (compare to number of individuals prior to restriction)
  m <- glm(data=dat, 
           family="quasibinomial",
           formula=as.formula(formula.outcome)) # fit outcome regression for standardization to overall population
  print(m)
  
  dat.grid <- left_join(data.frame(t.new=rep(seq(1,followupdays, by=1), times=length(unique(dat$id.new))), # create grid of overall population baseline characteristics for risk prediction
                        id.new = rep(unique(dat$id.new), each=followupdays),
                        stringsAsFactors=FALSE),
                        dat %>% filter(t.new==0) %>% 
                          select(id.new, all.vars(stats::formula(formula.outcome)[-2])[all.vars(stats::formula(formula.outcome)[-2]) != "t.new"]),
  by="id.new")
  dat.grid$probs_1 <- predict(object=m, newdata=dat.grid %>% mutate(group="plasma"), type="response")
  dat.grid$probs_0 <- predict(object=m, newdata=dat.grid %>% mutate(group="no.plasma"), type="response")
  output <- dat.grid %>% 
    group_by(id.new) %>% arrange(t.new) %>% mutate(s_1 = cumprod(1-probs_1), s_0 = cumprod(1-probs_0)) %>% ungroup() %>% 
    group_by(t.new) %>% summarise(ci_1 = 1-mean(s_1), ci_0 = 1-mean(s_0)) %>% ungroup() %>% 
    select(ci_1, ci_0) %>% as.data.frame() %>% mutate(day=row_number()) # standardization
  output <- rbind(data.frame(ci_1=0, ci_0=0, day=0), # risk 0% at time zero
                  output)
  return(output)
}

