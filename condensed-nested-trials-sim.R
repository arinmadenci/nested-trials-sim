if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(tidyverse, here, pbapply, data.table)
n=1000 # simulating dataset with 1000 individuals
duration_followup_days = 120
LOS.eligibility = 30 # indiviual with length of stay less than this number will be considered eligible
condensed.dat <- data.frame("id"=rep(1:n, each=duration_followup_days), # say we had full follow-up of this length available for each individual in this example 
                  "calendar.time"=rep(sample(1:365, n, replace=TRUE), each=duration_followup_days), # simulates days elapsed since the first possible enrollment
                  # (note that even if you are ultimately interested in 30-day follow-up as you mentioned, 
                  # you will want to have collected more than that initially because the nested trials start at different points in time;
                  # for example, change "duration_followup_days" to 30 and run the code to see instability in tails, especially with IP weighting)
                  "t_eligible"=rep(1:duration_followup_days, times=n), # time since first eligibility
                  "gender"=rep(rbinom(n=n, size=1, prob=0.5), each=duration_followup_days), # example of time-fixed baseline covariate
                  "systolic"=rnorm(n=n*duration_followup_days, mean=120, sd=10), # example of covariate that varies over time
                  "icu"=rbinom(n=n*duration_followup_days, size=1, prob=0.05), # (not implemented) could be considered ineligible if currently in the ICU, but would again become eligible after transfer out of the ICU (did not try to simulate length of ICU stay)
                  "mortality"=rbinom(n=n*duration_followup_days, size=1, prob=0.005), # outcome
                  "intervention"=rbinom(n=n*duration_followup_days, size=1, prob=0.02)
) %>% 
  mutate(intervention_lag = ifelse(t_eligible==1,0,lag(intervention)),
         calendar.time=calendar.time + t_eligible -1,
         eligible = ifelse(icu==0,1,0), # example of defining eligibility criteria (in this case just based on not being in ICU), prior to expanding out the trials (but without restricting to eligible individuals only *yet*)
         eligible = ifelse(t_eligible < LOS.eligibility,eligible,0)) %>% 
  mutate_at(vars(mortality, intervention), list(~ ifelse(.==1,1,NA))) %>%  # not the most efficient way to do this (indexing will fail for large datasets), but using here just for the set-up
  group_by(id) %>% fill(mortality, intervention, .direction="down") %>% mutate("mortality_temp" = ifelse(mortality==1 & lag(mortality)==1, 1, NA)) %>%
  ungroup() %>% 
  mutate_at(vars(mortality, intervention, intervention_lag), list(~ifelse(is.na(.),0,.))) %>% 
  filter(is.na(mortality_temp)) %>% select(-contains("temp")) ## clumsy way to create a simulated dataset covariates

# processing
condensed.dat <- condensed.dat %>%  #excluded ineligible (due to already having had the intervention) person-rows
  group_by(id) %>% mutate(t_outcome = max(t_eligible) - t_eligible,
                          outcome = max(mortality)) %>% 
  ungroup() %>% filter(intervention_lag==0 & eligible==1)

#### kept above for simplicity so as to have the same starting point
save(condensed.dat, file=here::here("condensed-dat.Rda"))

