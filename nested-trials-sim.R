# setup simulated dataset
if (!require("pacman")) install.packages("pacman"); library(pacman); p_load(tidyverse, here)
n=1000 # simulating dataset with 500 individuals
duration_followup_days = 50
dat <- data.frame("id"=rep(1:n, each=duration_followup_days), # say we had full 90 day follow-up available for each individual in this example 
                  "calendar.time"=rep(sample(1:365, n, replace=TRUE), each=duration_followup_days), # simulates days passed since the first possible enrollment
                  # (note that even if you are ultimately interested in 30-day follow-up as you mentioned, 
                  # you will want to have collected more than that initially because the nested trials start at different points in time;
                  # for example, change "duration_followup_days" to 30 and run the code to see instability in tails, especially with IP weighting)
                  "t"=rep(1:duration_followup_days, times=n),
                  "gender"=rep(rbinom(n=n, size=1, prob=0.5), each=duration_followup_days), # example of time-fixed baseline covariate
                  "systolic"=rnorm(n=n*duration_followup_days, mean=120, sd=10), # example of covariate that varies over time
                  "icu"=rbinom(n=n*duration_followup_days, size=1, prob=0.05), # using as an example where individuals are not eligible if currently in the ICU, but would again become eligible after transfer out of the ICU (did not try to simulate length of ICU stay)
                  "mortality"=rbinom(n=n*duration_followup_days, size=1, prob=0.005),
                  "intervention"=rbinom(n=n*duration_followup_days, size=1, prob=0.02)
                  ) %>% 
  mutate_at(vars(mortality, intervention), list(~ ifelse(.==1,1,NA))) %>% 
              group_by(id) %>% fill(mortality, intervention, .direction="down") %>% mutate("mortality_temp" = ifelse(mortality==1 & lag(mortality)==1, 1, NA)) %>% 
  mutate(intervention_lag = ifelse(row_number()==1,0,lag(intervention)),
         calendar.time = calendar.time + t - 1) %>% 
  ungroup() %>% 
  mutate_at(vars(mortality, intervention, intervention_lag), list(~ifelse(is.na(.),0,.))) %>% 
  filter(is.na(mortality_temp)) %>% select(-contains("temp")) ## clumsy way to create a simulated dataset covariates


# expand trials
new.dat <- dat %>% mutate(eligible = ifelse(icu==0,1,0)) %>% # example of defining eligibility criteria (in this case just based on not being in ICU), prior to expanding out the trials (but without restricting to eligible individuals only *yet*)
  group_by(id) %>% mutate(clone = row_number()) %>% ungroup() %>% # in this case, "clone" is identical to "t"
  mutate(id_clone = paste0(id,"_",clone)) # temporarily creates a new id defined by "id" and "clone" number

  # observed intervention: plasma
  # using all trials where plasma was administered (presuming this is the minority of trials)
new.dat_plasma <- pbapply::pblapply(new.dat %>% filter(intervention==1 & intervention_lag==0 & eligible==1) %>% .$id_clone,
                                 function(x) {
                                   x.id = new.dat$id[new.dat$id_clone==x]
                                   x.clone = new.dat$clone[new.dat$id_clone==x]
                                   new.dat %>% filter(id == x.id & clone >= x.clone) %>% # includes all person-time after the onset of trials in which plasma was observed to be the intervention
                                     mutate(id.new=x)
                                 }) %>% bind_rows()
  
  # observed intervention: no plasma
n.unexposed = 200; set.seed(1) # if you want to only sample this a certain number of unexposed trials, to avoid a dataset that would otherwise be undesirably large without much gain in precision
# n.unexposed = nrow(new.dat %>% filter(intervention==0 & intervention_lag==0 & eligible==1)) 
# uncomment the line above if not wanting to sample from unexposed trials (i.e., if you want to use *all* trials and can accomodate a large dataset)
temp.no.plasma.ids <- new.dat %>% filter(intervention==0 & intervention_lag==0 & eligible==1) %>% .$id_clone %>% sample(., size=n.unexposed, replace = FALSE) # samples from a list of eligible trial ids
new.dat_no.plasma <- pbapply::pblapply(temp.no.plasma.ids,
                                function(x) {
                                  x.id = new.dat$id[new.dat$id_clone==x]
                                  x.clone = new.dat$clone[new.dat$id_clone==x]
                                  new.dat %>% filter(id == x.id & clone >= x.clone) %>% # same as above
                                    mutate(id.new=x)
                                }) %>% bind_rows()

expanded.dat <- bind_rows(new.dat_plasma %>% mutate(group = "plasma", group.binary=1), 
                          new.dat_no.plasma %>% mutate(group = "no.plasma", group.binary=0)) %>% # combines the plasma and no.plasma trials
  arrange(id.new, t) %>% 
  group_by(id.new) %>% 
  mutate(t.new = t - min(t)) %>% # re-defines follow-up time to begin at the onset of each trial
  mutate_at(vars(t, calendar.time, systolic), list(bl = ~ first(.))) %>% # covariates indicating which calendar day, hospital day each trial was started on and baseline systolic BP
  ungroup() %>% select(-c(id, id_clone, clone, t, systolic, calendar.time))
rm(dat, new.dat, new.dat_no.plasma, new.dat_plasma, n, n.unexposed, temp.no.plasma.ids)

save(expanded.dat, file=here::here("expanded-dat.Rda"))
