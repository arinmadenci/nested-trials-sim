library(splines)
library("survival")
library("survminer")
data("lung")
d <- lung %>% filter(time<365*2) # using "lung" as an example dataset
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  d); summary(res.cox) # cox model for comparison

window=1 # two weeks as arbitrary discrete time unit
lung.expanded <- survSplit(Surv(time, status) ~ ., data = d %>% mutate(id=row_number()),
            cut=seq(from=0,to=365*2, by=window))

m <- glm(data=lung.expanded,
         status ~ ns(tstart, knots=quantile(tstart, c(0.275, 0.5, 0.725)), Boundary.knots = quantile(tstart, c(0.1, 0.9))) + 
           age + sex + ph.ecog, # for analysis would include time-varying hazards here; did not include time-varying hazards in this simularion, for more direct comparison to cox model above
         family="binomial"
)

grid.dat <- merge(d %>% mutate(id=row_number()),
                  data.frame(tstart=rep(seq(0,max(d$time), window), times=nrow(d)),
                             id=rep(1:nrow(d), each=max(d$time)/ (window) + 1)),
                  by="id"
)
grid.dat$preds_1 <- predict(m, newdata=grid.dat %>% mutate(sex=1), type="response") # discrete time hazards for sex=1
grid.dat$preds_2 <- predict(m, newdata=grid.dat %>% mutate(sex=2), type="response")

mean(grid.dat$preds_1/grid.dat$preds_2, na.rm=TRUE)
