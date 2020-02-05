library(survival)
library(survminer)
library(reshape2)

data(GBSG2, package = "TH.data")
data(UnempDur, package = "Ecdat")


# Create time and event data
time <- c(5, 6, 2, 4, 4)
event <- c(1, 0, 0, 1, 1)

# Compute Kaplan-Meier estimate
km <- survfit(Surv(GBSG2$time, GBSG2$cens) ~ 1)
km

# Take a look at the structure
str(km)

# Create data.frame
data.frame(time = km$time, n.risk = km$n.risk, n.event = km$n.event,
           n.censor = km$n.censor, surv = km$surv)

# Plot the survival curve
ggsurvplot(
  fit = km,
  data = GBSG2,
  palette = "blue", 
  linetype = 1, 
  surv.median.line = "hv", 
  risk.table = TRUE,
  cumevents = TRUE, 
  cumcensor = TRUE,
  tables.height = 0.1
)


## Importance of censoring
# Create dancedat data
dancedat <- data.frame(
  name = c("Chris", "Martin", "Conny", "Desi", "Reni", "Phil", 
           "Flo", "Andrea", "Isaac", "Dayra", "Caspar"),
  time = c(20, 2, 14, 22, 3, 7, 4, 15, 25, 17, 12),
  obs_end = c(1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0))

# Estimate the survivor function pretending that all censored observations are actual observations.
km_wrong <- survfit(Surv(time) ~ 1, data = dancedat)

# Estimate the survivor function from this dataset via kaplan-meier.
km <- survfit(Surv(time, obs_end) ~ 1, data = dancedat)

# Plot the two and compare
ggsurvplot_combine(list(correct = km, wrong = km_wrong))

## Modifying graphs

# Kaplan-Meier estimate
km <- survfit(Surv(time, cens) ~ 1, data = GBSG2)

# plot of the Kaplan-Meier estimate
ggsurvplot(km)

# add the risk table to plot
ggsurvplot(km, risk.table = TRUE)

# add a line showing the median survival time
ggsurvplot(km, risk.table = TRUE, surv.median.line = "hv")


# Weibull model
#' Smooth curve rather than stepped
#' like using a linear model instead of a histogram
#' assumes a Weibull distribution
#' uses survreg() insatead of survfit()

# Weibull estimate
wb <- survreg(Surv(time, cens) ~ 1, data = GBSG2)

# Predict the time point beyond whcich 90% of paitents survive
predict(wb, type = "quantile", p = 1 - 0.9, newdata = data.frame(1))

# Computing the survival curve for the Weibull model
surv <- seq(.99, .01, by = -.01)
t <- predict(wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))
head(data.frame(time = t, surv = surv))

# Visualising the Weibull model
# cannot use ggsurvplot() as for the Kaplan-Meier models

# Table for survival curve
surv_wb <- data.frame(time = t, surv = surv, 
                      upper = NA, lower = NA, std.err = NA)

# Plot
ggsurvplot_df(fit = surv_wb, surv.geom = geom_line) # geom_line for Weibull & geom_step for Kaplan-Meier

# Weibull model with mutliple predictors
wbmod <- survreg(Surv(time, cens) ~ horTh + tsize, data = GBSG2)

coef(wbmod) # model coefficents

# Retrieve survival curve from model for single group
surv <- seq(.99, .01, by = -.01)
t_yes <- predict(wbmod, type = "quantile", p = 1 - surv,
                 newdata = data.frame(horTh = "yes"))

# Plotting covariate
# Decide on covariate combinations
newdat <- expand.grid(
  horTh = levels(GBSG2$horTh),
  tsize = quantile(GBSG2$tsize, probs = c(0.25, 0.5, 0.75))
)

# Compute survival curves for all "imaginary patients"
t <- predict(wbmod, type = "quantile", p = 1 - surv, newdata = newdat)

dim(t)

t[,1:7] # data needs reshaping in order for us to plot it

# before melting we need to add the paitent informaiton
surv_wbmod_wide <- cbind(newdat, t)
surv_wbmod <- melt(surv_wbmod_wide, id.vars = c("horTh", "tsize"), 
                   variable.name = "surv_id",  value.name = "time")
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA # needed by the plotting function

# Plot

ggsurvplot_df(surv_wbmod, surv.geom = geom_line, # need this to be geom_step if using a KM model
              linetype = "horTh", color = "tsize", legend.title = NULL)

