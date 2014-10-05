fortify.survfit <- function(survfit.data) {
  data.frame(time = survfit.data$time,
             n.risk = survfit.data$n.risk,
             n.event = survfit.data$n.event,
             n.censor = survfit.data$n.censor,
             surv = survfit.data$surv,
             std.err = survfit.data$std.err,
             upper = survfit.data$upper,
             lower = survfit.data$lower,
             strata = rep(names(survfit.data$strata), survfit.data$strata))
}