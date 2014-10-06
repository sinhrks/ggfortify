#' Convert \code{survival::survfit} to data.frame.
#' 
#' @param data \code{survival::survfit} instance
#' @return data.frame
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(d.survfit)
#' @export
fortify.survfit <- function(data) {
  data.frame(time = data$time,
             n.risk = data$n.risk,
             n.event = data$n.event,
             n.censor = data$n.censor,
             surv = data$surv,
             std.err = data$std.err,
             upper = data$upper,
             lower = data$lower,
             strata = rep(names(data$strata), data$strata))
}

#' Autoplot \code{survival::survfit}.
#' 
#' @param data \code{survival::survfit} instance
#' @return ggplot
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(d.survfit)
#' @export
autoplot.survfit <- function(data) {
  plot.data <- ggplot2::fortify(data)
  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = time)) +
    ggplot2::geom_line(mapping = aes(y = surv, colour = strata)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = strata),
                         alpha = 0.5) +
    ggplot2::geom_point(data = plot.data[plot.data$n.censor > 0, ],
                        ggplot2::aes(y = surv), shape = '+', size = 3) + 
    ggplot2::scale_y_continuous(labels = scales::percent)
}

#' Convert \code{survival::survfit.cox} to data.frame.
#' 
#' @param data \code{survival::survfit.cox} instance
#' @return data.frame
#' @examples
#' d.coxph <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(survival::survfit(d.coxph))
#' @export
fortify.survfit.cox <- function(data) {
  data.frame(time = data$time,
             n.risk = data$n.risk,
             n.event = data$n.event,
             n.censor = data$n.censor,
             surv = data$surv,
             cumhaz = data$cumhaz,
             std.err = data$std.err,
             upper = data$upper,
             lower = data$lower)
}