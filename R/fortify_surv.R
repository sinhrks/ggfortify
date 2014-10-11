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
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int Logical flag indicating whether to plot censors
#' @return ggplot
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(d.survfit)
#' ggplot2::autoplot(d.survfit, conf.int = FALSE, censor = FALSE)
#' @export
autoplot.survfit <- function(data, conf.int = TRUE, censor = TRUE) {
  plot.data <- ggplot2::fortify(data)
  
  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = time)) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  if ('strata' %in% names(plot.data)) {
    p <- p + ggplot2::geom_line(mapping = aes(y = surv, colour = strata))
    conf.mapping <- ggplot2::aes(ymin = lower, ymax = upper, fill = strata)
  } else {
    p <- p + ggplot2::geom_line(mapping = aes(y = surv))
    conf.mapping <- ggplot2::aes(ymin = lower, ymax = upper)
  }
  
  if (conf.int) {
    p <- p + ggplot2::geom_ribbon(mapping = conf.mapping, alpha = 0.5)
  }
  if (censor) {
    p <- p + ggplot2::geom_point(data = plot.data[plot.data$n.censor > 0, ],
                                 ggplot2::aes(y = surv), shape = '+', size = 3)
  }
  p
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

autoplot.survfit.cox <- autoplot.survfit