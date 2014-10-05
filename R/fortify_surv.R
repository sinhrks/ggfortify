#' Convert \code{survival::survfit} to data.frame.
#' 
#' @param survfit.data \code{survival::survfit} instance
#' @return data.frame
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(d.survfit)
#' @export
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

#' Autoplot \code{survival::survfit}.
#' 
#' @param survfit.data \code{survival::survfit} instance
#' @return data.frame
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(d.survfit)
#' @export
autoplot.survfit <- function(survfit.data) {
  plot.data <- ggplot2::fortify(survfit.data)
  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = time)) +
    ggplot2::geom_line(mapping = aes(y = surv, colour = strata)) +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = strata),
                         alpha = 0.5) +
    ggplot2::geom_point(data = plot.data[plot.data$n.censor > 0, ],
                        ggplot2::aes(y = surv), shape = '+', size = 3) + 
    ggplot2::scale_y_continuous(labels = scales::percent)
}