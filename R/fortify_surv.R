#' Convert \code{survival::survfit} to data.frame.
#' 
#' @param data \code{survival::survfit} instance
#' @return data.frame
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(d.survfit)
#' @export
fortify.survfit <- function(data) {
  d <- data.frame(time = data$time,
                  n.risk = data$n.risk,
                  n.event = data$n.event,
                  n.censor = data$n.censor,
                  surv = data$surv,
                  std.err = data$std.err,
                  upper = data$upper,
                  lower = data$lower,
                  strata = rep(names(data$strata), data$strata))
  dplyr::tbl_df(d)
}

#' Autoplot \code{survival::survfit}.
#' 
#' @param data \code{survival::survfit} instance
#' @param surv.colour Line colour for survival curve (used when data doesn't have \code{strata})
#' @param surv.linetype Line type for survival curve
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param censor Logical flag indicating whether to plot censors
#' @param censor.shape Shape for censors
#' @param censor.size Size for censors
#' @return ggplot
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(d.survfit)
#' ggplot2::autoplot(d.survfit, conf.int = FALSE, censor = FALSE)
#' @export
autoplot.survfit <- function(data, 
                             surv.colour = '#000000', surv.linetype = 'solid',
                             conf.int = TRUE,
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             censor = TRUE,
                             censor.shape = '+', censor.size = 3) {
  plot.data <- ggplot2::fortify(data)
  
  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = time)) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  if ('strata' %in% names(plot.data)) {
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(y = surv, colour = strata),
                                linetype = surv.linetype)
    conf.mapping <- ggplot2::aes(ymin = lower, ymax = upper, fill = strata)
    if (conf.int) {
      p <- p + ggplot2::geom_ribbon(mapping = conf.mapping, alpha = conf.int.alpha)
    }
  } else {
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(y = surv),
                                colour = surv.colour, linetype = surv.linetype)
    conf.mapping <- ggplot2::aes(ymin = lower, ymax = upper)
    if (conf.int) {
      p <- p + ggplot2::geom_ribbon(mapping = conf.mapping,
                                    fill = conf.int.fill, alpha = conf.int.alpha)
    }
  }

  if (censor) {
    p <- p + ggplot2::geom_point(data = plot.data[plot.data$n.censor > 0, ],
                                 ggplot2::aes(y = surv),
                                 shape = censor.shape, size = censor.size)
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
  d <- data.frame(time = data$time,
                  n.risk = data$n.risk,
                  n.event = data$n.event,
                  n.censor = data$n.censor,
                  surv = data$surv,
                  cumhaz = data$cumhaz,
                  std.err = data$std.err,
                  upper = data$upper,
                  lower = data$lower)
  dplyr::tbl_df(d)
}

autoplot.survfit.cox <- autoplot.survfit