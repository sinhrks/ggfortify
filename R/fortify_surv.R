#' Convert \code{survival::survfit} to \code{data.frame}
#'
#' @param model \code{survival::survfit} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @aliases fortify.survfit.cox
#' @examples
#' library(survival)
#' fortify(survfit(Surv(time, status) ~ sex, data = lung))
#' fortify(survfit(Surv(time, status) ~ 1, data = lung))
#' fortify(survfit(coxph(Surv(time, status) ~ sex, data = lung)))
#' fortify(survfit(coxph(Surv(time, status) ~ 1, data = lung)))
#' @export
fortify.survfit <- function(model, data = NULL, ...) {
  d <- data.frame(time = model$time,
                  n.risk = model$n.risk,
                  n.event = model$n.event,
                  n.censor = model$n.censor,
                  surv = model$surv,
                  std.err = model$std.err,
                  upper = model$upper,
                  lower = model$lower)

  if (is(model, 'survfit.cox')) {
    d <- cbind_wraps(d, data.frame(cumhaz = model$cumhaz))
  } else if (is(model, 'survfit')) {
    if ('strata' %in% names(model)) {
      d <- cbind_wraps(d, data.frame(strata = rep(names(model$strata), model$strata)))
    }
  } else {
    stop(paste0('Unsupported class for fortify.survfit: ', class(model)))
  }
  post_fortify(d)
}

#' Autoplot \code{survival::survfit}
#'
#' @param object \code{survival::survfit} instance
#' @param surv.geom geometric string for survival curve. 'line' or 'point'
#' @param surv.colour line colour for survival curve
#' @param surv.size point size for survival curve
#' @param surv.linetype line type for survival curve
#' @param surv.alpha alpha for survival curve
#' @param surv.fill fill colour survival curve
#' @param surv.shape point shape survival curve
#' @inheritParams plot_confint
#' @param censor Logical flag indicating whether to plot censors
#' @param censor.colour colour for censors
#' @param censor.size size for censors
#' @param censor.alpha alpha for censors
#' @param censor.shape shape for censors
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @aliases autoplot.survfit.cox
#' @examples
#' library(survival)
#' autoplot(survfit(Surv(time, status) ~ sex, data = lung))
#' autoplot(survfit(Surv(time, status) ~ 1, data = lung))
#' autoplot(survfit(Surv(time, status) ~ sex, data=lung), conf.int = FALSE, censor = FALSE)
#' autoplot(survfit(coxph(Surv(time, status) ~ sex, data = lung)))
#' @export
autoplot.survfit <- function(object,
                             surv.geom = 'line',
                             surv.colour = NULL, surv.size = NULL, surv.linetype = NULL,
                             surv.alpha = NULL, surv.fill = NULL, surv.shape = NULL,
                             conf.int = TRUE,
                             conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             censor = TRUE, censor.colour = NULL, censor.size = 3,
                             censor.alpha = NULL, censor.shape = '+',
                             xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                             main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                             ...) {

  plot.data <- ggplot2::fortify(object)

  if (is.null(surv.colour) & ('strata' %in% colnames(plot.data))) {
      surv.colour <- 'strata'
  }
  if (missing(conf.int.fill) & !is.null(surv.colour)) {
    conf.int.fill <- surv.colour
  }

  geomfunc <- get_geom_function(surv.geom, allowed = c('line', 'point'))

  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes_string(x = 'time', y = 'surv')) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  p <- p + geom_factory(geomfunc, plot.data,
                        colour = surv.colour, size = surv.size, linetype = surv.linetype,
                        alpha = surv.alpha, fill = surv.fill, shape = surv.shape)
  p <- plot_confint(p, data = plot.data, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  if (censor) {
    p <- p + geom_factory(geom_point, plot.data[plot.data$n.censor > 0, ],
                          colour = censor.colour, size = censor.size,
                          alpha = censor.alpha, shape = censor.shape)
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
