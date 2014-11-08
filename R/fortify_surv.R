#' Convert \code{survival::survfit} to data.frame.
#' 
#' @param data \code{survival::survfit} instance
#' @return data.frame
#' @aliases fortify.survfit.cox 
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(d.survfit)
#' 
#' d.coxph <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::fortify(survival::survfit(d.coxph))
#' @export
fortify.survfit <- function(data) {
  d <- data.frame(time = data$time,
                  n.risk = data$n.risk,
                  n.event = data$n.event,
                  n.censor = data$n.censor,
                  surv = data$surv,
                  std.err = data$std.err,
                  upper = data$upper,
                  lower = data$lower)
  
  if (is(data, 'survfit.cox')) {
    d <- cbind(d, data.frame(cumhaz = data$cumhaz))
  } else if (is(data, 'survfit')) {
    d <- cbind(d, data.frame(strata = rep(names(data$strata), data$strata)))
  } else {
    stop(paste0('Unsupported class for fortify.survfit: ', class(data)))
  }
  dplyr::tbl_df(d)
}

#' Autoplot \code{survival::survfit}.
#' 
#' @param data \code{survival::survfit} instance
#' @param surv.colour Line colour for survival curve (used when data doesn't have \code{strata})
#' @param surv.linetype Line type for survival curve
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param censor Logical flag indicating whether to plot censors
#' @param censor.shape Shape for censors
#' @param censor.size Size for censors
#' @return ggplot
#' @aliases autoplot.survfit.cox
#' @examples
#' d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(d.survfit)
#' ggplot2::autoplot(d.survfit, conf.int = FALSE, censor = FALSE)
#' 
#' d.coxph <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
#' ggplot2::autoplot(survival::survfit(d.coxph))
#' @export
autoplot.survfit <- function(data, 
                             surv.colour = '#000000', surv.linetype = 'solid',
                             conf.int = TRUE,
                             conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             censor = TRUE,
                             censor.shape = '+', censor.size = 3) {
  plot.data <- ggplot2::fortify(data)
  
  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes_string(x = 'time')) +
    ggplot2::scale_y_continuous(labels = scales::percent)
  if ('strata' %in% names(plot.data)) {
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes_string(y = 'surv', colour = 'strata'),
                                linetype = surv.linetype)
    conf.mapping <- ggplot2::aes_string(ymin = 'lower', ymax = 'upper', fill = 'strata')
    if (conf.int) {
      p <- p + ggplot2::geom_ribbon(mapping = conf.mapping, alpha = conf.int.alpha)
      if (conf.int.linetype != 'none') {
        p <- p + ggplot2::geom_line(mapping = ggplot2::aes_string(y = 'lower', colour = 'strata'),
                                    linetype = conf.int.linetype) +
          ggplot2::geom_line(mapping = ggplot2::aes_string(y = 'upper', colour = 'strata'),
                             linetype = conf.int.linetype)   
      }
    }
  } else {
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes_string(y = 'surv'),
                                colour = surv.colour, linetype = surv.linetype)
    conf.mapping <- ggplot2::aes_string(ymin = 'lower', ymax = 'upper')
    
    p <- plot.conf.int(p, conf.int = conf.int,
                       conf.int.colour = conf.int.colour,
                       conf.int.linetype = conf.int.linetype,
                       conf.int.fill = conf.int.fill,
                       conf.int.alpha = conf.int.alpha)
  }
  
  if (censor) {
    p <- p + ggplot2::geom_point(data = plot.data[plot.data$n.censor > 0, ],
                                 ggplot2::aes_string(y = 'surv'),
                                 shape = censor.shape, size = censor.size)
  }
  p
}


#' d.survreg <- survival::survreg(survival::Surv(time, status) ~ sex,
#'                                dist = 'weibull', data = survival::lung)
#' ggplot2::fortify(d.survreg)
#' 

# fortify.survreg <- function(data) {
#   predict(d.survreg, newdata = list(sex = 1), type = "quantile",
#           p = seq(.01, .99, by = .01))
# }