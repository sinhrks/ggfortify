#' Attach confidence interval to \code{ggplot2::ggplot}
#' 
#' @param p \code{ggplot2::ggplot} instance
#' @param data Data contains lower and upper confidence intervals
#' @param lower Column name for lower confidence interval
#' @param upper Column name for upper confidence interval
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' d <- ggplot2::fortify(stats::acf(AirPassengers))
#' p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = lag))
#' ggfortify:::plot.conf.int(p)
plot.conf.int <- function (p, data = NULL, lower = 'lower', upper = 'upper', 
                           conf.int = TRUE,
                           conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                           conf.int.fill = '#000000', conf.int.alpha = 0.3) {
  
  if (conf.int) {
    mapping_ribbon = ggplot2::aes_string(ymin = lower, ymax = upper)
    mapping_lower = ggplot2::aes_string(y = lower)
    mapping_upper = ggplot2::aes_string(y = upper)
    if (!is.null(conf.int.fill)) {
      p <- p + ggplot2::geom_ribbon(data = data, 
                                    mapping = mapping_ribbon,
                                    fill = conf.int.fill, alpha = conf.int.alpha)
    }
    if (conf.int.linetype != 'none') {
      p <- p + ggplot2::geom_line(data = data,
                                  mapping = mapping_lower, 
                                  colour = conf.int.colour, linetype = conf.int.linetype,
                                  na.rm = TRUE) +
        ggplot2::geom_line(data = data,
                           mapping = mapping_upper, 
                           colour = conf.int.colour, linetype = conf.int.linetype,
                           na.rm = TRUE)   
    }
  }
  p
}

#' Plot distribution
#' 
#' @param func PDF or CDF function
#' @param x Numeric vector to be passed to func
#' @param p \code{ggplot2::ggplot} instance to plot
#' @param colour Line colour
#' @param linetype Line type
#' @param fill Fill colour
#' @param alpha Alpha
#' @param ... Keywords passed to PDC/CDF func
#' @return ggplot
#' @examples
#' ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
#' ggdistribution(ppois, seq(0, 30), lambda = 20)
#' 
#' p <- ggdistribution(pchisq, 0:20, df = 7, fill = 'blue')
#' ggdistribution(pchisq, 0:20, p = p, df = 9, fill = 'red')
#' @export
ggdistribution <- function (func, x, p = NULL, 
                            colour = '#000000', linetype = 'solid',
                            fill = NULL, alpha = 0.3, ...)  {
  d <- data.frame(x = x, y = func(x, ...),
                  ymin = rep(0, length(x)))
  mapping <- ggplot2::aes_string(x = 'x', y = 'y', ymin = 'ymin', ymax = 'y')
  if (is.null(p)) {
    p <- ggplot2::ggplot() + 
      ggplot2::scale_x_continuous(name = '') + 
      ggplot2::scale_y_continuous(name = '', labels = scales::percent) 
  }
  if (!is.null(fill)) {
    p <- p + ggplot2::geom_ribbon(data = d, mapping = mapping,
                                  colour = colour, linetype = linetype,
                                  fill = fill, alpha = alpha)
  } else {
    p <- p + ggplot2::geom_line(data = d, mapping = mapping,
                                colour = colour, linetype = linetype)
  }
  p
}