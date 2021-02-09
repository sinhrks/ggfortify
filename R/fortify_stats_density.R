#' Convert \code{stats::density} to \code{data.frame}
#'
#' @param model \code{stats::density} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(stats::density(stats::rnorm(1:50)))
#' @export
fortify.density <- function(model, data = NULL, ...) {
  d <- data.frame(x = model$x, y = model$y)
  post_fortify(d)
}

#' Autoplot \code{stats::density}
#'
#' @param object \code{stats::density} instance
#' @param p \code{ggplot2::ggplot} instance to plot
#' @param colour Line colour
#' @param linetype Line type
#' @param fill Fill colour
#' @param alpha Alpha
#' @inheritParams post_autoplot
#' @param ... other arguments passed to PDC/CDF func
#' @return ggplot
#' @examples
#' autoplot(stats::density(stats::rnorm(1:50)))
#' autoplot(stats::density(stats::rnorm(1:50)), fill = 'blue')
#' @importFrom scales percent
#' @export
autoplot.density <- function (object, p = NULL,
                              colour = '#000000', linetype = NULL,
                              fill = NULL, alpha = NULL,
                              xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                              main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                              ...)  {
  if (!is.data.frame(object)) {
    object <- ggplot2::fortify(object)
    object$ymin <- rep(0, nrow(object))
  }
  mapping <- ggplot2::aes_string(x = 'x', y = 'y', ymin = 'ymin', ymax = 'y')
  if (is.null(p)) {
    if (is.null(xlab)) {
      xlab = ''
    }
    if (is.null(ylab)) {
      ylab = ''
    }
    p <- ggplot2::ggplot(mapping = mapping) +
      ggplot2::scale_x_continuous(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab)
  }
  if (!is.null(fill)) {

    if (is.null(alpha)) {
      # specify default which should not affect to geom_line
      alpha <- 0.3
    }

    p <- p + geom_factory(geom_ribbon, object, colour = colour,
                          linetype = linetype, fill = fill, alpha = alpha)
  } else {
    # not to draw bottom line
    p <- p + geom_factory(geom_line, object, colour = colour,
                          linetype = linetype, alpha = alpha)
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
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
                            colour = '#000000', linetype = NULL,
                            fill = NULL, alpha = NULL, ...)  {
  data <- data.frame(x = x, y = func(x, ...),
                     ymin = rep(0, length(x)))
  p <- autoplot.density(data, p = p, colour = colour, linetype = linetype,
                        fill = fill, alpha = alpha, ...)
  p
}
