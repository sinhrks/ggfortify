#' @export
fortify.stl <- fortify.ts

#' @export
fortify.decomposed.ts <- fortify.ts

#' @export
autoplot.stl <- autoplot.ts

#' @export
autoplot.decomposed.ts <- autoplot.ts

#' Convert \code{stats::acf} to \code{data.frame}
#'
#' @param model \code{stats::acf} instance
#' @inheritParams fortify_base
#' @param conf.int Logical flag indicating whether to attach confidence intervals
#' @param conf.int.value Coverage probability for confidence interval
#' @param conf.int.type Type of confidence interval, 'white' for white noise or 'ma' MA(k-1) model
#' @return data.frame
#' @examples
#' fortify(stats::acf(AirPassengers))
#' fortify(stats::pacf(AirPassengers))
#' fortify(stats::ccf(AirPassengers, AirPassengers))
#'
#' fortify(stats::acf(AirPassengers), conf.int = TRUE)
#' @export
fortify.acf <- function(model, data = NULL,
                        conf.int = TRUE, conf.int.value = 0.95,
                        conf.int.type = 'white', ...) {
  d <- data.frame(Lag = model$lag, ACF = model$acf)
  if (conf.int) {
    cf <- confint.acf(model, ci = conf.int.value, ci.type = conf.int.type)
    cfd <- data.frame(lower = -cf, upper = cf)
    d <- cbind(d, cfd)
  }
  post_fortify(d)
}

#' Autoplot \code{stats::acf}. Note to pass `plot = FALSE` to original function to suppress
#' standard plot output
#'
#' @param object \code{stats::acf} instance
#' @param colour Line colour
#' @param linetype Line type
#' @inheritParams plot_confint
#' @param conf.int.value Coverage probability for confidence interval
#' @param conf.int.type Type of confidence interval, 'white' for white noise or 'ma' MA(k-1) model
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(stats::acf(AirPassengers, plot = FALSE))
#' autoplot(stats::pacf(AirPassengers, plot = FALSE))
#' autoplot(stats::ccf(AirPassengers, AirPassengers, plot = FALSE))
#' @export
autoplot.acf <- function(object,
                         colour = '#000000', linetype = 'solid',
                         conf.int = TRUE,
                         conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                         conf.int.fill = NULL, conf.int.alpha = 0.3,
                         conf.int.value = 0.95, conf.int.type = 'white',
                         xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                         main = NULL, xlab = NULL, ylab = 'ACF', asp = NULL,
                         ...) {
  plot.data <- ggplot2::fortify(object, conf.int = conf.int,
                                conf.int.value = conf.int.value,
                                conf.int.type = conf.int.type)

  # Prepare ymax and ymin used for geom_linerange
  plot.data <- dplyr::mutate(plot.data,
                              ymax = ifelse(ACF > 0, ACF, 0),
                              ymin = ifelse(ACF < 0, ACF, 0))

  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_linerange(mapping = ggplot2::aes_string(ymin = 'ymin', ymax = 'ymax'),
                            colour = colour, linetype = linetype)

  p <- plot_confint(p = p, data = plot.data, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}

#' Convert \code{stats::spec} to \code{data.frame}
#'
#' @param model \code{stats::spec} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(spectrum(AirPassengers))
#' fortify(stats::spec.ar(AirPassengers))
#' fortify(stats::spec.pgram(AirPassengers))
#' @export
fortify.spec <- function(model, data = NULL, ...) {
  d <- data.frame(Frequency = model$freq,
                  Spectrum = model$spec)
  post_fortify(d)
}

#' Autoplot \code{stats::spec}
#'
#' @param object \code{stats::spec} instance
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(stats::spec.ar(AirPassengers))
#' autoplot(stats::spec.pgram(AirPassengers))
#' @export
autoplot.spec <- function(object,
                          xlim = c(NA, NA), ylim = c(NA, NA), log = "y",
                          main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                          ...) {
  plot.data <- ggplot2::fortify(object)
  mapping <- ggplot2::aes_string(x = 'Frequency', y = 'Spectrum')
  p <- ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = mapping, stat = 'identity')
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}

#' Convert \code{stats::prcomp}, \code{stats::princomp} to \code{data.frame}
#'
#' @param model \code{stats::prcomp} or \code{stats::princomp} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @aliases fortify.princomp
#' @examples
#' fortify(stats::prcomp(iris[-5]))
#' fortify(stats::prcomp(iris[-5]), data = iris)
#'
#' fortify(stats::princomp(iris[-5]))
#' fortify(stats::princomp(iris[-5]), data = iris)
#' @export
fortify.prcomp <- function(model, data = NULL, ...) {

  if (is(model, 'prcomp')) {
    d <- as.data.frame(model$x)
    values <- model$x %*% t(model$rotation)
  } else if (is(model, 'princomp')) {
    d <- as.data.frame(model$scores)
    values <- model$scores %*% t(model$loadings[, ])
  } else {
    stop(paste0('Unsupported class for fortify.pca_common: ', class(model)))
  }

  values <- ggfortify::unscale(values, center = model$center,
                               scale = model$scale)
  values <- cbind_wraps(data, values)
  d <- cbind_wraps(values, d)
  post_fortify(d)
}

#' @export
fortify.princomp <- fortify.prcomp

#' Convert \code{stats::factanal} to \code{data.frame}
#'
#' @param model \code{stats::factanal} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
#' fortify(d.factanal)
#' fortify(d.factanal, data = state.x77)
#' @export
fortify.factanal <- function(model, data = NULL, ...) {

  if (is.null(model$scores)) {
    stop(paste0('Unable to fortify factanal result without scores, ',
                'specify scores="regression", or "Bartlett" when calling factanal'))
  }
  d <- as.data.frame(model$scores)
  d <- cbind_wraps(data, d)
  post_fortify(d)
}

#' Convert \code{lfda::lfda} or \code{lfda::klfda} or \code{lfda::self} to \code{data.frame}
#'
#' @param model \code{lfda::lfda} or \code{lfda::klfda} or \code{lfda::self} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' \dontrun{
#' model <- lfda::lfda(iris[, -5], iris[, 5], 3, metric = "plain")
#' fortify(model)
#' }
#' @export
fortify.lfda <- function(model, data = NULL, ...) {

  if (!is(model, 'lfda')) {
    stop('model is not a lfda object')
  }

  model <- stats::prcomp(model$Z)

  d <- as.data.frame(model$x)
  values <- model$x %*% t(model$rotation)

  values <- ggfortify::unscale(values, center = model$center,
                               scale = model$scale)
  values <- cbind_wraps(data, values)
  d <- cbind_wraps(values, d)
  post_fortify(d)
}

#' Autoplot PCA-likes
#'
#' @param object PCA-like instance
#' @param data Joined to fitting result if provided.
#' @param scale scaling parameter, disabled by 0
#' @param x principal component number used in x axis
#' @param y principal component number used in y axis
#' @param variance_percentage show the variance explained by the principal component?
#' @param ... other arguments passed to [ggbiplot()]
#' @inheritParams ggbiplot
#' @inheritParams plot_label
#' @inheritParams post_autoplot
#' @aliases autoplot.prcomp autoplot.princomp autoplot.factanal
#' @examples
#' autoplot(stats::prcomp(iris[-5]))
#' autoplot(stats::prcomp(iris[-5]), data = iris)
#' autoplot(stats::prcomp(iris[-5]), data = iris, colour = 'Species')
#' autoplot(stats::prcomp(iris[-5]), label = TRUE, loadings = TRUE, loadings.label = TRUE)
#' autoplot(stats::prcomp(iris[-5]), frame = TRUE)
#' autoplot(stats::prcomp(iris[-5]), data = iris, frame = TRUE,
#'          frame.colour = 'Species')
#' autoplot(stats::prcomp(iris[-5]), data = iris, frame = TRUE,
#'          frame.type = 't', frame.colour = 'Species')
#'
#' autoplot(stats::princomp(iris[-5]))
#' autoplot(stats::princomp(iris[-5]), data = iris)
#' autoplot(stats::princomp(iris[-5]), data = iris, colour = 'Species')
#' autoplot(stats::princomp(iris[-5]), label = TRUE, loadings = TRUE, loadings.label = TRUE)
#'
#' #Plot PC 2 and 3
#' autoplot(stats::princomp(iris[-5]), x = 2, y = 3)
#'
#' #Don't show the variance explained
#' autoplot(stats::princomp(iris[-5]), variance_percentage = FALSE)
#'
#' d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
#' autoplot(d.factanal)
#' autoplot(d.factanal, data = state.x77, colour = 'Income')
#' autoplot(d.factanal, label = TRUE, loadings = TRUE, loadings.label = TRUE)
#' @export
autoplot.pca_common <- function(object, data = NULL,
                                scale = 1.0, x = 1, y = 2,
                                variance_percentage = TRUE, ...) {

  plot.data <- ggplot2::fortify(object, data = data)
  plot.data$rownames <- rownames(plot.data)

  if (is_derived_from(object, 'prcomp')) {

    ve <- object$sdev^2 / sum(object$sdev^2)
    PC <- paste0("PC", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    loadings.column <- 'rotation'

    lam <- object$sdev[c(x, y)]
    lam <- lam * sqrt(nrow(plot.data))

  } else if (is_derived_from(object, 'princomp')) {

    ve <- object$sdev^2 / sum(object$sdev^2)
    PC <- paste0("Comp.", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    loadings.column <- 'loadings'

    lam <- object$sdev[c(x, y)]
    lam <- lam * sqrt(nrow(plot.data))

  } else if (is_derived_from(object, 'factanal')) {

    if (is.null(attr(object, "covariance"))) {
      p <- nrow(object$loading)
      ve <- colSums(object$loading^2) / p
    } else ve <- NULL

    PC <- paste0("Factor", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    scale <- 0
    loadings.column <- 'loadings'

  } else if (is_derived_from(object, 'lfda')) {

    ve <- NULL
    PC <- paste0("PC", c(x, y))
    x.column <- PC[1]
    y.column <- PC[2]
    scale <- 0
    loadings.column <- NULL

  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(object)))
  }

  # scaling
  if (scale != 0) {
    lam <- lam ^ scale
    plot.data[, c(x.column, y.column)] <- t(t(plot.data[, c(x.column, y.column)]) / lam)
  }

  # move target columns to 1st and 2nd
  plot.columns <- unique(c(x.column, y.column, colnames(plot.data)))
  plot.data <- plot.data[, plot.columns]

  if (!is.null(loadings.column)) {
    loadings.data <-as.data.frame(object[[loadings.column]][, ])
    loadings.data$rownames <- rownames(loadings.data)

    loadings.columns <- unique(c(x.column, y.column, colnames(loadings.data)))
    loadings.data <- loadings.data[, loadings.columns]
  } else {
    loadings.data <- NULL
  }

  #Make labels
  if (is.null(ve) | !variance_percentage) {
    labs <- PC
  } else {
    ve <- ve[c(x, y)]
    labs <- paste0(PC, " (", round(ve * 100, 2), "%)")
  }
  xlab <- labs[1]
  ylab <- labs[2]

  p <- ggbiplot(plot.data = plot.data,
                loadings.data = loadings.data,
                xlab = xlab,
                ylab = ylab, ...)
  return(p)
}

#' @export
autoplot.prcomp <- autoplot.pca_common

#' @export
autoplot.princomp <- autoplot.pca_common

#' @export
autoplot.factanal <- autoplot.pca_common

#' @export
autoplot.lfda <- autoplot.pca_common

#' Convert \code{stats::dist} to \code{data.frame}
#'
#' @param model \code{stats::dist} instance
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' fortify(eurodist)
#' @export
fortify.dist <- function(model, data = NULL, ...) {
  model <- as.matrix(model)
  ggplot2::fortify(model)
}

#' @export
autoplot.dist <- autoplot.matrix


#' Convert \code{stats::stepfun} to \code{data.frame}
#'
#' @param model \code{stats::stepfun} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(stepfun(c(1, 2, 3), c(4, 5, 6, 7)))
#' fortify(stepfun(c(1), c(4, 5)))
#' fortify(stepfun(c(1, 3, 4, 8), c(4, 5, 2, 3, 5)))
#' fortify(stepfun(c(1, 2, 3, 4, 5, 6, 7, 8, 10), c(4, 5, 6, 7, 8, 9, 10, 11, 12, 9)))
#' @export
fortify.stepfun <- function(model, data, ...) {
  x <- stats::knots(model)
  lim <- range(x)
  if (length(x) > 1L) {
    dr <- max(0.08 * diff(lim), stats::median(diff(x)))
  } else {
    dr <- abs(x) / 16
  }
  lim <- lim + dr * c(-1, 1)
  x <- c(lim[1], rep(x, each = 2), lim[2])
  y <- base::eval(expression(c(yleft, y)), envir = environment(model))
  y <- rep(y, each = 2)
  d <- data.frame(x = x, y = y)
  post_fortify(d)
}

#' Plot \code{stats::stepfun}
#'
#' @param object \code{stats::stepfun} instance
#' @param colour colour
#' @param size point size
#' @param linetype line type
#' @param alpha alpha
#' @param shape point shape
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(stepfun(c(1, 2, 3), c(4, 5, 6, 7)))
#' autoplot(stepfun(c(1), c(4, 5)), shape = NULL)
#' autoplot(stepfun(c(1, 3, 4, 8), c(4, 5, 2, 3, 5)), linetype = 'dashed')
#' autoplot(stepfun(c(1, 2, 3, 4, 5, 6, 7, 8, 10), c(4, 5, 6, 7, 8, 9, 10, 11, 12, 9)), colour = 'red')
#' @export
autoplot.stepfun <- function(object,
                             colour = NULL, size = NULL, linetype = NULL,
                             alpha = NULL, shape = 1,
                             xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                             main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                             ...) {
  # not accept shape kw

  plot.data <- ggplot2::fortify(object)

  mapping <- ggplot2::aes_string(x = 'x', y = 'y')
  p <- ggplot2::ggplot(plot.data, mapping = mapping) +
    geom_factory(ggplot2::geom_line, data = plot.data,
                colour = colour, size = size, linetype = linetype,
                alpha = alpha, stat = 'identity')
  if (nrow(plot.data) >= 3 & !is.null(shape)) {
    # indexer will be logical vector
    # to skip first value, start with c(FALSE, FALSE)
    indexer <- c(FALSE, FALSE, 3:nrow(plot.data) %% 2 == 1)
    point.data <- plot.data[indexer, ]
    p <- p +
      geom_factory(ggplot2::geom_point, data = point.data,
                   colour = colour, size = size,
                   alpha = alpha, shape = shape)
  }

  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
