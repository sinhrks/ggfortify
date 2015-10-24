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
  plot.data <- dplyr::mutate_(plot.data,
                              ymax = 'ifelse(ACF > 0, ACF, 0)',
                              ymin = 'ifelse(ACF < 0, ACF, 0)')

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
    values <- model$scores %*% t(model$loadings[,])
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
#' model <- lfda::lfda(iris[, -5], iris[, 5], 3, metric = "plain")
#' fortify(model)
#' @export
fortify.lfda <- function(model, data = NULL, ...) {

  if(!is(model, 'lfda')){stop('model is not a lfda object')}

  pcaModel <- stats::prcomp(model$Z)

  if (is(pcaModel, 'prcomp')) {
    d <- as.data.frame(pcaModel$x)
    values <- pcaModel$x %*% t(pcaModel$rotation)
  } else if (is(pcaModel, 'princomp')) {
    d <- as.data.frame(pcaModel$scores)
    values <- pcaModel$scores %*% t(pcaModel$loadings[,])
  } else {
    stop(paste0('Unsupported class for fortify.pca_common: ', class(pcaModel)))
  }

  values <- ggfortify::unscale(values, center = pcaModel$center,
                               scale = pcaModel$scale)
  values <- cbind_wraps(data, values)
  d <- cbind_wraps(values, d)
  post_fortify(d)
}

#' Autoplot PCA-likes
#'
#' @param object PCA-like instance
#' @param data Joined to fitting result if provided.
#' @param colour colour
#' @param size size
#' @param linetype line type
#' @param alpha alpha
#' @param fill fill
#' @param shape shape
#' @param label Logical value whether to display data labels
#' @inheritParams plot_label
#' @param loadings Logical value whether to display loadings arrows
#' @param loadings.colour Point colour for data
#' @param loadings.label Logical value whether to display loadings labels
#' @param loadings.label.label Column name used for loadings text labels
#' @param loadings.label.colour Colour for loadings text labels
#' @param loadings.label.alpha Alpha for loadings text labels
#' @param loadings.label.size Size for loadings text labels
#' @param loadings.label.angle Angle for loadings text labels
#' @param loadings.label.family Font family for loadings text labels
#' @param loadings.label.fontface Fontface for loadings text labels
#' @param loadings.label.lineheight Lineheight for loadings text labels
#' @param loadings.label.hjust Horizontal adjustment for loadings text labels
#' @param loadings.label.vjust Vertical adjustment for loadings text labels
#' @param frame Logical value whether to draw outliner convex / ellipse
#' @param frame.type Character specifying frame type.
#' 'convex' or types supporeted by \code{ggplot2::stat_ellipse} can be used.
#' @param frame.colour Colour for frame
#' @param frame.level Passed for \code{ggplot2::stat_ellipse} 's level. Ignored in 'convex'.
#' @param frame.alpha Alpha for frame
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
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
#' d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
#' autoplot(d.factanal)
#' autoplot(d.factanal, data = state.x77, colour = 'Income')
#' autoplot(d.factanal, label = TRUE, loadings = TRUE, loadings.label = TRUE)
autoplot.pca_common <- function(object, data = NULL,
                                colour = NULL, size = NULL, linetype = NULL,
                                alpha = NULL, fill = NULL, shape = NULL,
                                label = FALSE, label.label = 'rownames',
                                label.colour = colour, label.alpha = NULL,
                                label.size = NULL, label.angle = NULL,
                                label.family = NULL, label.fontface = NULL,
                                label.lineheight = NULL,
                                label.hjust = NULL, label.vjust = NULL,
                                loadings = FALSE, loadings.colour = '#FF0000',
                                loadings.label = FALSE,
                                loadings.label.label = 'rownames',
                                loadings.label.colour = '#FF0000',
                                loadings.label.alpha = NULL,
                                loadings.label.size = NULL, loadings.label.angle = NULL,
                                loadings.label.family = NULL, loadings.label.fontface = NULL,
                                loadings.label.lineheight = NULL,
                                loadings.label.hjust = NULL, loadings.label.vjust = NULL,
                                frame = FALSE, frame.type = 'convex',
                                frame.colour = colour, frame.level = 0.95,
                                frame.alpha = 0.2,
                                xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                                main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                                ...) {

  plot.data <- ggplot2::fortify(object, data = data)
  plot.data$rownames <- rownames(plot.data)

  if (is(object, 'prcomp')) {
    x.column <- 'PC1'
    y.column <- 'PC2'
    loadings.column <- 'rotation'
  } else if (is(object, 'princomp')) {
    x.column <- 'Comp.1'
    y.column <- 'Comp.2'
    loadings.column <- 'loadings'
  } else if (is(object, 'factanal')) {
    x.column <- 'Factor1'
    y.column <- 'Factor2'
    loadings.column <- 'loadings'
  } else if (is(object, 'lfda')) {
    x.column <- 'PC1'
    y.column <- 'PC2'
    loadings.column <- 'rotation'
  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(object)))
  }
  mapping <-  ggplot2::aes_string(x = x.column, y = y.column)
  loadings.mapping <- ggplot2::aes_string(x = 0, y = 0, xend = x.column, yend = y.column)

  if (is.logical(shape) && !shape && missing(label)) {
    # if label is missing and shape=FALSE, turn label to TRUE
    label <- TRUE
  }
  if (loadings.label && !loadings) {
    # If loadings.label is TRUE, draw loadings
    loadings <- TRUE
  }

  p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  if (!is.logical(shape) || shape) {
    p <- p + geom_factory(ggplot2::geom_point, plot.data,
                          colour = colour, size = size, linetype = linetype,
                          alpha = alpha, fill = fill, shape = shape)
  }
  p <- plot_label(p = p, data = plot.data, label = label,
                  label.label = label.label, label.colour = label.colour,
                  label.alpha = label.alpha, label.size = label.size,
                  label.angle = label.angle, label.family = label.family,
                  label.fontface = label.fontface, label.lineheight = label.lineheight,
                  label.hjust = label.hjust, label.vjust = label.vjust)
  if (loadings) {
    loadings.data <- as.data.frame(object[[loadings.column]][,])
    loadings.data$rownames <- rownames(loadings.data)

    p <- p + geom_segment(data = loadings.data,
                          mapping = loadings.mapping,
                          arrow = grid::arrow(length = grid::unit(8, 'points')),
                          colour = loadings.colour)
    p <- plot_label(p = p, data = loadings.data, label = loadings.label,
                    label.label = loadings.label.label, label.colour = loadings.label.colour,
                    label.alpha = loadings.label.alpha, label.size = loadings.label.size,
                    label.angle = loadings.label.angle, label.family = loadings.label.family,
                    label.fontface = loadings.label.fontface,
                    label.lineheight = loadings.label.lineheight,
                    label.hjust = loadings.label.hjust, label.vjust = loadings.label.vjust)
  }

  if (missing(frame) && !missing(frame.type)) {
    # if frame is missing but frame.type is specified, turn frame to TRUE
    frame <- TRUE
  }

  # dummy to solve "no visible binding for global variable '.'" warnings
  . <- NULL

  if (frame) {
    if (frame.type == 'convex') {
      if (is.null(frame.colour) || !(frame.colour %in% colnames(plot.data))) {
        hulls <- plot.data[grDevices::chull(plot.data[c(x.column, y.column)]), ]
      } else {
        hulls <- plot.data %>%
          dplyr::group_by_(frame.colour) %>%
          dplyr::do(.[grDevices::chull(.[c(x.column, y.column)]), ])
      }
      mapping <- aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::geom_polygon(data = hulls, mapping = mapping,
                                     alpha = frame.alpha)
    } else if (frame.type %in% c('t', 'norm', 'euclid')) {
      ggversion <- utils::packageVersion('ggplot2')
      if (utils::compareVersion(as.character(ggversion), '1.0.0') >= 0) {
        mapping <- aes_string(colur = frame.colour, fill = frame.colour)
        p <- p + ggplot2::stat_ellipse(mapping = mapping,
                                       level = frame.level, type = frame.type,
                                       geom = 'polygon', alpha = frame.alpha)
      } else {
        stop('ggplot 1.0.0 or later is required for stat_ellipse.')
      }

    }
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
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
  y <- eval(expression(c(yleft, y)), envir = environment(model))
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
