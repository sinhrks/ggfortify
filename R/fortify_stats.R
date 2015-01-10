#' @export
fortify.stl <- fortify.ts

#' @export
fortify.decomposed.ts <- fortify.stl

#' @export
autoplot.stl <- autoplot.ts

#' @export
autoplot.decomposed.ts <- autoplot.ts

#' Convert \code{stats::acf} to \code{data.frame}.
#' 
#' @param data \code{stats::acf} instance
#' @param conf.int Logical flag indicating whether to attach confidence intervals
#' @param conf.int.value Coverage probability for confidence interval
#' @param conf.int.type Type of confidence interval, 'white' for white noise or 'ma' MA(k-1) model
#' @return data.frame
#' @examples
#' ggplot2::fortify(stats::acf(AirPassengers))
#' ggplot2::fortify(stats::pacf(AirPassengers))
#' ggplot2::fortify(stats::ccf(AirPassengers, AirPassengers))
#' 
#' ggplot2::fortify(stats::acf(AirPassengers), conf.int = TRUE)
#' @export
fortify.acf <- function(data,
                        conf.int = TRUE, conf.int.value = 0.95,
                        conf.int.type = 'white') {
  d <- data.frame(Lag = data$lag, ACF = data$acf)
  if (conf.int) {
    cf <- confint.acf(data, ci = conf.int.value, ci.type = conf.int.type)
    cfd <- data.frame(lower = -cf, upper = cf)
    d <- cbind(d, cfd)
  }
  dplyr::tbl_df(d)
}

#' Autoplot \code{stats::acf}.
#' 
#' @param data \code{stats::acf} instance
#' @param colour Line colour
#' @param linetype Line type
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param conf.int.value Coverage probability for confidence interval
#' @param conf.int.type Type of confidence interval, 'white' for white noise or 'ma' MA(k-1) model
#' @return ggplot
#' @examples
#' ggplot2::autoplot(stats::acf(AirPassengers))
#' ggplot2::autoplot(stats::pacf(AirPassengers))
#' ggplot2::autoplot(stats::ccf(AirPassengers, AirPassengers))
#' @export
autoplot.acf <- function(data, 
                         colour = '#000000', linetype = 'solid',
                         conf.int = TRUE,
                         conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                         conf.int.fill = NULL, conf.int.alpha = 0.3,
                         conf.int.value = 0.95, conf.int.type = 'white') {
  plot.data <- ggplot2::fortify(data, conf.int = conf.int,
                                conf.int.value = conf.int.value,
                                conf.int.type = conf.int.type)

  # Prepare ymax and ymin used for geom_linerange
  plot.data <- dplyr::mutate(plot.data,
                             ymax = ifelse(ACF > 0, ACF, 0),
                             ymin = ifelse(ACF < 0, ACF, 0))

  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_linerange(mapping = ggplot2::aes_string(ymin = 'ymin', ymax = 'ymax'),
                            colour = colour, linetype = linetype)

  p <- plot.conf.int(p, conf.int = conf.int,
                     conf.int.colour = conf.int.colour,
                     conf.int.linetype = conf.int.linetype,
                     conf.int.fill = conf.int.fill,
                     conf.int.alpha = conf.int.alpha)
  p <- p + ggplot2::ylab('ACF')
  p
}

#' Convert \code{stats::spec} to data.frame.
#' 
#' @param data \code{stats::spec} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(spectrum(AirPassengers))
#' ggplot2::fortify(stats::spec.ar(AirPassengers))
#' ggplot2::fortify(stats::spec.pgram(AirPassengers))
#' @export
fortify.spec <- function(data) {
  d <- data.frame(Frequency = data$freq,
                  Spectrum = data$spec)
  dplyr::tbl_df(d)
}

#' Autoplot \code{stats::spec}.
#' 
#' @param data \code{stats::spec} instance
#' @return ggplot
#' @examples
#' ggplot2::autoplot(stats::spec.ar(AirPassengers))
#' ggplot2::autoplot(stats::spec.pgram(AirPassengers))
#' @export
autoplot.spec <- function(data) {
  plot.data <- ggplot2::fortify(data)
  mapping <- ggplot2::aes_string(x = 'Frequency', y = 'Spectrum')
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = mapping, stat = 'identity') +
    ggplot2::scale_y_log10()
}

#' Convert \code{stats::prcomp}, \code{stats::princomp} to data.frame.
#' 
#' @param data \code{stats::prcomp} or \code{stats::princomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return data.frame
#' @aliases fortify.princomp
#' @examples
#' df <- iris[-5]
#' ggplot2::fortify(stats::prcomp(df))
#' ggplot2::fortify(stats::prcomp(df), original = iris)
#' 
#' ggplot2::fortify(stats::princomp(df))
#' ggplot2::fortify(stats::princomp(df), original = iris)
#' @export
fortify.prcomp <- function(data, original = NULL) {
  if (is(data, 'prcomp')) {
    d <- as.data.frame(data$x)
    values <- data$x %*% t(data$rotation)
  } else if (is(data, 'princomp')) {
    d <- as.data.frame(data$scores)
    values <- data$scores %*% t(data$loadings[,])
  } else {
    stop(paste0('Unsupported class for fortify.pca_common: ', class(data)))
  }

  values <- ggfortify::unscale(values, center = data$center,
                               scale = data$scale)
  values <- cbind_wraps(values, original)
  d <- cbind_wraps(values, d)
  dplyr::tbl_df(d)
}

#' @export
fortify.princomp <- fortify.prcomp

#' Convert \code{stats::factanal} to data.frame.
#' 
#' @param data \code{stats::factanal} instance
#' @param original Joined to Factanal result if provided.
#' @return data.frame
#' @examples
#' d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
#' ggplot2::fortify(d.factanal)
#' ggplot2::fortify(d.factanal, original = state.x77)
#' @export
fortify.factanal <- function(data, original = NULL) {
  if (is.null(data$scores)) {
    stop(paste0('Unable to fortify factanal result without scores, ',
                'specify scores="regression", or "Bartlett" when calling factanal'))
  }
  d <- as.data.frame(data$scores)
  d <- cbind_wraps(original, d)
  dplyr::tbl_df(d)
}

#' Autoplot PCA-likes.
#' 
#' @param data PCA-like instance
#' @param original Joined to fitting result if provided.
#' @param colour Column name string to specify colorize points 
#' @param label Logical value whether to display data labels
#' @param label.colour Text colour for data labels
#' @param label.size Text size for data labels
#' @param loadings Logical value whether to display loadings arrows
#' @param loadings.colour Point colour for data
#' @param loadings.label Logical value whether to display loadings labels
#' @param loadings.label.colour Text colour for loadings labels
#' @param loadings.label.size Text size for loadings labels
#' @param frame Logical value whether to draw outliner convex / ellipse
#' @param frame.type Character specifying frame type.
#' 'convex' or types supporeted by \code{ggplot2::stat_ellipse} can be used.
#' @param frame.colour Colour for frame
#' @param frame.level Passed for \code{ggplot2::stat_ellipse} 's level. Ignored in 'convex'.
#' @param frame.alpha Alpha for frame
#' @return ggplot
#' @aliases autoplot.prcomp autoplot.princomp autoplot.factanal 
#' @examples
#' df <- iris[-5]
#' ggplot2::autoplot(stats::prcomp(df))
#' ggplot2::autoplot(stats::prcomp(df), original = iris)
#' ggplot2::autoplot(stats::prcomp(df), original = iris, colour = 'Species')
#' ggplot2::autoplot(stats::prcomp(df), label = TRUE, loadings = TRUE, loadings.label = TRUE)
#' ggplot2::autoplot(stats::prcomp(df), frame = TRUE)
#' ggplot2::autoplot(stats::prcomp(df), original = iris, frame = TRUE,
#'                   frame.colour = 'Species')
#' ggplot2::autoplot(stats::prcomp(df), original = iris, frame = TRUE,
#'                   frame.type = 't', frame.colour = 'Species')
#' 
#' ggplot2::autoplot(stats::princomp(df))
#' ggplot2::autoplot(stats::princomp(df), original = iris)
#' ggplot2::autoplot(stats::princomp(df), original = iris, colour = 'Species')
#' ggplot2::autoplot(stats::princomp(df), label = TRUE, loadings = TRUE, loadings.label = TRUE)
#' 
#' d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
#' ggplot2::autoplot(d.factanal)
#' ggplot2::autoplot(d.factanal, original = state.x77, colour = 'Income')
#' ggplot2::autoplot(d.factanal, label = TRUE, loadings = TRUE, loadings.label = TRUE)
autoplot.pca_common <- function(data, original = NULL,
                                colour = NULL, 
                                label = FALSE, label.colour = colour, label.size = 4,
                                loadings = FALSE, loadings.colour = '#FF0000',
                                loadings.label = FALSE,
                                loadings.label.colour = '#FF0000',
                                loadings.label.size = 4,
                                frame = FALSE, frame.type = 'convex', 
                                frame.colour = colour, frame.level = 0.95,
                                frame.alpha = 0.2) {
  
  plot.data <- ggplot2::fortify(data, original = original)
  plot.data$rownames <- rownames(plot.data)
  
  if (is(data, 'prcomp')) {
    x.column <- 'PC1'
    y.column <- 'PC2'
    loadings.column <- 'rotation'
  } else if (is(data, 'princomp')) {
    x.column <- 'Comp.1'
    y.column <- 'Comp.2'
    loadings.column <- 'loadings'
  } else if (is(data, 'factanal')) {
    x.column <- 'Factor1'
    y.column <- 'Factor2'
    loadings.column <- 'loadings'
  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(data)))
  }
  mapping = ggplot2::aes_string(x = x.column, y = y.column)
  loadings.mapping <- ggplot2::aes_string(x = 0, y = 0, xend = x.column, yend = y.column)
  
  p <- ggplot2::ggplot(data = plot.data, mapping = mapping) + 
    ggplot2::geom_point(mapping = ggplot2::aes_string(colour = colour))
  
  p <- plot.label(p = p, data = plot.data, flag = label, label = 'rownames',
                  colour = label.colour, size = label.size)

  if (loadings.label && !loadings) {
    # If loadings.label is TRUE, draw loadings 
    loadings <- TRUE
  }
  
  if (loadings) {
    loadings.data = as.data.frame(data[[loadings.column]][,])
    loadings.data$rownames <- rownames(loadings.data)
    
    p <- p + geom_segment(data = loadings.data,
                          mapping = loadings.mapping,
                          arrow = grid::arrow(length = grid::unit(8, 'points')),
                          colour = loadings.colour)
    
    p <- plot.label(p = p, data = loadings.data, flag = loadings.label, label = 'rownames',
                    colour = loadings.label.colour, size = loadings.label.size)
  }
  
  if (frame) {
    if (frame.type == 'convex') {
      if (is.null(frame.colour) || !(frame.colour %in% colnames(plot.data))) {
        hulls <- plot.data[chull(plot.data[c(x.column, y.column)]), ]
        print(hulls)
        # p <- p + ggplot2::geom_polygon(data = hulls, 
        #                                alpha = frame.alpha)
      } else {
        hulls <- plot.data %>%
          dplyr::group_by_(frame.colour) %>%
          dplyr::do(.[chull(.[c(x.column, y.column)]), ])

      }
      mapping = aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::geom_polygon(data = hulls, mapping = mapping,
                                     alpha = frame.alpha)
    } else if (frame.type %in% c('t', 'norm', 'euclid')) {
      ggversion <- utils::packageVersion('ggplot2')
      if (compareVersion(as.character(ggversion), '1.0.0') >= 0) {
        mapping = aes_string(colur = frame.colour, fill = frame.colour)
        p <- p + ggplot2::stat_ellipse(mapping = mapping,
                                       level = frame.level, type = frame.type,
                                       geom = 'polygon', alpha = frame.alpha)
      } else {
        stop('ggplot 1.0.0 or later is required for stat_ellipse.')
      }
      
    }
  }
  p
}

#' @export
autoplot.prcomp <- autoplot.pca_common

#' @export
autoplot.princomp <- autoplot.pca_common

#' @export
autoplot.factanal <- autoplot.pca_common

#' Convert \code{stats::dist} to data.frame.
#' 
#' @param data \code{stats::dist} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(eurodist)
#' @export
fortify.dist <- function(data, original = NULL) {
  data <- as.matrix(data)
  ggplot2::fortify(data, original = original)
}

#' @export
autoplot.dist <- autoplot.matrix