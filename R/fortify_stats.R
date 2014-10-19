#' Convert \code{stats::ts} to data.frame.
#' 
#' @param data \code{stats::ts} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(AirPassengers)
#' @export
fortify.ts <- function(data) {
  dtindex <- get.dtindex(data) 
  d <- cbind(data.frame(time = dtindex),
             as.data.frame(data))
  dplyr::tbl_df(d)
}

#' Autoplot \code{stats::ts}.
#' 
#' @param data \code{stats::ts} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @param facet Logical value to specify use facets for multivariate \code{stats::ts}.
#' @param ts.colour Line colour for \code{stats::ts}
#' @param ts.linetype Line type for \code{stats::ts}
#' @return ggplot
#' @examples
#' data(Canada, package = 'vars')
#' ggplot2::autoplot(AirPassengers)
#' ggplot2::autoplot(Canada)
#' ggplot2::autoplot(Canada, facet = FALSE)
#' @export
autoplot.ts <- function(data, scales = 'free_y', facet = TRUE,
                        ts.colour = '#000000', ts.linetype = 'solid') {
  ts.label <- 'time'
  
  plot.data <- ggplot2::fortify(data)
  data.names <- names(plot.data)
  measures <- data.names[data.names != ts.label]

  if (length(measures) == 1) {
    p <- ggplot2::ggplot(data = plot.data,
                         mapping = ggplot2::aes_string(x = ts.label, y = measures[1])) + 
      ggplot2::geom_line(colour = ts.colour, linetype = ts.linetype)
  } else { 
    plot.data <- reshape2::melt(plot.data, id.vars = c(ts.label),
                                measure.vars = measures)
      
    if (facet) {
      mapping <- ggplot2::aes_string(x = ts.label, y = 'value')
      p <- ggplot2::ggplot(data = plot.data, mapping = mapping) +
        ggplot2::geom_line(colour = ts.colour, linetype = ts.linetype) + 
        ggplot2::facet_grid(variable ~ ., scales = scales)
    } else {
      # ts.colour cannot be used
      mapping <- ggplot2::aes_string(x = ts.label, y = 'value', colour = 'variable')
      p <- ggplot2::ggplot(data = plot.data, mapping = mapping) +
        ggplot2::geom_line(linetype = ts.linetype)
    }
  }
  p + ggplot2::scale_y_continuous(name = '')
}

#' Convert \code{stats::stl} to data.frame.
#' 
#' @param data \code{stats::stl} instance
#' @return data.frame
#' @examples
#' d.stl <- stats::stl(AirPassengers, s.window = 'periodic')
#' ggplot2::fortify(d.stl)
#' @export
fortify.stl <- function(data) {
  ts.data <- data$time.series
  ncomp <- ncol(ts.data)
  orig <- drop(ts.data %*% rep(1, ncol(ts.data)))
  
  dtindex <- get.dtindex(ts.data)  
  d <- cbind(data.frame(time = dtindex, data = orig),
             data.frame(data$time.series))
  dplyr::tbl_df(d)
}

#' Autoplot \code{stats::stl}.
#' 
#' @param data \code{stats::stl} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @param ts.colour Line colour for \code{stats::ts}
#' @param ts.linetype Line type for \code{stats::ts}
#' @return ggplot
#' @examples
#' d.stl <- stats::stl(AirPassengers, s.window = 'periodic')
#' ggplot2::autoplot(d.stl)
#' @export
autoplot.stl <- function(data, scales = 'free_y',
                         ts.colour = '#000000', ts.linetype = 'solid') {
  plot.data <- ggplot2::fortify(data)
  measures <- c("data", "seasonal", "trend", "remainder")
  plot.data <- reshape2::melt(plot.data, id.vars = c('time'),
                              measure.vars = measures)
  ggplot2::ggplot(data = plot.data, 
                  mapping = ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = ts.colour, linetype = ts.linetype) +
    ggplot2::facet_grid(variable ~ ., scales = scales) + 
    ggplot2::scale_y_continuous(name = '')
}

#' Convert \code{stats::acf} to data.frame.
#' 
#' @param data \code{stats::acf} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(stats::acf(AirPassengers))
#' ggplot2::fortify(stats::pacf(AirPassengers))
#' ggplot2::fortify(stats::ccf(AirPassengers, AirPassengers))
#' @export
fortify.acf <- function(data) {
  d <- data.frame(lag = data$lag,
                  acf = data$acf)
  dplyr::tbl_df(d)
}

#' Autoplot \code{stats::acf}.
#' 
#' @param data \code{stats::acf} instance
#' @return ggplot
#' @examples
#' ggplot2::autoplot(stats::acf(AirPassengers))
#' ggplot2::autoplot(stats::pacf(AirPassengers))
#' ggplot2::autoplot(stats::ccf(AirPassengers, AirPassengers))
#' @export
autoplot.acf <- function(data) {
  plot.data <- ggplot2::fortify(data)

  # Prepare ymax and ymin used for geom_linerange
  plot.data <- dplyr::mutate(plot.data,
                             ymax = ifelse(acf > 0, acf, 0),
                             ymin = ifelse(acf < 0, acf, 0))

  # ToDo: Calcurate Confidence Interval
  # http://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram

  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_linerange(mapping = ggplot2::aes(x = lag, ymin = ymin, ymax = ymax))
}

#' Convert \code{stats::spec} to data.frame.
#' 
#' @param data \code{stats::spec} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(stats::spec.ar(AirPassengers))
#' ggplot2::fortify(stats::spec.pgram(AirPassengers))
#' @export
fortify.spec <- function(data) {
  d <- data.frame(freq = data$freq,
                  spec = data$spec)
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
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = freq, y = spec),
                      stat = 'identity') +
    ggplot2::scale_y_log10()
}

#' Convert \code{stats::prcomp}, \code{stats::princomp} to data.frame.
#' 
#' @param data \code{stats::prcomp} or \code{stats::princomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return data.frame
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
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
  if (!is.null(original)) {
    dots <- names(original)[! names(original) %in% names(values)]
    original <- dplyr::select_(original, .dots = dots)
    values <- cbind(values, original)
  }
  d <- cbind(values, d)
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
  if (!is.null(original)) {
    d <- cbind(original, d)
  }
  dplyr::tbl_df(d)
}

#' Autoplot PCA-likes.
#' 
#' @param data PCA-like instance
#' @param original Joined to fitting result if provided.
#' @param colour Column name string to specify colorize points 
#' @param label Logical value whether to display data labels
#' @param label.size Text size for data labels
#' @param loadings Logical value whether to display loadings arrows
#' @param loadings.colour Point colour for data
#' @param loadings.label Logical value whether to display loadings labels
#' @param loadings.label.colour Text colour for loadings labels
#' @param loadings.label.size Text size for loadings labels
#' @return ggplot
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::autoplot(stats::prcomp(df))
#' ggplot2::autoplot(stats::prcomp(df), original = iris)
#' ggplot2::autoplot(stats::prcomp(df), original = iris, colour = 'Species')
#' ggplot2::autoplot(stats::prcomp(df), label = TRUE, loadings = TRUE, loadings.label = TRUE)
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
                                label = FALSE, label.size = 4,
                                loadings = FALSE, loadings.colour = '#FF0000',
                                loadings.label = FALSE,
                                loadings.label.colour = '#FF0000', loadings.label.size = 4) {
  
  plot.data <- ggplot2::fortify(data, original = original)
  plot.data$rownames <- rownames(plot.data)
  
  if (is(data, 'prcomp')) {
    mapping = ggplot2::aes(x = PC1, y = PC2, label = rownames)
    loadings.mapping <- ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2)
    loadings.column = 'rotation'
  } else if (is(data, 'princomp')) {
    mapping = ggplot2::aes(x = Comp.1, y = Comp.2, label = rownames)
    loadings.mapping <- ggplot2::aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2)
    loadings.column = 'loadings'
  } else if (is(data, 'factanal')) {
    mapping <- ggplot2::aes(x = Factor1, y = Factor2, label = rownames)
    loadings.mapping <- ggplot2::aes(x = 0, y = 0, xend = Factor1, yend = Factor2)
    loadings.column = 'loadings'
  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(data)))
  }
  
  p <- ggplot2::ggplot(data = plot.data, mapping = mapping) + 
    ggplot2::geom_point(mapping = ggplot2::aes_string(colour = colour))
  
  if (label) {
    p <- p + ggplot2::geom_text(mapping = ggplot2::aes_string(colour = colour),
                                size = label.size)
  }
  
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
    
    if (loadings.label) {
      p <- p + geom_text(data = loadings.data,
                         colour = loadings.label.colour,
                         size = loadings.label.size)
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


#' Convert \code{stats::kmeans} to data.frame.
#' 
#' @param data \code{stats::kmeans} instance
#' @param original Joined to K-means result if provided. Intended to be used for attaching
#' cluster labels to the original
#' @return data.frame
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::fortify(stats::kmeans(df, 3))
#' ggplot2::fortify(stats::kmeans(df, 3), original = iris)
#' @export
fortify.kmeans <- function(data, original = NULL) {
  d <- data.frame(cluster = as.factor(data$cluster))
  if (!is.null(original)) {
    d <- cbind(original, d)
  }
  dplyr::tbl_df(d)
}


#' Autoplot \code{stats::kmeans}.
#' 
#' @param data \code{stats::kmeans} instance
#' @param original Original data used for K-means. Mandatory for plotting.
#' @return ggplot
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::autoplot(stats::kmeans(df, 3), original = iris)
#' @export
autoplot.kmeans <- function(data, original = NULL) {
  if (is.null(original)) {
    stop("'original' data is mandatory for plotting kmeans instance")
  }
  cls.fortified <- ggplot2::fortify(data, original = original)
  pca.data <- dplyr::select_(cls.fortified, .dots = colnames(data$center))
  p <- ggplot2::autoplot(stats::prcomp(pca.data), original = cls.fortified,
                         colour = 'cluster')
  p
}

