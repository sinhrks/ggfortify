#' Convert \code{stats::ts} to data.frame.
#' 
#' @param data \code{stats::ts} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(AirPassengers)
#' @export
fortify.ts <- function(data) {
  dtindex <- get.dtindex(data) 
  cbind(data.frame(time = dtindex),
        as.data.frame(data))
}

#' Autoplot \code{stats::ts}.
#' 
#' @param data \code{stats::ts} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @return ggplot
#' @examples
#' data(Canada, package = 'vars')
#' ggplot2::autoplot(AirPassengers)
#' ggplot2::autoplot(Canada)
#' @export
autoplot.ts <- function(data, scales = 'free_y') {
  ts.label <- 'time'
  
  plot.data <- ggplot2::fortify(data)
  data.names <- names(plot.data)
  measures <- data.names[data.names != ts.label]

  if (length(measures) == 1) {
    p <- ggplot2::ggplot(data = plot.data) +
      ggplot2::geom_line(mapping = ggplot2::aes_string(x = ts.label, y = measures[1]))
  } else { 
    plot.data <- reshape2::melt(plot.data, id.vars = c(ts.label),
                                measure.vars = measures)
    p <- ggplot2::ggplot(data = plot.data) +
      ggplot2::geom_line(mapping = ggplot2::aes_string(x = ts.label, y = 'value')) +
      ggplot2::facet_grid(variable ~ ., scales = scales)
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
  cbind(data.frame(time = dtindex, data = orig),
        data.frame(data$time.series))
}

#' Autoplot \code{stats::stl}.
#' 
#' @param data \code{stats::stl} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @return ggplot
#' @examples
#' d.stl <- stats::stl(AirPassengers, s.window = 'periodic')
#' ggplot2::autoplot(d.stl)
#' @export
autoplot.stl <- function(data, scales = 'free_y') {
  plot.data <- ggplot2::fortify(data)
  measures <- c("data", "seasonal", "trend", "remainder")
  plot.data <- reshape2::melt(plot.data, id.vars = c('time'),
                              measure.vars = measures)
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = time, y = value)) +
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
  data.frame(lag = data$lag,
             acf = data$acf)
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
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = lag, y = acf),
                      stat = 'identity')
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
  data.frame(freq = data$freq,
             spec = data$spec)
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

#' Convert \code{stats::prcomp} to data.frame.
#' 
#' @param data \code{stats::prcomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return data.frame
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::fortify(stats::prcomp(df))
#' ggplot2::fortify(stats::prcomp(df), original = iris)
#' @export
fortify.prcomp <- function(data, original = NULL) {
  d <- as.data.frame(data$x)
  values <- data$x %*% t(data$rotation)
  values <- ggfortify::unscale(values, center = data$center,
                               scale = data$scale)
  if (!is.null(original)) {
    dots <- names(original)[! names(original) %in% names(values)]
    original <- dplyr::select_(original, .dots = dots)
    values <- cbind(values, original)
  }
  cbind(values, d)
}

#' Autoplot \code{stats::prcomp}.
#' 
#' @param data \code{stats::prcomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return ggplot
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::autoplot(stats::prcomp(df))
#' ggplot2::autoplot(stats::prcomp(df), original = iris)
#' @export
autoplot.prcomp <- function(data, original = NULL) {
  plot.data <- ggplot2::fortify(data, original = original)
  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = PC1, y = PC2)) +
    ggplot2::geom_point()
}

#' Convert \code{stats::princomp} to data.frame.
#' 
#' @param data \code{stats::princomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return data.frame
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::fortify(stats::princomp(df))
#' ggplot2::fortify(stats::princomp(df), original = iris)
#' @export
fortify.princomp <- function(data, original = NULL) {
  d <- as.data.frame(data$scores)
  values <- data$scores %*% t(data$loadings[,])
  values <- ggfortify::unscale(values, center = data$center,
                               scale = data$scale)
  if (!is.null(original)) {
    dots <- names(original)[! names(original) %in% names(values)]
    original <- dplyr::select_(original, .dots = dots)
    values <- cbind(values, original)
  }
  cbind(values, d)
}

#' Autoplot \code{stats::princomp}.
#' 
#' @param data \code{stats::princomp} instance
#' @param original Joined to PCA result if provided. Intended to be used for attaching
#' non-numeric values original data has. Numeric values are automatically attached.
#' @return ggplot
#' @examples
#' df <- iris[c(1, 2, 3, 4)]
#' ggplot2::autoplot(stats::princomp(df))
#' ggplot2::autoplot(stats::princomp(df), original = iris)
#' @export
autoplot.princomp <- function(data, original = NULL) {
  plot.data <- ggplot2::fortify(data, original = original)
  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = Comp.1, y = Comp.2)) +
    ggplot2::geom_point()
}

