#' Convert \code{stats::stl} to data.frame.
#' 
#' @param stl.data \code{stats::stl} instance
#' @return data.frame
#' @examples
#' d.stl <- stats::stl(AirPassengers, s.window = 'periodic')
#' ggplot2::fortify(d.stl)
#' @export
fortify.stl <- function(stl.data) {
  ts.data <- stl.data$time.series
  ncomp <- ncol(ts.data)
  orig <- drop(ts.data %*% rep(1, ncol(ts.data)))
  
  dtindex <- attr(ts.data, which='tsp')
  dtindex <- seq(from = dtindex[1], to = dtindex[2], by= 1 / dtindex[3])
  dtindex <- zoo::as.Date.yearmon(dtindex)
  
  cbind(data.frame(time = dtindex, data = orig),
        data.frame(stl.data$time.series))
}

#' Autoplot \code{stats::stl}.
#' 
#' @param stl.data \code{stats::stl} instance
#' @return data.frame
#' @examples
#' d.stl <- stats::stl(AirPassengers, s.window = 'periodic')
#' ggplot2::autoplot(d.stl)
#' @export
autoplot.stl <- function(stl.data) {
  plot.data <- ggplot2::fortify(stl.data)
  measures <- c("data", "seasonal", "trend", "remainder")
  plot.data <- reshape2::melt(plot.data, id.vars = c('time'),
                              measure.vars = measures)
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = time, y = value)) +
    ggplot2::facet_grid(variable ~ ., scales = 'free_y')
}

#' Convert \code{stats::acf} to data.frame.
#' 
#' @param acf.data \code{stats::acf} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(stats::acf(AirPassengers))
#' ggplot2::fortify(stats::pacf(AirPassengers))
#' ggplot2::fortify(stats::ccf(AirPassengers, AirPassengers))
#' @export
fortify.acf <- function(acf.data) {
  data.frame(lag = acf.data$lag,
             acf = acf.data$acf)
}

#' Autoplot \code{stats::acf}.
#' 
#' @param acf.data \code{stats::acf} instance
#' @return data.frame
#' @examples
#' ggplot2::autoplot(stats::acf(AirPassengers))
#' ggplot2::autoplot(stats::pacf(AirPassengers))
#' ggplot2::autoplot(stats::ccf(AirPassengers, AirPassengers))
#' @export
autoplot.acf <- function(acf.data) {
  plot.data <- ggplot2::fortify(acf.data)
  ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = lag, y = acf),
                      stat = 'identity')
}

