#' Convert time-series-like to data.frame.
#' 
#' @param data \code{stats::ts}, \code{timeSeries::timeSeries} or \code{tseries::irts} instance
#' @param index.name Specify column name for time series index
#' @param data.name Specify column name for time series data. Only used for univariate time-series.
#' Multivariate time-series retain the original column name
#' @return data.frame
#' @examples
#' ggplot2::fortify(AirPassengers)
#' ggplot2::fortify(timeSeries::as.timeSeries(AirPassengers))
#' 
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' ggplot2::fortify(its)
#' @export
fortify.ts <- function(data, index.name = 'Index', data.name = 'Data') {
  # no need to define `fortify.xts` because zoo package has `fortify.zoo`
  
  if (is(data, 'timeSeries')) {
    d <- as.data.frame(data)
    dtindex <- as.POSIXct(rownames(d))
  } else if (is(data, 'irts')) {
    d <- as.data.frame(data$value)
    dtindex <- data$time
  } else if (is(data, 'ts')) {
    d <- as.data.frame(data)
    dtindex <- get.dtindex(data)
  } else {
    stop(paste0('Unsupported class for fortify.ts: ', class(data)))
  }
  dtframe <- data.frame(Index = dtindex)
  colnames(dtframe) <- index.name
  if (ncol(d) == 1) {
    colnames(d) <- data.name
  }
  d <- cbind(dtframe, d)
  dplyr::tbl_df(d)
}

#' @export
fortify.timeSeries <- fortify.ts

#' @export
fortify.irts <- fortify.ts

#' Autoplot time-series-like.
#' 
#' @param data time-series-like instance
#' @param columns Character vector specifies target column name(s)
#' @param scales Scale value passed to \code{ggplot2}
#' @param facet Logical value to specify use facets for multivariate time-series
#' @param ts.colour Line colour for time-series
#' @param ts.linetype Line type for time-series
#' @return ggplot
#' @aliases autoplot.xts autoplot.timeSeries autoplot.irts autoplot.Arima autoplot.ar
#'  autoplot.fracdiff
#' @examples
#' data(Canada, package = 'vars')
#' ggplot2::autoplot(AirPassengers)
#' ggplot2::autoplot(UKgas)
#' ggplot2::autoplot(Canada)
#' ggplot2::autoplot(Canada, columns = 'e')
#' ggplot2::autoplot(Canada, facet = FALSE)
#' 
#' library(zoo)
#' ggplot2::autoplot(xts::as.xts(AirPassengers))
#' ggplot2::autoplot(xts::as.xts(UKgas))
#' ggplot2::autoplot(xts::as.xts(Canada))
#' 
#' ggplot2::autoplot(timeSeries::as.timeSeries(AirPassengers))
#' ggplot2::autoplot(timeSeries::as.timeSeries(Canada))
#' 
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' ggplot2::autoplot(its)
#' 
#' ggplot2::autoplot(stats::stl(UKgas, s.window = 'periodic'))
#' ggplot2::autoplot(stats::decompose(UKgas))
#' 
#' ggplot2::autoplot(stats::arima(UKgas))
#' ggplot2::autoplot(stats::ar(AirPassengers))
#' ggplot2::autoplot(forecast::auto.arima(AirPassengers))
#' ggplot2::autoplot(forecast::arfima(AirPassengers))
#' ggplot2::autoplot(forecast::nnetar(AirPassengers))
#' @export
autoplot.ts <- function(data, columns = NULL,
                        scales = 'free_y', facet = TRUE,
                        ts.colour = '#000000', ts.linetype = 'solid') {
  if (is.data.frame(data)) {
    ts.label = 'Index'
    plot.data <- data
  } else {
    ts.label = 'Index'
    plot.data <- ggplot2::fortify(data)
  }
  
  if (is.null(columns)) {
    data.names <- names(plot.data)
    columns <- data.names[data.names != ts.label]
  }
  
  ts.column <- plot.data[[ts.label]]
  if (is(ts.column, 'yearmon') || is(ts.column, 'yearqtr')) {
    plot.data[[ts.label]] <- zoo::as.Date(plot.data[[ts.label]])
  } 
  
  if (length(columns) == 1) {
    # plot.data <- dplyr::filter_(plot.data, paste0('!is.na(', measures[1], ')'))
    # Unable to use is.na because column can be ts object
    p <- ggplot2::ggplot(data = plot.data,
                         mapping = ggplot2::aes_string(x = ts.label, y = columns[1])) + 
      ggplot2::geom_line(colour = ts.colour, linetype = ts.linetype)
  } else { 
    plot.data <- tidyr::gather_(plot.data, 'variable', 'value', columns)
    # plot.data <- dplyr::filter(plot.data, !is.na(value))
    
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
  p <- p +
    ggplot2::scale_y_continuous(name = '') 
  p
}

#' @export
autoplot.zooreg <- autoplot.ts

#' @export
autoplot.xts <- autoplot.ts

#' @export
autoplot.timeSeries <- autoplot.ts

#' @export
autoplot.irts <- autoplot.ts