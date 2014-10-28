library(ggplot2)
library(gridExtra)

#' Convert time-series-like to data.frame.
#' 
#' @param data \code{stats::ts}, \code{timeSeries::timeSeries} or \code{tseries::irts} instance
#' @param index.name Specify column name for time series index
#' @param index.name Specify column name for univariate time series data. Ignored in multivariate time series. 
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @param columns Character vector specifies target column name(s)
#' @param melt Logical flag indicating whether to melt each timeseries as variable
#' @return data.frame
#' @examples
#' ggplot2::fortify(AirPassengers)
#' ggplot2::fortify(timeSeries::as.timeSeries(AirPassengers))
#' 
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' ggplot2::fortify(its)
#' @export
fortify.ts <- function(data, index.name = 'Index', data.name = 'Data', 
                       is.date = NULL, columns = NULL, melt = FALSE) {
  # no need to define `fortify.xts` because zoo package has `fortify.zoo`
  
  if (is(data, 'timeSeries')) {
    d <- as.data.frame(data)
    dtindex <- as.POSIXct(rownames(d))
  } else if (is(data, 'irts')) {
    d <- as.data.frame(data$value)
    dtindex <- data$time
  } else if (is(data, 'ts')) {
    d <- as.data.frame(as.matrix(data))
    dtindex <- get.dtindex(data, is.date = is.date)
  } else {
    stop(paste0('Unsupported class for fortify.ts: ', class(data)))
  }
  dtframe <- data.frame(Index = dtindex)
  colnames(dtframe) <- index.name
  if (ncol(d) == 1) {
    colnames(d) <- data.name
  }
  d <- cbind(dtframe, d)
  
  # filtering columns
  if (is.null(columns)) {
    data.names <- names(d)
    columns <- data.names[data.names != index.name]
  } else {
    d <- dplyr::select_(d, .dots = c(index.name, columns))
  }
  # unpivot
  if (melt) {
    d <- tidyr::gather_(d, 'variable', 'value', columns)
  }  
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
#' @param group Character vector specifies grouping
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @param p \code{ggplot2::ggplot} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @param facet Logical value to specify use facets for multivariate time-series
#' @param ts.geom Geometric string for colour for time-series. 'line' or 'bar'.
#' @param ts.colour Line colour for time-series
#' @param ts.linetype Line type for time-series
#' @param xlab Character vector or expression for x axis label
#' @param ylab Character vector or expression for y axis label
#' @return ggplot
#' @aliases autoplot.xts autoplot.timeSeries autoplot.irts autoplot.stl autoplot.decomposed.ts
#' @examples
#' data(Canada, package = 'vars')
#' ggplot2::autoplot(AirPassengers)
#' ggplot2::autoplot(UKgas, ts.geom = 'bar')
#' ggplot2::autoplot(Canada)
#' ggplot2::autoplot(Canada, columns = 'e', is.date = TRUE)
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
#' @export
autoplot.ts <- function(data, columns = NULL, group = NULL, is.date = NULL,
                        p = NULL, 
                        scales = 'free_y', facet = TRUE,
                        ts.geom = 'line',
                        ts.colour = '#000000', ts.linetype = 'solid', 
                        xlab = '', ylab = '') {
  
  # fortify data
  if (is.data.frame(data)) {
    plot.data <- data
  } else {
    plot.data <- ggplot2::fortify(data, is.date = is.date)
  }
  
  # create ggplot instance if not passed
  ts.label = 'Index'
  if (is.null(p)) {
    null.p <- TRUE
    mapping = ggplot2::aes_string(x = ts.label)
    p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  } else {
    null.p <- FALSE
  }
  
  if (ts.geom == 'line') {
    geomobj <- ggplot2::geom_line
  } else if (ts.geom == 'bar') {
    geomobj <- ggplot2::geom_bar
  } else {
    stop("Invalid geom is specified. It must be 'line' or 'bar'")
  }
  
  if (is.null(columns)) {
    data.names <- names(plot.data)
    columns <- data.names[data.names != ts.label]
  }
  if (length(columns) > 1) {
    .is.univariate <- FALSE
  } else {
    .is.univariate <- TRUE
  }
  plot.data <- tidyr::gather_(plot.data, 'variable', 'value', columns)
  
  # must be done here, because fortify.zoo is defined in zoo package
  ts.column <- plot.data[[ts.label]]
  if (is(ts.column, 'yearmon') || is(ts.column, 'yearqtr')) {
    plot.data[[ts.label]] <- zoo::as.Date(plot.data[[ts.label]])
  } 

  if (facet) {
    mapping <- ggplot2::aes_string(x = ts.label, y = 'value')
    p <- p + geomobj(data = plot.data, mapping = mapping,
                     colour = ts.colour, linetype = ts.linetype, stat = 'identity')
    if (!.is.univariate) {
      p <- p + ggplot2::facet_grid(variable ~ ., scales = scales)
    }
  } else {
    # ts.colour cannot be used
    mapping <- ggplot2::aes_string(x = ts.label, y = 'value', colour = 'variable')
    p <- p + geomobj(data = plot.data, mapping = mapping,
                     linetype = ts.linetype, stat = 'identity')
  }
  if (null.p) {
    p <- p +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_y_continuous() 
  }
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

#' Convert time series models (like AR, ARIMA) to data.frame.
#' 
#' @param data \code{stats::ar}, \code{stats::Arima}, \code{stats::HoltWinters},
#'  \code{fracdiff::fracdiff}, \code{forecast::nnetar} or \code{fGarch::fGARCH}instance
#' @param original Original data for \code{stats::ar}, \code{stats::Arima}.
#' Not used in other models. 
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @return data.frame
#' @aliases fortify.ar fortify.Arima fortify.fracdiff 
#' fortify.nnetar fortify.HoltWinters fortify.fGARCH
#' @examples
#' ggplot2::fortify(stats::ar(AirPassengers))
#' ggplot2::fortify(stats::arima(UKgas))
#' ggplot2::fortify(stats::arima(UKgas), original = UKgas, is.date = TRUE)
#' ggplot2::fortify(forecast::auto.arima(austres))
#' ggplot2::fortify(forecast::arfima(AirPassengers))
#' ggplot2::fortify(forecast::nnetar(UKgas))
#' ggplot2::fortify(stats::HoltWinters(USAccDeaths))
#' 
#' data(LPP2005REC, package = 'timeSeries')
#' x = timeSeries::as.timeSeries(LPP2005REC)
#' d.Garch = fGarch::garchFit(LPP40 ~ garch(1, 1), data = 100 * x, trace = FALSE)
#' ggplot2::fortify(d.Garch)
fortify.tsmodel <- function(data, original = NULL, is.date = NULL) {
  
  if (is(data, 'Arima') || is(data, 'ar')) {
    if (is.null(original)) {
      original <- forecast::getResponse(data)
      fit <- fitted(data)
    } else {
      fit <- original - residuals(data)
    }
    d <- ggplot2::fortify(original, is.date = is.date)
    fit <- ggplot2::fortify(fit, data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(residuals(data), data.name = 'Residuals', is.date = is.date)
    
  } else if (is(data, 'fracdiff') || is(data, 'nnetar') ||
               (is(data, 'HoltWinters'))) {
    d <- ggplot2::fortify(data$x, is.date = is.date)
    fit <- ggplot2::fortify(fitted(data), data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(residuals(data), data.name = 'Residuals', is.date = is.date)
  } else if (is(data, 'fGARCH')) {
    index = attr(data@data, 'names')
    d <- data.frame(Index = index, Data = data@data)
    fit <- data.frame(Index = index, Fitted = data@fitted)
    resid <- data.frame(Index = index, Residuals = data@residuals)
  } else {
    stop(paste0('Unsupported class for fortify.Arima: ', class(data)))
  }
  d <- dplyr::left_join(d, fit, by = 'Index')
  d <- dplyr::left_join(d, resid, by = 'Index')
  dplyr::tbl_df(d)
}

#' @export
fortify.ar <- fortify.tsmodel

#' @export
fortify.Arima <- fortify.tsmodel

#' @export
fortify.tsmodel <- fortify.tsmodel

#' @export
fortify.HoltWinters <- fortify.tsmodel

#' @export
fortify.fracdiff <- fortify.tsmodel

#' @export
fortify.nnetar <- fortify.tsmodel

#' @export
fortify.fGARCH <- fortify.tsmodel

#' Autoplot time series models (like AR, ARIMA).
#' 
#' @param data \code{stats::ar}, \code{stats::Arima}, \code{stats::HoltWinters},
#'  \code{fracdiff::fracdiff}, \code{forecast::nnetar} or \code{fGarch::fGARCH}instance
#' @param original Original data for \code{stats::ar}, \code{stats::Arima}.
#' Not used in other models. 
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @param fitted.colour Line colour for fitted time-series
#' @param fitted.linetype Line type for fitted time-series
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @aliases autoplot.ar autoplot.fracdiff autoplot.nnetar autoplot.HoltWinters autoplot.fGARCH
#' @examples
#' ggplot2::autoplot(stats::ar(AirPassengers))
#' ggplot2::autoplot(stats::arima(UKgas), original = UKgas)
#' ggplot2::autoplot(forecast::arfima(AirPassengers))
#' ggplot2::autoplot(forecast::nnetar(UKgas), is.date = FALSE)
#' ggplot2::autoplot(stats::HoltWinters(USAccDeaths))
#' @export
autoplot.tsmodel <- function(data, original = NULL, is.date = NULL,
                             fitted.colour = '#FF0000', fitted.linetype = 'solid',
                             ...) {
  if (is(data, 'HoltWinters')) {
    mapping = ggplot2::aes_string(y = 'xhat')
  } else {
    mapping = ggplot2::aes_string(y = 'Fitted')
  }
  plot.data <- ggplot2::fortify(data, original = original, is.date = is.date)
  p <- ggfortify:::autoplot.ts(plot.data, columns = 'Data', ...)
  p <- p + ggplot2::geom_line(mapping = mapping,
                              colour = fitted.colour, linetype = fitted.linetype)
  p
}

#' @export
autoplot.ar <- autoplot.tsmodel

#' @export
autoplot.Arima <- autoplot.tsmodel

#' @export
autoplot.HoltWinters <- autoplot.tsmodel

#' @export
autoplot.fracdiff <- autoplot.tsmodel

#' @export
autoplot.nnetar <- autoplot.tsmodel

#' @export
autoplot.fGarch <- autoplot.tsmodel