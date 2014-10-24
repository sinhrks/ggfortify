#' Convert \code{forecast::forecast} to data.frame.
#' 
#' @param data \code{forecast::forecast} instance
#' @return data.frame
#' @export
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
#' ggplot2::fortify(d.forecast)
fortify.forecast <- function(data) {
  forecasted <- forecast:::as.data.frame.forecast(data)
  forecasted$Index <- get.dtindex(data$mean)
  fitted <- data.frame(Index = get.dtindex(data$fitted),
                       Original = data$x,
                       Fitted = data$fitted)
  
  rownames(fitted) <- NULL
  rownames(forecasted) <- NULL
  
  d <- dplyr::rbind_list(fitted, forecasted)
  dplyr::tbl_df(d)
}

#' Autoplot \code{forecast::forecast}.
#' 
#' @param data \code{forecast::forecast} instance
#' @param ts.colour Line colour for time-series
#' @param ts.linetype Line type for time-series
#' @param predict.colour Line colour for predicted time-series
#' @param predict.linetype Line type for predicted time-series
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' ggplot2::autoplot(forecast::forecast(d.arima, h = 50))
#' ggplot2::autoplot(forecast::forecast(d.arima, level = c(95), h = 50))
#' ggplot2::autoplot(forecast::forecast(d.arima, h = 50), conf.int = FALSE)
#' @export
autoplot.forecast <- function(data, 
                              ts.colour = '#000000', ts.linetype = 'solid',
                              predict.colour = '#0000FF', predict.linetype = 'solid',
                              conf.int = TRUE,
                              conf.int.fill = '#000000', conf.int.alpha = 0.3) {
  plot.data <- ggplot2::fortify(data)
  lower = '`Lo 95`'  # prioritize to use 95%
  upper = '`Hi 95`'
  
  if (! 'Lo 95' %in% names(plot.data)) {
    # escape by backquote
    lower <- paste0('`', names(plot.data)[5], '`')
  }
  if (! 'Hi 95' %in% names(plot.data)) {
    upper <- paste0('`', names(plot.data)[6], '`')
  }
  
  # Filter existing values to avoid warnings
  original.data <- dplyr::filter(plot.data, !is.na(Original))
  predict.data <- dplyr::filter(plot.data, !is.na(`Point Forecast`))
  
  p <- ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = Index)) +
    ggplot2::geom_line(data = original.data,
                       mapping = ggplot2::aes(y = Original),
                       colour = ts.colour, linetype = ts.linetype) +
    ggplot2::geom_line(data = predict.data,
                       mapping = ggplot2::aes(y = `Point Forecast`),
                       colour = predict.colour, linetype = predict.linetype)
  
  if (conf.int) {
    p <- p + ggplot2::geom_ribbon(data = predict.data,
                                  mapping = ggplot2::aes_string(ymin = lower, ymax = upper),
                                  fill = conf.int.fill, alpha = conf.int.alpha)
  }
  p
}