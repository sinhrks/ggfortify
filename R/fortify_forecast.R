#' Convert \code{forecast::forecast} to data.frame.
#' 
#' @param forecast.data \code{forecast::forecast} instance
#' @return data.frame
#' @export
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
#' ggplot2::fortify(d.forecast)
fortify.forecast <- function(forecast.data) {
  forecasted <- as.data.frame(forecast.data)
  forecasted$Time <- zoo::as.Date(time(forecast.data$mean))
  fitted <- data.frame(Time = zoo::as.Date(time(forecast.data$fitted)),
                       Original = forecast.data$x,
                       Fitted = forecast.data$fitted)
  
  rownames(fitted) <- NULL
  rownames(forecasted) <- NULL
  
  dplyr::rbind_list(fitted, forecasted)
}

#' Autoplot \code{forecast::forecast}.
#' 
#' @param forecast.data \code{forecast::forecast} instance
#' @return data.frame
#' @export
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
#' ggplot2::autoplot(d.forecast)
autoplot.forecast <- function(forecast.data) {
  plot.data <- ggplot2::fortify(forecast.data)
  lower = '`Lo 95`'  # prioritize to use 95%
  upper = '`Hi 95`'
  
  if (! 'Lo 95' %in% names(plot.data)) {
    # escape by backquote
    lower <- paste0('`', names(plot.data)[5], '`')
  }
  if (! 'Hi 95' %in% names(plot.data)) {
    upper <- paste0('`', names(plot.data)[6], '`')
  }
  
  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = Time)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = Original)) +
    ggplot2::geom_line(mapping = ggplot2::aes_string(y = '`Point Forecast`'), colour='blue') +
    ggplot2::geom_ribbon(mapping = ggplot2::aes_string(ymin = lower, ymax = upper),
                         alpha = 0.5)
}