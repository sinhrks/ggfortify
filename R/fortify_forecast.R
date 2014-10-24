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
  d <- ggplot2::fortify(data$x)
  fitted <- ggplot2::fortify(data$fitted, data.name = 'Fitted')
  d <- dplyr::left_join(d, fitted, by = 'Index')
  rownames(d) <- NULL
  rownames(forecasted) <- NULL
  
  d <- dplyr::rbind_list(d, forecasted)
  dplyr::tbl_df(d)
}

#' Autoplot \code{forecast::forecast}.
#' 
#' @param data \code{forecast::forecast} instance
#' @param predict.colour Line colour for predicted time-series
#' @param predict.linetype Line type for predicted time-series
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' ggplot2::autoplot(forecast::forecast(d.arima, h = 10))
#' ggplot2::autoplot(forecast::forecast(d.arima, level = c(85), h = 10))
#' ggplot2::autoplot(forecast::forecast(d.arima, h = 5), conf.int = FALSE)
#' ggplot2::autoplot(forecast::forecast(forecast::ets(UKgas), h = 5))
#' ggplot2::autoplot(forecast::forecast(stats::HoltWinters(UKgas), h = 10))
#' @export
autoplot.forecast <- function(data, 
                              predict.colour = '#0000FF', predict.linetype = 'solid',
                              conf.int = TRUE,
                              conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                              conf.int.fill = '#000000', conf.int.alpha = 0.3,
                              ...) {
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
  original.data <- dplyr::filter(plot.data, !is.na(Data))
  predict.data <- dplyr::filter(plot.data, !is.na(`Point Forecast`))
  
  p <- ggfortify:::autoplot.ts(original.data, columns = 'Data', ...)
  p <- p + ggplot2::geom_line(data = predict.data,
                              mapping = ggplot2::aes_string(y = '`Point Forecast`'),
                              colour = predict.colour, linetype = predict.linetype)
  p <- ggfortify:::plot.conf.int(p, data = predict.data, 
                                 lower = lower, upper = upper,
                                 conf.int = conf.int,
                                 conf.int.colour = conf.int.colour,
                                 conf.int.linetype = conf.int.linetype,
                                 conf.int.fill = conf.int.fill,
                                 conf.int.alpha = conf.int.alpha)
  p
}

#' Convert \code{forecast::bats} and \code{forecast::ets} to data.frame.
#' 
#' @param data \code{forecast::bats} or \code{forecast::ets} instance
#' @return data.frame
#' @aliases fortify.ar fortify.fracdiff
#' @examples
#' ggplot2::fortify(forecast::bats(UKgas))
#' ggplot2::fortify(forecast::ets(UKgas))
#' @export
fortify.ets <- function(data) {
  if (is(data, 'ets')) {
    d <- ggplot2::fortify(data$x)
    resid <- ggplot2::fortify(data$residuals, data.name = 'Residuals')
    fitted <- ggplot2::fortify(data$fitted, data.name = 'Fitted')
    d <- dplyr::left_join(d, fitted, by = 'Index')
    d <- dplyr::left_join(d, resid, by = 'Index')

    level <- ggplot2::fortify(data$states[, 'l'], data.name = 'Level')
    d <- dplyr::left_join(d, level, by = 'Index')
    if ('b' %in% colnames(data$states)) {
      slope <- ggplot2::fortify(data$states[, 'b'], data.name = 'Slope')
      d <- dplyr::left_join(d, slope, by = 'Index')
    }
    if ('s1' %in% colnames(data$states)) {
      season <- ggplot2::fortify(data$states[, 's1'], data.name = 'Season')
      d <- dplyr::left_join(d, season, by = 'Index')
    }
  } else if (is(data, 'bats')) {
    if (!is.null(data$lambda)) 
      y <- forecast::BoxCox(data$y, data$lambda)
    else y <- data$y
    d <- ggplot2::fortify(y)
    resid <- ggplot2::fortify(data$errors, data.name = 'Residuals')
    fitted <- ggplot2::fortify(data$fitted.values, data.name = 'Fitted')
    d <- dplyr::left_join(d, fitted, by = 'Index')
    d <- dplyr::left_join(d, resid, by = 'Index')
    
    d <- cbind(d, Level = data$x[1, ])
    if (!is.null(data$beta)) 
      d <- cbind(d, Slope = data$x[2, ])
    
    nonseas <- 2 + (!is.null(data$beta))
    nseas <- length(data$gamma.values)
    if (!is.null(data$gamma)) {
      seas.states <- data$x[-(1:(1 + (!is.null(data$beta)))), ]
      j <- cumsum(c(1, data$seasonal.periods))
      for (i in 1:nseas) d <- cbind(d, Season = seas.states[j[i], ])
      if (nseas > 1) 
        colnames(d)[nonseas + 1:nseas] <- paste0("Season", 1:nseas)
    }
  } else {
    stop(paste0('Unsupported class for fortify.ets: ', class(data)))
  }
  dplyr::tbl_df(d)
}

#' @export
fortify.bats <- fortify.ets

#' Autoplot \code{forecast::bats} and \code{forecast::ets} 
#' 
#' @param data \code{forecast::bats} and \code{forecast::ets}  instance
#' @param columns Character vector specifies target column name(s)
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @aliases autoplot.bats
#' @examples
#' d.bats <- forecast::bats(UKgas)
#' ggplot2::autoplot(d.bats)
#' ggplot2::autoplot(d.bats, columns = 'Residuals')
#' ggplot2::autoplot(forecast::ets(UKgas))
#' @export
autoplot.ets <- function(data, columns = NULL, ...) {
  plot.data <- ggplot2::fortify(data)
  if (is.null(columns)) {
    columns <- c('Data', 'Level', 'Slope', 'Season')
    # Slope and Season can be optionals
    columns <- columns[columns %in% names(plot.data)]
  }
  p <- ggfortify:::autoplot.ts(plot.data, columns = columns, ...)
  p
}

#' @export
autoplot.bats <- autoplot.ets