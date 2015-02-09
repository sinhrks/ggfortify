#' Convert \code{forecast::forecast} to data.frame.
#'
#' @param model \code{forecast::forecast} instance
#' @param data original dataset, if needed
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param ... other arguments passed to methods
#' @return data.frame
#' @export
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
#' fortify(d.forecast)
#' fortify(d.forecast, ts.connect = TRUE)
fortify.forecast <- function(model, data = NULL, is.date = NULL,
                             ts.connect = FALSE, ...) {
  forecasted <- as.data.frame(model)
  forecasted$Index <- get.dtindex(model$mean, is.date = is.date)

  d <- ggplot2::fortify(model$x, is.date = is.date)
  fitted <- ggplot2::fortify(model$fitted, data.name = 'Fitted', is.date = is.date)
  d <- dplyr::left_join(d, fitted, by = 'Index')
  d <- ggfortify::rbind_ts(forecasted, d, ts.connect = ts.connect)
  post.fortify(d)
}

#' Autoplot \code{forecast::forecast}.
#'
#' @param object \code{forecast::forecast} instance
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param predict.colour Line colour for predicted time-series
#' @param predict.linetype Line type for predicted time-series
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @examples
#' d.arima <- forecast::auto.arima(AirPassengers)
#' autoplot(forecast::forecast(d.arima, h = 10))
#' autoplot(forecast::forecast(d.arima, level = c(85), h = 10))
#' autoplot(forecast::forecast(d.arima, h = 5), conf.int = FALSE)
#' autoplot(forecast::forecast(d.arima, h = 10), is.date = FALSE)
#' autoplot(forecast::forecast(forecast::ets(UKgas), h = 5))
#' autoplot(forecast::forecast(stats::HoltWinters(UKgas), h = 10))
#' @export
autoplot.forecast <- function(object, is.date = NULL, ts.connect = TRUE,
                              predict.colour = '#0000FF', predict.linetype = 'solid',
                              conf.int = TRUE,
                              conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                              conf.int.fill = '#000000', conf.int.alpha = 0.3,
                              ...) {
  plot.data <- ggplot2::fortify(object, is.date = is.date, ts.connect = ts.connect)
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

  p <- autoplot.ts(original.data, columns = 'Data', ...)
  p <- autoplot.ts(predict.data, columns = 'Point Forecast', p = p,
                   ts.colour = predict.colour, ts.linetype = predict.linetype)
  p <- plot.conf.int(p, data = predict.data, lower = lower,
                     upper = upper, conf.int = conf.int,
                     conf.int.colour = conf.int.colour,
                     conf.int.linetype = conf.int.linetype,
                     conf.int.fill = conf.int.fill,
                     conf.int.alpha = conf.int.alpha)
  p
}

#' Convert \code{forecast::bats} and \code{forecast::ets} to data.frame.
#'
#' @param model \code{forecast::bats} or \code{forecast::ets} instance
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' fortify(forecast::bats(UKgas))
#' fortify(forecast::ets(UKgas))
#' @export
fortify.ets <- function(model, data = NULL, ...) {
  if (is(model, 'ets')) {
    d <- ggplot2::fortify(model$x)
    resid <- ggplot2::fortify(model$residuals, data.name = 'Residuals')
    fitted <- ggplot2::fortify(model$fitted, data.name = 'Fitted')
    d <- dplyr::left_join(d, fitted, by = 'Index')
    d <- dplyr::left_join(d, resid, by = 'Index')

    level <- ggplot2::fortify(model$states[, 'l'], data.name = 'Level')
    d <- dplyr::left_join(d, level, by = 'Index')
    if ('b' %in% colnames(model$states)) {
      slope <- ggplot2::fortify(model$states[, 'b'], data.name = 'Slope')
      d <- dplyr::left_join(d, slope, by = 'Index')
    }
    if ('s1' %in% colnames(model$states)) {
      season <- ggplot2::fortify(model$states[, 's1'], data.name = 'Season')
      d <- dplyr::left_join(d, season, by = 'Index')
    }
  } else if (is(model, 'bats')) {
    if (!is.null(model$lambda))
      y <- forecast::BoxCox(model$y, model$lambda)
    else y <- model$y
    d <- ggplot2::fortify(y)
    resid <- ggplot2::fortify(model$errors, data.name = 'Residuals')
    fitted <- ggplot2::fortify(model$fitted.values, data.name = 'Fitted')
    d <- dplyr::left_join(d, fitted, by = 'Index')
    d <- dplyr::left_join(d, resid, by = 'Index')

    d <- cbind(d, Level = model$x[1, ])
    if (!is.null(model$beta))
      d <- cbind(d, Slope = model$x[2, ])

    nonseas <- 2 + (!is.null(model$beta))
    nseas <- length(model$gamma.values)
    if (!is.null(model$gamma)) {
      seas.states <- model$x[-(1:(1 + (!is.null(model$beta)))), ]
      j <- cumsum(c(1, model$seasonal.periods))
      for (i in 1:nseas) d <- cbind(d, Season = seas.states[j[i], ])
      if (nseas > 1)
        colnames(d)[nonseas + 1:nseas] <- paste0("Season", 1:nseas)
    }
  } else {
    stop(paste0('Unsupported class for fortify.ets: ', class(model)))
  }
  post.fortify(d)
}

#' @export
fortify.bats <- fortify.ets

#' Autoplot \code{forecast::bats} and \code{forecast::ets}
#'
#' @param object \code{forecast::bats} and \code{forecast::ets}  instance
#' @param columns Character vector specifies target column name(s)
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @aliases autoplot.bats
#' @examples
#' d.bats <- forecast::bats(UKgas)
#' autoplot(d.bats)
#' autoplot(d.bats, columns = 'Residuals')
#' autoplot(forecast::ets(UKgas))
#' @export
autoplot.ets <- function(object, columns = NULL, ...) {
  plot.data <- ggplot2::fortify(object)
  if (is.null(columns)) {
    columns <- c('Data', 'Level', 'Slope', 'Season')
    # Slope and Season can be optionals
    columns <- columns[columns %in% names(plot.data)]
  }
  p <- autoplot.ts(plot.data, columns = columns, ...)
  p
}

#' @export
autoplot.bats <- autoplot.ets
