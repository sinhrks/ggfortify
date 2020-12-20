#' Convert time-series-like to data.frame
#'
#' @param model time-series-like instance
#' @inheritParams fortify_base
#' @param columns character vector specifies target column name(s)
#' @param is.date logical frag indicates whether the \code{stats::ts} is date or not
#' If not provided, regard the input as date when the frequency is 4 or 12
#' @param index.name specify column name for time series index
#' @param data.name specify column name for univariate time series data. Ignored in multivariate time series.
#' @param scale logical flag indicating whether to perform scaling each timeseries
#' @param melt logical flag indicating whether to melt each timeseries as variable
#' @return data.frame
#' @examples
#' fortify(AirPassengers)
#' fortify(timeSeries::as.timeSeries(AirPassengers))
#' fortify(tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2)))
#' fortify(stats::stl(UKgas, s.window = 'periodic'))
#' fortify(stats::decompose(UKgas))
#' @export
fortify.ts <- function(model, data = NULL, columns = NULL, is.date = NULL,
                       index.name = 'Index', data.name = 'Data',
                       scale = FALSE, melt = FALSE, ...) {
  # no need to define `fortify.xts` because zoo package has `fortify.zoo`
  if (is(model, 'timeSeries')) {
    d <- as.data.frame(model)
    dtindex <- as.POSIXct(rownames(d))
  } else if (is(model, 'irts')) {
    d <- as.data.frame(model$value)
    dtindex <- model$time
  } else if (is(model, 'ts')) {
    d <- as.data.frame(as.matrix(model))
    dtindex <- get.dtindex(model, is.date = is.date)
  } else if (is(model, 'stl')) {
      # stl allows only univariate series
      ts.data <- model$time.series
      orig <- drop(ts.data %*% rep(1, ncol(ts.data)))

      dtindex <- get.dtindex(ts.data, is.date = is.date)
      d <- cbind(data.frame(Data = orig),
                 data.frame(model$time.series))
    } else if (is(model, 'decomposed.ts')) {
      dtindex <- get.dtindex(model$x, is.date = is.date)
      dtframe <- ggplot2::fortify(model$x)

      # for tbl_df
      dtframe <- data.frame(Data = dtframe[['Data']])

      # trend and random can be multivariate
      rndframe <- model$random
      colnames(rndframe) <- NULL
      dcframe <- data.frame(seasonal = model$seasonal,
                            trend = model$trend,
                            remainder = rndframe)
      d <- cbind(dtframe, dcframe)
  } else {
    stop(paste0('Unsupported class for fortify.ts: ', class(model)))
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

  # scaling
  if (scale) {
    for (col in columns) {
      d[[col]] <- base::scale(d[[col]], center = TRUE, scale = TRUE)
    }
  }

  # unpivot
  if (melt) {
    d <- tidyr::gather_(d, 'variable', 'value', columns)
  }
  post_fortify(d)
}

#' @export
fortify.timeSeries <- fortify.ts

#' @export
fortify.irts <- fortify.ts

#' Autoplot time-series-like
#'
#' @param object time-series-like instance
#' @param columns Character vector specifies target column name(s)
#' @param group Character vector specifies grouping
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not
#' If not provided, regard the input as date when the frequency is 4 or 12
#' @param index.name Specify column name for time series index when passing \code{data.frame} via data.
#' @param p \code{ggplot2::ggplot} instance
#' @param ts.scale Logical flag indicating whether to perform scaling each timeseries
#' @param stacked Logical flag indicating whether to stack multivariate timeseries
#' @inheritParams apply_facets
#' @param ts.geom geometric string for time-series. 'line', 'bar', 'ribbon', or 'point'
#' @param ts.colour line colour for time-series
#' @param ts.size point size for time-series
#' @param ts.linetype line type for time-series
#' @param ts.alpha alpha for time-series
#' @param ts.fill fill colour for time-series
#' @param ts.shape point shape for time-series
#' @param geom same as ts.geom
#' @param colour same as ts.colour
#' @param size same as ts.size
#' @param linetype same as ts.linetype
#' @param alpha same as ts.alpha
#' @param fill same as ts.fill
#' @param shape same as ts.shape
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @aliases autoplot.xts autoplot.timeSeries autoplot.irts autoplot.stl autoplot.decomposed.ts
#' @examples
#' \dontrun{
#' data(Canada, package = 'vars')
#' autoplot(AirPassengers)
#' autoplot(UKgas, ts.geom = 'bar')
#' autoplot(Canada)
#' autoplot(Canada, facets = FALSE)
#'
#' library(zoo)
#' autoplot(xts::as.xts(AirPassengers))
#' autoplot(timeSeries::as.timeSeries(AirPassengers))
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' autoplot(its)
#'
#' autoplot(stats::stl(UKgas, s.window = 'periodic'))
#' autoplot(stats::decompose(UKgas))
#' }
#' @export
autoplot.ts <- function(object, columns = NULL, group = NULL,
                        is.date = NULL, index.name = 'Index',
                        p = NULL, ts.scale = FALSE, stacked = FALSE,
                        facets = TRUE, nrow = NULL, ncol = 1, scales = 'free_y',
                        ts.geom = 'line', ts.colour = NULL, ts.size = NULL, ts.linetype = NULL,
                        ts.alpha = NULL, ts.fill = NULL, ts.shape = NULL,
                        geom = ts.geom, colour = ts.colour, size = ts.size, linetype = ts.linetype,
                        alpha = ts.alpha, fill = ts.fill, shape = ts.shape,
                        xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                        main = NULL, xlab = '', ylab = '', asp = NULL,
                        ...) {
  geomfunc <- get_geom_function(geom, allowed = c('line', 'bar', 'point', 'ribbon'))

  # fortify data
  if (is.data.frame(object)) {
    plot.data <- object
  } else {
    plot.data <- ggplot2::fortify(object, scale = ts.scale,
                                  is.date = is.date, index.name = index.name)
  }

  if (is.null(columns)) {
    if (is(object, 'bats') || is(object, 'ets')) {
      # for forecast::bats and forecast::ets
      columns <- c('Data', 'Level', 'Slope', 'Season')
      # Slope and Season can be optionals
      columns <- columns[columns %in% names(plot.data)]
    } else {
      data.names <- names(plot.data)
      columns <- data.names[data.names != index.name]
    }
  }

  if (is.null(colour) && !is.null(fill)) {
    colour <- fill
  } else if (!is.null(colour) && is.null(fill)) {
    if (geom %in% c('bar', 'ribbon')) {
      # do not set for line / point to handle as NULL in geom_factory
      fill <- colour
    }
  }

  if (length(columns) > 1) {
    .is.univariate <- FALSE
  } else {
    .is.univariate <- TRUE
    facets <- FALSE
    stacked <- FALSE
  }
  # required for shift op
  tslen <- nrow(plot.data)

  if (!facets && stacked && geom != 'bar') {
    for (i in seq_len(length(columns) - 1)) {
      plot.data[columns[i + 1]] <- plot.data[columns[i + 1]] + plot.data[columns[i]]
    }
  }
  # must be done here, because fortify.zoo is defined in zoo package
  ts.column <- plot.data[[index.name]]
  if (is(ts.column, 'yearmon') || is(ts.column, 'yearqtr')) {
    plot.data[[index.name]] <- zoo::as.Date(plot.data[[index.name]])
  }

  group_key <- 'plot_group'  # gets used later. Don't know why its defined here.
  plot.data <- tidyr::pivot_longer(plot.data, names_to=group_key, values_to='value', columns) %>%
    arrange(plot_group)      # somewhere later the sort order matters.

  # create ggplot instance if not passed
  if (is.null(p)) {
    null.p <- TRUE
    mapping <- ggplot2::aes_string(x = index.name)
    p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  } else {
    null.p <- FALSE
  }

  if (!facets && stacked) {
    # using dplyr::lag may be easier, but it likely to
    # cause a trouble in CMD check
    value <- plot.data$value
    shifted <- c(rep(0, times = tslen), value[1:(length(value) - tslen)])
    plot.data[['base']] <- shifted
  } else {
    plot.data[['base']] <- 0
  }

  args <- list(geomfunc, plot.data, colour = colour, size = size,
               linetype = linetype, alpha = alpha, fill = fill,
               shape = shape, stat = 'identity')
  if (geom == 'ribbon') {
    args['ymin'] <- 'base'
    args['ymax'] <- 'value'
  } else {
    args['y'] <- 'value'
  }

  if (facets) {
    args['group'] <- group_key
    p <- p + do.call(geom_factory, args)
    p <- apply_facets(p, ~ plot_group, nrow = nrow, ncol = ncol, scales = scales)
  } else {
    if (!.is.univariate) {
      # ts.colour cannot be used
      if (!is.null(colour)) {
        warning('multivariate timeseries with facets=FALSE are colorized by variable, colour is ignored')
      }
      args['colour'] <- group_key
      if (geom %in% c('bar', 'ribbon')) {
        args['fill'] <- group_key
      }
      if (geom == 'ribbon' && !stacked && is.null(alpha)) {
        args['alpha'] <- 0.5
      }
      if (geom == 'bar' && !stacked) {
        args['position'] <- 'dodge'
      }
    }
    p <- p + do.call(geom_factory, args)
  }
  if (null.p) {
    p <- p + ggplot2::scale_y_continuous()
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
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

#' Convert time series models (like AR, ARIMA) to \code{data.frame}
#'
#' @param model Time series model instance
#' @param data original dataset, needed for \code{stats::ar}, \code{stats::Arima}
#' @inheritParams fortify_base
#' @param predict Predicted \code{stats::ts}
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @return data.frame
#' @aliases fortify.ar fortify.Arima fortify.fracdiff
#' fortify.nnetar fortify.HoltWinters fortify.fGARCH
#' @examples
#' fortify(stats::ar(AirPassengers))
#' fortify(stats::arima(UKgas))
#' fortify(stats::arima(UKgas), data = UKgas, is.date = TRUE)
#' fortify(forecast::auto.arima(austres))
#' fortify(forecast::arfima(AirPassengers))
#' fortify(forecast::nnetar(UKgas))
#' fortify(stats::HoltWinters(USAccDeaths))
#'
#' data(LPP2005REC, package = 'timeSeries')
#' x = timeSeries::as.timeSeries(LPP2005REC)
#' d.Garch = fGarch::garchFit(LPP40 ~ garch(1, 1), data = 100 * x, trace = FALSE)
#' fortify(d.Garch)
fortify.tsmodel <- function(model, data = NULL,
                            predict = NULL,
                            is.date = NULL,
                            ts.connect = TRUE, ...) {

  if (is(model, 'Arima') || is(model, 'ar')) {
    if (is.null(data)) {
      data <- forecast::getResponse(model)
      fit <- stats::fitted(model)
    } else {
      fit <- data - stats::residuals(model)
    }
    d <- ggplot2::fortify(data, is.date = is.date)
    fit <- ggplot2::fortify(fit, data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(stats::residuals(model), data.name = 'Residuals', is.date = is.date)

    if (!is.null(predict)) {
      pred <- ggplot2::fortify(predict$pred, data.name = 'Predicted')
      se <- as.vector(predict$se)
      pred$lower <- pred$Predicted - se
      pred$upper <- pred$Predicted + se
    }
  } else if (is(model, 'HoltWinters')) {
    # same as fracdiff and nnetar
    d <- ggplot2::fortify(model$x, is.date = is.date)
    fit <- ggplot2::fortify(stats::fitted(model), data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(stats::residuals(model), data.name = 'Residuals', is.date = is.date)

    if (!is.null(predict)) {
      pred <- ggplot2::fortify(predict)
      if (! 'upr' %in% names(pred)) {
        pred$upr <- pred$Data
        pred$lwr <- pred$Data
      }
      colnames(pred) <- c('Index', 'Predicted', 'upper', 'lower')
    }
  } else if (is(model, 'fracdiff') || is(model, 'nnetar')) {
    d <- ggplot2::fortify(model$x, is.date = is.date)
    fit <- ggplot2::fortify(stats::fitted(model), data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(stats::residuals(model), data.name = 'Residuals', is.date = is.date)
  } else if (is(model, 'fGARCH')) {
    index <- attr(model@data, 'names')
    index <- as.vector(index)
    d <- data.frame(Index = index, Data = model@data)
    fit <- data.frame(Index = index, Fitted = model@fitted)
    resid <- data.frame(Index = index, Residuals = model@residuals)

    if (!is.null(predict)) {
      pred <- data.frame(Predicted = predict$meanForecast)
      pred$lower <- pred$Predicted - predict$meanError
      pred$upper <- pred$Predicted + predict$meanError
    }
  } else if (is(model, 'dlmFiltered')) {
    d <- ggplot2::fortify(model$y, is.date = is.date)
    m <- dlm::dropFirst(model$m)
    if (!is.univariate(m, raise = FALSE)) {
      m <- m[, 1]
    }
    fit <- ggplot2::fortify(m, data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(model$y - m, data.name = 'Residuals', is.date = is.date)
  } else if (is(model, 'KFS')) {
    d <- ggplot2::fortify(model$model$y, is.date = is.date)
    m <- model$alphahat
    if (is.null(m)) {
      m <- model$m
      if (is.null(m)) {
        stop('Object does not contain smoothed estimates of states.')
      }
      m[1] <- model$model$y[1]
    }
    fit <- ggplot2::fortify(m, data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(model$model$y - m,
                              data.name = 'Residuals', is.date = is.date)
  } else {
    stop(paste0('Unsupported class for fortify.Arima: ', class(model)))
  }
  d <- dplyr::left_join(d, fit, by = 'Index')
  d <- dplyr::left_join(d, resid, by = 'Index')
  if (!is.null(predict)) {
    d <- rbind_ts(pred, d, ts.connect = ts.connect)
  }
  post_fortify(d)
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

#' @export
fortify.dlmFiltered <- fortify.tsmodel

#' @export
fortify.KFS <- fortify.tsmodel

#' Autoplot time series models (like AR, ARIMA)
#'
#' @param object Time series model instance
#' @param data original dataset, needed for \code{stats::ar}, \code{stats::Arima}
#' @param predict Predicted \code{stats::ts}
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param fitted.geom geometric string for fitted time-series
#' @param fitted.colour line colour for fitted time-series
#' @param fitted.size point size for fitted time-series
#' @param fitted.linetype line type for fitted time-series
#' @param fitted.alpha alpha for fitted time-series
#' @param fitted.fill fill colour for fitted time-series
#' @param fitted.shape point shape for fitted time-series
#' @param predict.geom geometric string for predicted time-series
#' @param predict.colour line colour for predicted time-series
#' @param predict.size point size for predicted time-series
#' @param predict.linetype line type for predicted time-series
#' @param predict.alpha alpha for predicted time-series
#' @param predict.fill fill colour for predicted time-series
#' @param predict.shape point shape for predicted time-series
#' @inheritParams plot_confint
#' @param ... Keywords passed to \code{autoplot.ts}
#' @return ggplot
#' @aliases autoplot.ar autoplot.fracdiff autoplot.nnetar autoplot.HoltWinters autoplot.fGARCH
#' @examples
#' d.ar <- stats::ar(AirPassengers)
#' autoplot(d.ar)
#' autoplot(d.ar, predict = predict(d.ar, n.ahead = 5))
#' autoplot(stats::arima(UKgas), data = UKgas)
#' autoplot(forecast::arfima(AirPassengers))
#' autoplot(forecast::nnetar(UKgas), is.date = FALSE)
#'
#' d.holt <- stats::HoltWinters(USAccDeaths)
#' autoplot(d.holt)
#' autoplot(d.holt, predict = predict(d.holt, n.ahead = 5))
#' autoplot(d.holt, predict = predict(d.holt, n.ahead = 5, prediction.interval = TRUE))
#' @export
autoplot.tsmodel <- function(object, data = NULL,
                             predict = NULL,
                             is.date = NULL, ts.connect = TRUE,
                             fitted.geom = 'line',
                             fitted.colour = '#FF0000', fitted.size = NULL,
                             fitted.linetype = NULL, fitted.alpha = NULL,
                             fitted.fill = NULL, fitted.shape = NULL,
                             predict.geom = 'line',
                             predict.colour = '#0000FF', predict.size = NULL,
                             predict.linetype = NULL, predict.alpha = NULL,
                             predict.fill = NULL, predict.shape = NULL,
                             conf.int = TRUE,
                             conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             ...) {
  fcol <- ifelse(is(object, 'HoltWinters'), 'xhat', 'Fitted')
  plot.data <- ggplot2::fortify(object, predict = predict,
                                data = data, is.date = is.date)
  p <- autoplot.ts(plot.data, columns = 'Data', ...)

  # must be passed by ts.<option>
  p <- autoplot.ts(plot.data, columns = fcol, p = p,
                   ts.geom = fitted.geom,
                   ts.colour = fitted.colour, ts.size = fitted.size,
                   ts.linetype = fitted.linetype, ts.alpha = fitted.alpha,
                   ts.fill = fitted.fill, ts.shape = fitted.shape)
  if (!is.null(predict)) {
    predict.data <- dplyr::filter_(plot.data, '!is.na(Predicted)')
    p <- autoplot.ts(predict.data, columns = 'Predicted', p = p,
                     ts.geom = predict.geom,
                     ts.colour = predict.colour, ts.size = predict.size,
                     ts.linetype = predict.linetype, ts.alpha = predict.alpha,
                     ts.fill = predict.fill, ts.shape = predict.shape)
    p <- plot_confint(p = p, data = predict.data, conf.int = conf.int,
                      conf.int.colour = conf.int.colour,
                      conf.int.linetype = conf.int.linetype,
                      conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  }
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
autoplot.fGARCH <- autoplot.tsmodel

#' @export
autoplot.dlmFiltered <- autoplot.tsmodel

#' @export
autoplot.KFS <- autoplot.tsmodel
