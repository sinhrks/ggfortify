#' Convert time-series-like to data.frame.
#'
#' @param model \code{stats::ts}, \code{timeSeries::timeSeries} or \code{tseries::irts} instance
#' @param data original dataset, if needed
#' @param columns Character vector specifies target column name(s)
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param index.name Specify column name for time series index
#' @param data.name Specify column name for univariate time series data. Ignored in multivariate time series.
#' @param scale Logical flag indicating whether to perform scaling each timeseries
#' @param melt Logical flag indicating whether to melt each timeseries as variable
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' fortify(AirPassengers)
#' fortify(timeSeries::as.timeSeries(AirPassengers))
#'
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' fortify(its)
#'
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
      ncomp <- ncol(ts.data)
      orig <- drop(ts.data %*% rep(1, ncol(ts.data)))

      dtindex <- get.dtindex(ts.data, is.date = is.date)
      d <- cbind(data.frame(Data = orig),
                 data.frame(model$time.series))
    } else if (is(model, 'decomposed.ts')) {
      dtindex <- get.dtindex(model$x, is.date = is.date)
      dtframe <- ggplot2::fortify(model$x)

      # for tbl_df
      # dtframe <- dtframe[, -1]
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
  post.fortify(d)
}

#' @export
fortify.timeSeries <- fortify.ts

#' @export
fortify.irts <- fortify.ts

#' Autoplot time-series-like.
#'
#' @param object time-series-like instance
#' @param columns Character vector specifies target column name(s)
#' @param group Character vector specifies grouping
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param index.name Specify column name for time series index when passing \code{data.frame} via data.
#' @param p \code{ggplot2::ggplot} instance
#' @param ts.scale Logical flag indicating whether to perform scaling each timeseries
#' @param scales Scale value passed to \code{ggplot2}
#' @param facets Logical value to specify use facets for multivariate time-series
#' @param facet (Deprecated) use facets
#' @param nrow Number of facet/subplot rows
#' @param ncol Number of facet/subplot columns
#' @param ts.geom Geometric string for time-series plotting. 'line', 'bar' or 'point'.
#' @param ts.colour Line colour for time-series
#' @param ts.linetype Line type for time-series
#' @param xlab Character vector or expression for x axis label
#' @param ylab Character vector or expression for y axis label
#' @param ... other arguments passed to methods
#' @return ggplot
#' @aliases autoplot.xts autoplot.timeSeries autoplot.irts autoplot.stl autoplot.decomposed.ts
#' @examples
#' data(Canada, package = 'vars')
#' autoplot(AirPassengers)
#' autoplot(UKgas, ts.geom = 'bar')
#' autoplot(Canada)
#' autoplot(Canada, columns = 'e', is.date = TRUE)
#' autoplot(Canada, facets = FALSE)
#'
#' library(zoo)
#' autoplot(xts::as.xts(AirPassengers))
#' autoplot(xts::as.xts(UKgas))
#' autoplot(xts::as.xts(Canada))
#'
#' autoplot(timeSeries::as.timeSeries(AirPassengers))
#' autoplot(timeSeries::as.timeSeries(Canada))
#'
#' its <- tseries::irts(cumsum(rexp(10, rate = 0.1)), matrix(rnorm(20), ncol=2))
#' autoplot(its)
#'
#' autoplot(stats::stl(UKgas, s.window = 'periodic'))
#' autoplot(stats::decompose(UKgas))
#' @export
autoplot.ts <- function(object, columns = NULL, group = NULL,
                        is.date = NULL, index.name = 'Index',
                        p = NULL,
                        ts.scale = FALSE, scales = 'free_y',
                        facet = TRUE, facets = facet,
                        nrow = NULL, ncol = 1,
                        ts.geom = 'line',
                        ts.colour = '#000000', ts.linetype = 'solid',
                        xlab = '', ylab = '', ...) {

  # deprecation
  if (! missing(facet)) {
    deprecate.warning('facet', 'facets')
    facets <- facet
  }

  # fortify data
  if (is.data.frame(object)) {
    plot.data <- object
  } else {
    plot.data <- ggplot2::fortify(object, scale = ts.scale,
                                  is.date = is.date, index.name = index.name)
  }

  if (is.null(columns)) {
    data.names <- names(plot.data)
    columns <- data.names[data.names != index.name]
  }
  if (length(columns) > 1) {
    .is.univariate <- FALSE
  } else {
    .is.univariate <- TRUE
  }
  plot.data <- tidyr::gather_(plot.data, 'variable', 'value', columns)

  # create ggplot instance if not passed
  if (is.null(p)) {
    null.p <- TRUE
    mapping = ggplot2::aes_string(x = index.name)
    p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  } else {
    null.p <- FALSE
  }

  if (ts.geom == 'line') {
    geomobj <- ggplot2::geom_line
  } else if (ts.geom == 'bar') {
    geomobj <- ggplot2::geom_bar
  } else if (ts.geom == 'point') {
    geomobj <- ggplot2::geom_point
  } else {
    stop("Invalid geom is specified. Use 'line' or 'bar'.")
  }

  # must be done here, because fortify.zoo is defined in zoo package
  ts.column <- plot.data[[index.name]]
  if (is(ts.column, 'yearmon') || is(ts.column, 'yearqtr')) {
    plot.data[[index.name]] <- zoo::as.Date(plot.data[[index.name]])
  }

  if (facets) {
    mapping <- ggplot2::aes_string(x = index.name, y = 'value',
                                   group = 'variable')
    p <- p + geomobj(data = plot.data, mapping = mapping,
                     colour = ts.colour, linetype = ts.linetype, stat = 'identity')
    if (!.is.univariate) {
      p <- p + ggplot2::facet_wrap(~ variable, scales = scales,
                                   nrow = nrow, ncol = ncol)
    }
  } else {
    # ts.colour cannot be used
    mapping <- ggplot2::aes_string(x = index.name, y = 'value',
                                   colour = 'variable')
    p <- p + geomobj(data = plot.data, mapping = mapping,
                     linetype = ts.linetype, stat = 'identity')
  }
  if (null.p) {
    p <- p +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
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
#' @param model Time series model instance
#' @param data original dataset, needed for \code{stats::ar}, \code{stats::Arima}
#' @param original (Deprecated) use data
#' @param predict Predicted \code{stats::ts}
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param ... other arguments passed to methods
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
fortify.tsmodel <- function(model, data = NULL, original = NULL,
                            predict = NULL,
                            is.date = NULL,
                            ts.connect = TRUE, ...) {

  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }

  if (is(model, 'Arima') || is(model, 'ar')) {
    if (is.null(data)) {
      library(forecast)
      data <- forecast::getResponse(model)
      fit <- fitted(model)
    } else {
      fit <- data - residuals(model)
    }
    d <- ggplot2::fortify(data, is.date = is.date)
    fit <- ggplot2::fortify(fit, data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(residuals(model), data.name = 'Residuals', is.date = is.date)

    if (!is.null(predict)) {
      pred <- ggplot2::fortify(predict$pred, data.name = 'Predicted')
      se <- as.vector(predict$se)
      pred$lower <- pred$Predicted - se
      pred$upper <- pred$Predicted + se
    }
  } else if (is(model, 'HoltWinters')) {
    # same as fracdiff and nnetar
    d <- ggplot2::fortify(model$x, is.date = is.date)
    fit <- ggplot2::fortify(fitted(model), data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(residuals(model), data.name = 'Residuals', is.date = is.date)

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
    fit <- ggplot2::fortify(fitted(model), data.name = 'Fitted', is.date = is.date)
    resid <- ggplot2::fortify(residuals(model), data.name = 'Residuals', is.date = is.date)
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
    n <- nrow(d)
    d <- rbind_ts(pred, d, ts.connect = ts.connect)
  }
  post.fortify(d)
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

#' Autoplot time series models (like AR, ARIMA).
#'
#' @param object Time series model instance
#' @param data original dataset, needed for \code{stats::ar}, \code{stats::Arima}
#' @param original (Deprecated) use data
#' @param predict Predicted \code{stats::ts}
#' If not provided, try to retrieve from current environment using variable name.
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param fitted.colour Line colour for fitted time-series
#' @param fitted.linetype Line type for fitted time-series
#' @param predict.colour Line colour for predicted time-series
#' @param predict.linetype Line type for predicted time-series
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @param ... Keywords passed to autoplot.ts
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
#'
#' form <- function(theta){
#'   dlm::dlmModPoly(order=1, dV=exp(theta[1]), dW=exp(theta[2]))
#' }
#' model <- form(dlm::dlmMLE(Nile, parm=c(1, 1), form)$par)
#' filtered <- dlm::dlmFilter(Nile, model)
#' autoplot(filtered)
#' autoplot(dlm::dlmSmooth(filtered))
#' @export
autoplot.tsmodel <- function(object, data = NULL, original = NULL,
                             predict = NULL,
                             is.date = NULL, ts.connect = TRUE,
                             fitted.colour = '#FF0000', fitted.linetype = 'solid',
                             predict.colour = '#0000FF', predict.linetype = 'solid',
                             conf.int = TRUE,
                             conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             ...) {

  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }

  fcol <- ifelse(is(object, 'HoltWinters'), 'xhat', 'Fitted')
  plot.data <- ggplot2::fortify(object, predict = predict,
                                data = data, is.date = is.date)
  p <- autoplot.ts(plot.data, columns = 'Data', ...)
  p <- autoplot.ts(plot.data, columns = fcol, p = p,
                   ts.colour = fitted.colour,
                   ts.linetype = fitted.linetype)
  if (!is.null(predict)) {
    predict.data <- dplyr::filter_(plot.data, '!is.na(Predicted)')
    p <- autoplot.ts(predict.data, columns = 'Predicted', p = p,
                     ts.colour = predict.colour,
                     ts.linetype = predict.linetype)
    p <- plot.conf.int(p, data = predict.data,
                       conf.int = conf.int,
                       conf.int.colour = conf.int.colour,
                       conf.int.linetype = conf.int.linetype,
                       conf.int.fill = conf.int.fill,
                       conf.int.alpha = conf.int.alpha)
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
