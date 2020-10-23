#' Convert \code{vars::varprd} to \code{data.frame}
#'
#' @param model \code{vars::varprd} instance
#' @inheritParams fortify_base
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param melt Logical flag indicating whether to melt each timeseries as variable
#' @return data.frame
#' @examples
#' data(Canada, package = 'vars')
#' d.var <- vars::VAR(Canada, p = 3, type = 'const')
#' fortify(stats::predict(d.var, n.ahead = 50))
#' @export
fortify.varprd <- function(model, data = NULL, is.date = NULL,
                           ts.connect = FALSE, melt = FALSE, ...){
  fitted <- ggplot2::fortify(model$model$y)

  fcst <- model$fcst
  dtindex.cont <- get.dtindex.continuous(model$model$y, length = nrow(fcst[[1]]),
                                         is.date = is.date)
  cols <- names(fcst)

  if (melt) {
    # for autoplot conversion
    for (col in cols){
      pred <- data.frame(fcst[[col]])
      pred$Index <- dtindex.cont
      obs <- fitted[, c('Index', col)]
      colnames(obs) <- c('Index', 'Data')
      binded <- ggfortify::rbind_ts(pred, obs, ts.connect = ts.connect)
      binded$variable <- col
      fcst[[col]] <- binded
    }
    return(dplyr::bind_rows(fcst))
  } else {
    for (col in cols){
      colnames(fcst[[col]]) <- paste0(col, '.', colnames(fcst[[col]]))
    }
    pred <- data.frame(do.call(cbind, fcst))
    pred$Index <- dtindex.cont
    binded <- ggfortify::rbind_ts(pred, fitted, ts.connect = ts.connect)
    return(binded)
  }
}

#' Autoplot \code{vars::varprd}
#'
#' @param object \code{vars::varpred} instance
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param scales Scale value passed to \code{ggplot2}
#' @inheritParams autoplot.tsmodel
#' @inheritParams plot_confint
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @examples
#' data(Canada, package = 'vars')
#' d.var <- vars::VAR(Canada, p = 3, type = 'const')
#' autoplot(stats::predict(d.var, n.ahead = 50), is.date = TRUE)
#' autoplot(stats::predict(d.var, n.ahead = 50), conf.int = FALSE)
#' @export
autoplot.varprd <- function(object, is.date = NULL, ts.connect = TRUE,
                            scales = 'free_y',
                            predict.geom = 'line',
                            predict.colour = '#0000FF', predict.size = NULL,
                            predict.linetype = NULL, predict.alpha = NULL,
                            predict.fill = NULL, predict.shape = NULL,
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                            conf.int.fill = '#000000', conf.int.alpha = 0.3,
                            ...) {

  plot.data <- ggplot2::fortify(object, is.date = is.date,
                                ts.connect = ts.connect, melt = TRUE)

  # Filter existing values to avoid warnings
  original.data <- dplyr::filter(plot.data, !is.na(Data))
  predict.data <- dplyr::filter(plot.data, !is.na(fcst))

  p <- autoplot.ts(original.data, columns = 'Data', ...)

  p <- autoplot.ts(predict.data, columns = 'fcst', p = p,
                   geom = predict.geom,
                   colour = predict.colour, size = predict.size,
                   linetype = predict.linetype, alpha = predict.alpha,
                   fill = predict.fill, shape = predict.shape)

  p <- p + ggplot2::facet_grid(variable ~ ., scales = scales)

  p <- plot_confint(p = p, data = predict.data, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  p
}
