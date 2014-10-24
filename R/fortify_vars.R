#' Convert \code{vars::varprd} to data.frame.
#' 
#' @param data \code{vars::varprd} instance
#' @param melt Logical flag indicating whether to melt each timeseries as variable
#' @return data.frame
#' @examples
#' data(Canada, package = 'vars')
#' d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
#' d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')
#' ggplot2::fortify(stats::predict(d.var, n.ahead = 50))
#' @export
fortify.varprd <- function(data, melt = FALSE){  
  dtindex <- get.dtindex(data$model$y)
  fitted <- cbind(data.frame(Index = dtindex),
                  data.frame(data$model$y))
  rownames(fitted) <- NULL
  
  fcst <- data$fcst
  dtindex.cont <- get.dtindex.continuous(data$model$y, length = nrow(fcst[[1]]))
  cols <- names(fcst)
  
  if (melt) {
    # for autoplot conversion
    for (col in cols){
      pred <- data.frame(fcst[[col]])
      rownames(pred) <- NULL
      pred$Index <- dtindex.cont
      obs <- fitted[, c('Index', col)]
      colnames(obs) <- c('Index', 'Data')
      binded <- dplyr::rbind_list(obs, pred)
      binded$variable <- col
      fcst[[col]] <- binded
    }
    return(dplyr::rbind_all(fcst))
  } else {
    for (col in cols){
      colnames(fcst[[col]]) <- paste0(col, '.', colnames(fcst[[col]]))
    }
    forecasted <- data.frame(do.call(cbind, fcst))
    forecasted$Index <- dtindex.cont
    rownames(forecasted) <- NULL
    
    return(dplyr::rbind_list(fitted, forecasted))
  }
}

#' Autoplot \code{vars::varprd}.
#' 
#' @param data \code{vars::varpred} instance
#' @param scales Scale value passed to \code{ggplot2}
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
#' data(Canada, package = 'vars')
#' d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
#' d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')
#' ggplot2::autoplot(stats::predict(d.var, n.ahead = 50))
#' ggplot2::autoplot(stats::predict(d.var, n.ahead = 50), conf.int = FALSE)
#' @export
autoplot.varprd <- function(data, scales = 'free_y',
                            predict.colour = '#0000FF', predict.linetype = 'solid',
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                            conf.int.fill = '#000000', conf.int.alpha = 0.3,
                            ...) {
  plot.data <- ggplot2::fortify(data, melt = TRUE)
  
  # Filter existing values to avoid warnings
  original.data <- dplyr::filter(plot.data, !is.na(Data))
  predict.data <- dplyr::filter(plot.data, !is.na(fcst))

  p <- ggfortify:::autoplot.ts(original.data, columns = 'Data', ...)

  p <- p + 
    ggplot2::geom_line(data = predict.data,
                       mapping = ggplot2::aes_string(y = 'fcst'),
                       colour = predict.colour, linetype = predict.linetype)  + 
    ggplot2::facet_grid(variable ~ ., scales = scales)
  
  p <- ggfortify:::plot.conf.int(p, data = predict.data, 
                                 conf.int = conf.int,
                                 conf.int.colour = conf.int.colour,
                                 conf.int.linetype = conf.int.linetype,
                                 conf.int.fill = conf.int.fill,
                                 conf.int.alpha = conf.int.alpha)
  p 
}