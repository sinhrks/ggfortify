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
  fitted <- cbind(data.frame(time = dtindex),
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
      pred$time <- dtindex.cont
      obs <- fitted[, c('time', col)]
      colnames(obs) <- c('time', 'original')
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
    forecasted$time <- dtindex.cont
    rownames(forecasted) <- NULL
    
    return(dplyr::rbind_list(fitted, forecasted))
  }
}

#' Autoplot \code{vars::varprd}.
#' 
#' @param data \code{vars::varpred} instance
#' @param scales Scale value passed to \code{ggplot2}

#' @param ts.colour Line colour for \code{stats::ts}
#' @param ts.linetype Line type for \code{stats::ts}
#' @param predict.colour Line colour for predicted \code{stats::ts}
#' @param predict.linetype Line type for predicted \code{stats::ts}
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' data(Canada, package = 'vars')
#' d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
#' d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')
#' ggplot2::autoplot(stats::predict(d.var, n.ahead = 50))
#' ggplot2::autoplot(stats::predict(d.var, n.ahead = 50), conf.int = FALSE)
#' @export
autoplot.varprd <- function(data, scales = 'free_y',
                            ts.colour = '#000000', ts.linetype = 'solid',
                            predict.colour = '#0000FF', predict.linetype = 'solid',
                            conf.int = TRUE,
                            conf.int.fill = '#000000', conf.int.alpha = 0.3) {
  plot.data <- ggplot2::fortify(data, melt = TRUE)
  
  # Filter existing values to avoid warnings
  original.data <- dplyr::filter(plot.data, !is.na(original))
  predict.data <- dplyr::filter(plot.data, !is.na(fcst))

  p <- ggplot2::ggplot(data = plot.data,
                       mapping = ggplot2::aes(x = time)) +
    ggplot2::geom_line(data = original.data,
                       mapping = ggplot2::aes(y = original),
                       colour = ts.colour, linetype = ts.linetype) +
    ggplot2::geom_line(data = predict.data,
                       mapping = ggplot2::aes(y = fcst),
                       colour = predict.colour, linetype = predict.linetype) + 
    ggplot2::facet_grid(variable ~ ., scales = scales)
  if (conf.int) {
    p <- p + ggplot2::geom_ribbon(data = predict.data,
                                  mapping = ggplot2::aes(ymin = lower, ymax = upper),
                                  fill = conf.int.fill, alpha = conf.int.alpha)
  }
  p 
}