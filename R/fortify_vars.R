#' Convert \code{vars::varprd} to data.frame.
#' 
#' @param data \code{vars::varprd} instance
#' @return data.frame
#' @examples
#' data(Canada, package = 'vars')
#' d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
#' d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')
#' ggplot2::fortify(stats::predict(d.var, n.ahead = 50))
#' @export
fortify.varprd <- function(data){  
  dtindex <- get.dtindex(data$model$y)
  fitted <- cbind(data.frame(time = dtindex),
                  data.frame(data$model$y))
  rownames(fitted) <- NULL
  
  fcst <- data$fcst
  dtindex.cont <- get.dtindex.continuous(data$model$y, length = nrow(fcst[[1]]))
  
  cols <- names(fcst)
  for (col in cols){
    colnames(fcst[[col]]) <- paste0(col, '.', colnames(fcst[[col]]))
  }
  forecasted <- data.frame(do.call(cbind, fcst))
  forecasted$time <- dtindex.cont
  
  rownames(forecasted) <- NULL
  
  dplyr::rbind_list(fitted, forecasted)
}

#' Autoplot \code{vars::varprd}.
#' 
#' @param data \code{vars::varpred} instance
#' @return ggplot
#' @export
#' @examples
#' data(Canada, package = 'vars')
#' d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
#' d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')
#' ggplot2::autoplot(stats::predict(d.var, n.ahead = 50))
autoplot.varprd <- function(data) {
  # do not use fortify to make facet
  # plot.data <- ggplot2::fortify(data)

  dtindex <- get.dtindex(data$model$y)
  fitted <- cbind(data.frame(time = dtindex),
                  data.frame(data$model$y))
  rownames(fitted) <- NULL
  
  fcst <- data$fcst
  dtindex.cont <- get.dtindex.continuous(data$model$y, length = nrow(fcst[[1]]))
  
  cols <- names(fcst)
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
  
  plot.data <- dplyr::rbind_all(fcst)

  ggplot2::ggplot(data = plot.data, mapping = ggplot2::aes(x = time)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = original)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = fcst), colour='blue') +
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = lower, ymax = upper),
                         alpha = 0.5) +
    ggplot2::facet_grid(variable ~ ., scales = 'free_y')
}