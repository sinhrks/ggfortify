#' Convert \code{changepoint::cpt} to data.frame.
#' 
#' @param data \code{chantepoint::cpt} instance
#' @param keep.original Logical value whether to keep original \code{stats::ts}
#' @return data.frame
#' @examples
#' ggplot2::fortify(changepoint::cpt.mean(AirPassengers))
#' ggplot2::fortify(changepoint::cpt.var(AirPassengers))
#' ggplot2::fortify(changepoint::cpt.meanvar(AirPassengers))
#' @export
fortify.cpt <- function(data, keep.original = TRUE) {
  # changepoint::cpt is S4 class
  d <- ggplot2::fortify(data@data.set)
  
  cptindex <- get.dtindex(data@data.set)[data@cpts]
  cptd <- data.frame(Index = cptindex)
  if ('mean' %in% names(data@param.est)) {
    cptd$mean <- data@param.est$mean
  }
  if ('variance' %in% names(data@param.est)) {
    cptd$variance <- data@param.est$variance
  }
  if (keep.original) {
    d <- dplyr::left_join(d, cptd, by = 'Index') 
  } else {
    d <- cptd
  } 
  d <- dplyr::tbl_df(d)
}

#' Autoplot \code{changepoint::cpt}.
#' 
#' @param data \code{changepoint::cpt} instance
#' @param ts.colour Line colour for time-series
#' @param ts.linetype Line type for time-series
#' @param cpt.colour Line colour for \code{changepoint::cpt}
#' @param cpt.linetype Line type for \code{changepoint::cpt}
#' @return ggplot
#' @examples
#' ggplot2::autoplot(changepoint::cpt.mean(AirPassengers))
#' ggplot2::autoplot(changepoint::cpt.meanvar(AirPassengers))
#' @export
autoplot.cpt <- function(data,
                         ts.colour = '#000000', ts.linetype = 'solid',
                         cpt.colour = '#FF0000', cpt.linetype = 'dashed') {
  plot.data <- ggplot2::fortify(data)
  y = names(plot.data)[2]
  
  p <- ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(mapping = ggplot2::aes_string(x = 'Index', y = y),
                       colour = ts.colour, linetyle = ts.linetype)

  if ('mean' %in% names(plot.data)) {
    d <- dplyr::filter(plot.data, !is.na(mean))
    p <- p + ggplot2::geom_vline(xintercept = as.integer(d$time),
                                 colour = cpt.colour, linetype = cpt.linetype)
  } 
  if ('variance' %in% names(plot.data)) {
    d <- dplyr::filter(plot.data, !is.na(variance))
    p <- p + ggplot2::geom_vline(xintercept = as.integer(d$time),
                                 colour = cpt.colour, linetype = cpt.linetype)
  }   
  p + ggplot2::scale_y_continuous(name = '')
}
