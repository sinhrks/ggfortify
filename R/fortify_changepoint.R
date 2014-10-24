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
  dplyr::tbl_df(d)
}

#' Autoplot \code{changepoint::cpt}.
#' 
#' @param data \code{changepoint::cpt} instance
#' @param cpt.colour Line colour for \code{changepoint::cpt}
#' @param cpt.linetype Line type for \code{changepoint::cpt}
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @examples
#' ggplot2::autoplot(changepoint::cpt.mean(AirPassengers))
#' ggplot2::autoplot(changepoint::cpt.meanvar(AirPassengers))
#' @export
autoplot.cpt <- function(data,
                         cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                         ...) {
  plot.data <- ggplot2::fortify(data)
  y = 'Data' # names(plot.data)[2]
  p <- ggfortify:::autoplot.ts(plot.data, columns = y, ...)

  if ('mean' %in% names(plot.data)) {
    d <- dplyr::filter(plot.data, !is.na(mean))
    p <- p + ggplot2::geom_vline(xintercept = as.integer(d$Index),
                                 colour = cpt.colour, linetype = cpt.linetype)
  } 
  if ('variance' %in% names(plot.data)) {
    d <- dplyr::filter(plot.data, !is.na(variance))
    p <- p + ggplot2::geom_vline(xintercept = as.integer(d$Index),
                                 colour = cpt.colour, linetype = cpt.linetype)
  }   
  p
}
