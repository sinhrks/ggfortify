#' Convert \code{changepoint::cpt} to data.frame.
#' 
#' @param data \code{chantepoint::cpt} instance
#' @return data.frame
#' @examples
#' ggplot2::fortify(changepoint::cpt.mean(AirPassengers))
#' ggplot2::fortify(changepoint::cpt.var(AirPassengers))
#' ggplot2::fortify(changepoint::cpt.meanvar(AirPassengers))
#' @export
fortify.cpt <- function(data) {
  # changepoint::cpt is S4 class
  dtindex <- get.dtindex(data@data.set)[data@cpts]
  d <- data.frame(time = dtindex,
                  cpts = data@cpts)
  if ('mean' %in% names(data@param.est)) {
    d$mean <- data@param.est$mean
  }
  if ('variance' %in% names(data@param.est)) {
    d$variance <- data@param.est$variance
  } 
  d
}
