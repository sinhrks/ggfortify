#' Convert \code{ts} index to \code{Date} \code{vector}.
#' 
#' @param data \code{ts} instance
#' @return vector
#' @examples
#' ggfortify:::get.dtindex(AirPassengers)
#' ggfortify:::get.dtindex(UKgas)
get.dtindex <- function(data) {
  dtindex <- attr(data, which='tsp')
  dtindex <- seq(from = dtindex[1], to = dtindex[2], by= 1 / dtindex[3])
  dtindex <- zoo::as.Date.yearmon(dtindex)
  dtindex
}

#' Get \code{Date} \code{vector} continue to \code{ts} index.
#' 
#' @param data \code{ts} instance
#' @param length A number to continue
#' @return vector
#' @examples
#' ggfortify:::get.dtindex.continuous(AirPassengers, length = 10)
#' ggfortify:::get.dtindex.continuous(UKgas, length = 10)
get.dtindex.continuous <- function(data, length) {
  dtindex <- attr(data, which='tsp')
  dt.by <- 1 / dtindex[3]
  dtindex <- seq(from = dtindex[2] + dt.by, length = length, by = dt.by)
  dtindex <- zoo::as.Date.yearmon(dtindex)
  dtindex
}