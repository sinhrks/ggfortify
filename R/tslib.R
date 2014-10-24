#' Convert \code{ts} index to \code{Date} \code{vector}.
#' 
#' @param data \code{ts} instance
#' @return vector
#' @examples
#' ggfortify:::get.dtindex(AirPassengers)
#' ggfortify:::get.dtindex(UKgas)
get.dtindex <- function(data) {
  dtindex <- attr(data, which='tsp')
  if (is.null(dtindex)) {
    stop('Failed to convert ts object index to date')
  }
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
  dtindex <- attr(data, which = 'tsp')
  if (is.null(dtindex)) {
    stop('Failed to convert ts object index to date')
  }
  dt.by <- 1 / dtindex[3]
  dtindex <- seq(from = dtindex[2] + dt.by, length = length, by = dt.by)
  dtindex <- zoo::as.Date.yearmon(dtindex)
  dtindex
}


#' Calcurate confidence interval for \code{stats::acf}
#' 
#' @param x \code{stats::acf} instance
#' @param ci Float value for confidence interval
#' @param ci.type "white" or "ma"
#' @return vector
#' @examples
#' confint(acf(AirPassengers))
#' confint(acf(AirPassengers), ci.type = 'ma')
confint.acf <- function (x, ci = 0.95, ci.type = "white") {
  if ((nser <- ncol(x$lag)) < 1L) 
    stop("x$lag must have at least 1 column")
  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"

  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }
  clim0 <- if (with.ci) 
    qnorm((1 + ci)/2)/sqrt(x$n.used)
  else c(0, 0)
  
  Npgs <- 1L
  nr <- nser
  
  if (nser > 1L && nser > max.mfrow) {
    Npgs <- ceiling(nser / max.mfrow)
    nr <- ceiling(nser / Npgs)
  }

  for (I in 1L:Npgs) for (J in 1L:Npgs) {
    iind <- (I - 1) * nr + 1L:nr
    jind <- (J - 1) * nr + 1L:nr

    for (i in iind) {
      for (j in jind) {
        if (!(max(i, j) > nser)) {
          clim <- if (with.ci.ma && i == j) 
            clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j]^2)))
          else clim0

          if (with.ci && ci.type == "white") {
            clim
          } else if (with.ci.ma && i == j) {
            clim <- clim[-length(clim)]
          }
        }
      }
    }
  }
  if (with.ci.ma && length(clim) < length(x$lag)) {
    clim <- c(NA, clim)
  }
  return(clim)
}