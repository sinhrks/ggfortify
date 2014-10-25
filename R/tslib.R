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
#' air.acf <- acf(AirPassengers, plot = FALSE)
#' ggfortify:::confint.acf(air.acf)
#' ggfortify:::confint.acf(air.acf, ci.type = 'ma')
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
  
  if (nser > 1L) {
    Npgs <- nser
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

#' Calcurate fitted values for \code{stats::ar}
#' 
#' @param object \code{stats::ar} instance
#' @return ts An time series of the one-step forecasts
#' @examples
#' fitted(ar(WWWusage))
#' @export
fitted.ar <- function(object, ...) {
  x <- forecast::getResponse(object)
  if (is.null(x)) {
    return(NULL)
  }
  return(x - object$resid)
}

#' Calcurate residuals for \code{stats::ar}
#' 
#' @param object \code{stats::ar} instance.
#' @return ts Residuals extracted from the object object.
#' @examples
#' fitted(ar(WWWusage))
#' @export
residuals.ar <- function(object, ...) {
  object$resid
}

#' Plots a cumulative periodogram.
#' 
#' @param x \code{stats::acf} instance
#' @param taper Proportion tapered in forming the periodogram
#' @param colour Line colour
#' @param linetype Line type
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' ggcpgram(AirPassengers)
#' @export
ggcpgram <- function (ts, taper = 0.1, 
                      colour = '#000000', linetype = 'solid',
                      conf.int = TRUE,
                      conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                      conf.int.fill = NULL, conf.int.alpha = 0.3) {
  if (NCOL(ts) > 1) 
    stop("only implemented for univariate time series")
  x <- as.vector(ts)
  x <- x[!is.na(x)]
  x <- spec.taper(scale(x, TRUE, FALSE), p = taper)
  y <- Mod(fft(x)) ^ 2 / length(x)
  y[1L] <- 0
  n <- length(x)
  x <- (0:(n / 2)) * frequency(ts) / n
  if (length(x)%%2 == 0) {
    n <- length(x) - 1
    y <- y[1L:n]
    x <- x[1L:n]
  }
  else y <- y[seq_along(x)]
  xm <- frequency(ts) / 2
  mp <- length(x) - 1
  crit <- 1.358 / (sqrt(mp) + 0.12 + 0.11 / sqrt(mp))

  d <- data.frame(x = x,
                  y = cumsum(y) / sum(y),
                  upper = 1 / xm * x + crit,
                  lower = 1 / xm * x - crit)
  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes_string(x = 'x', y = 'y')) +
    geom_line(colour = colour, linetype = linetype) +
    ggplot2::scale_x_continuous(name = '', limits = c(0, xm)) +
    ggplot2::scale_y_continuous(name = '', limits = c(0, 1))
  
  p <- ggfortify:::plot.conf.int(p, conf.int = conf.int,
                                 conf.int.colour = conf.int.colour,
                                 conf.int.linetype = conf.int.linetype,
                                 conf.int.fill = conf.int.fill,
                                 conf.int.alpha = conf.int.alpha)
  p
}