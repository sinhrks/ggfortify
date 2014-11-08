#' Convert \code{changepoint::cpt} and \code{strucchange::breakpoints} to data.frame.
#' 
#' @param data \code{chantepoint::cpt} or \code{strucchange::breakpoints} instance
#' @param original Original time series attached to \code{strucchange::breakpoints}
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @return data.frame
#' @aliases fortify.breakpointsfull fortify.breakpoints
#' @examples
#' library(changepoint)
#' ggplot2::fortify(cpt.mean(AirPassengers))
#' ggplot2::fortify(cpt.var(AirPassengers))
#' ggplot2::fortify(cpt.meanvar(AirPassengers))
#' 
#' library(strucchange)
#' bp.nile <- breakpoints(Nile ~ 1)
#' ggplot2::fortify(bp.nile)
#' ggplot2::fortify(breakpoints(bp.nile, breaks = 2))
#' ggplot2::fortify(breakpoints(bp.nile, breaks = 2), original = Nile)
#' @export
fortify.cpt <- function(data, original = NULL, is.date = NULL) {
  # changepoint::cpt is S4 class
  if (is(data, 'cpt')) {
    d <- ggplot2::fortify(data@data.set, is.date = is.date)
  
    cptindex <- get.dtindex(data@data.set, is.date = is.date)[data@cpts]
    cptd <- data.frame(Index = cptindex)
    if ('mean' %in% names(data@param.est)) {
      cptd$mean <- data@param.est$mean
    }
    if ('variance' %in% names(data@param.est)) {
      cptd$variance <- data@param.est$variance
    }
    d <- dplyr::left_join(d, cptd, by = 'Index')
  } else if (is(data, 'breakpointsfull') || is(data, 'breakpoints')) {
      dtindex <- get.dtindex(data$datatsp, is.tsp = TRUE, is.date = is.date)
      breakpoints <- rep(NA, length(dtindex))
      breakpoints[data$breakpoints] <- 1
      d <- data.frame(Index = dtindex, Breaks = breakpoints)
      if (!is.null(original)) {
        original <- ggplot2::fortify(original, is.date = is.date)
        d <- dplyr::left_join(original, d, by = 'Index')        
      } else if ('y' %in% names(data)) {
        # breakpointsfull should have y
        original <- data.frame(Index = dtindex, Data = data$y)
        d <- dplyr::left_join(original, d, by = 'Index')
      }
  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(data)))
  }
  dplyr::tbl_df(d)
}

#' @export
fortify.breakpoints <- fortify.cpt

#' @export
fortify.breakpointsfull <- fortify.cpt

#' Autoplot \code{changepoint::cpt}.
#' 
#' @param data \code{changepoint::cpt} instance
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12. 
#' @param cpt.colour Line colour for changepoints
#' @param cpt.linetype Line type for changepoints
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @examples
#' library(changepoint)
#' ggplot2::autoplot(cpt.mean(AirPassengers))
#' ggplot2::autoplot(cpt.meanvar(AirPassengers))
#' @export
autoplot.cpt <- function(data, is.date = NULL,
                         cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                         ...) {
  plot.data <- ggplot2::fortify(data, is.date = is.date)
  y = 'Data'
  p <- autoplot.ts(plot.data, columns = y, ...)

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

#' Autoplot \code{strucchange::breakpoints}.
#' 
#' @param data \code{strucchange::breakpoints} or \code{strucchange::breakpointsfull} instance.
#' @param original Original time series. Mandatory for plotting \code{strucchange::breakpoints} instance.
#' @param cpt.colour Line colour for changepoints
#' @param cpt.linetype Line type for changepoints
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @aliases autoplot.breakpointsfull
#' @examples
#' library(strucchange)
#' bp.nile <- breakpoints(Nile ~ 1)
#' ggplot2::autoplot(bp.nile)
#' ggplot2::autoplot(bp.nile, is.date = TRUE)
#' ggplot2::autoplot(breakpoints(bp.nile, breaks = 2), original = Nile)
#' @export
autoplot.breakpoints <- function(data, original = NULL,
                                 cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                                 ...) {
  plot.data <- ggplot2::fortify(data, original = original)
  y = 'Data'
  if (y %in% names(plot.data)) {
    p <- autoplot.ts(plot.data, columns = y, ...)
  } else {
    stop("'original' data is mandatory for plotting breakpoints instance")
  }
  
  d <- dplyr::filter(plot.data, !is.na(Breaks))
  p <- p + ggplot2::geom_vline(xintercept = as.integer(d$Index),
                               colour = cpt.colour, linetype = cpt.linetype)
  p
}

#' @export
fortify.breakpointsfull <- fortify.breakpoints
