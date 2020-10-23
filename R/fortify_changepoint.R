#' Convert \code{changepoint::cpt} and \code{strucchange::breakpoints} to \code{data.frame}
#'
#' @param model \code{chantepoint::cpt} or \code{strucchange::breakpoints} instance
#' @inheritParams fortify_base
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @return data.frame
#' @aliases fortify.breakpointsfull fortify.breakpoints
#' @examples
#' library(changepoint)
#' fortify(cpt.mean(AirPassengers))
#' fortify(cpt.var(AirPassengers))
#' fortify(cpt.meanvar(AirPassengers))
#'
#' library(strucchange)
#' bp.nile <- breakpoints(Nile ~ 1)
#' fortify(bp.nile)
#' fortify(breakpoints(bp.nile, breaks = 2))
#' fortify(breakpoints(bp.nile, breaks = 2), data = Nile)
#' @export
fortify.cpt <- function(model, data = NULL,
                        is.date = NULL, ...) {
  # changepoint::cpt is S4 class
  if (is(model, 'cpt')) {
    d <- ggplot2::fortify(model@data.set, is.date = is.date)

    cptindex <- get.dtindex(model@data.set, is.date = is.date)[model@cpts]
    cptd <- data.frame(Index = cptindex)
    if ('mean' %in% names(model@param.est)) {
      cptd$mean <- model@param.est$mean
    }
    if ('variance' %in% names(model@param.est)) {
      cptd$variance <- model@param.est$variance
    }
    d <- dplyr::left_join(d, cptd, by = 'Index')
  } else if (is(model, 'breakpointsfull') || is(model, 'breakpoints')) {
      dtindex <- get.dtindex(model$datatsp, is.tsp = TRUE, is.date = is.date)
      breakpoints <- rep(NA, length(dtindex))
      breakpoints[model$breakpoints] <- 1
      d <- data.frame(Index = dtindex, Breaks = breakpoints)
      if (!is.null(data)) {
        data <- ggplot2::fortify(data, is.date = is.date)
        d <- dplyr::left_join(data, d, by = 'Index')
      } else if ('y' %in% names(model)) {
        # breakpointsfull should have y
        data <- data.frame(Index = dtindex, Data = model$y)
        d <- dplyr::left_join(data, d, by = 'Index')
      }
  } else {
    stop(paste0('Unsupported class for autoplot.pca_common: ', class(model)))
  }
  post_fortify(d)
}

#' @export
fortify.breakpoints <- fortify.cpt

#' @export
fortify.breakpointsfull <- fortify.cpt

#' Autoplot \code{changepoint::cpt}
#'
#' @param object \code{changepoint::cpt} instance
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param cpt.colour Line colour for changepoints
#' @param cpt.linetype Line type for changepoints
#' @param ... other arguments passed \code{autoplot.ts}
#' @return ggplot
#' @examples
#' library(changepoint)
#' autoplot(cpt.mean(AirPassengers))
#' autoplot(cpt.meanvar(AirPassengers))
#' @export
autoplot.cpt <- function(object, is.date = NULL,
                         cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                         ...) {
  plot.data <- ggplot2::fortify(object, is.date = is.date)
  y <- 'Data'
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

#' Autoplot \code{strucchange::breakpoints}
#'
#' @param object \code{strucchange::breakpoints} or \code{strucchange::breakpointsfull} instance.
#' @param data Original time series. Mandatory for plotting \code{strucchange::breakpoints} instance.
#' @param cpt.colour Line colour for changepoints
#' @param cpt.linetype Line type for changepoints
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @aliases autoplot.breakpointsfull
#' @examples
#' library(strucchange)
#' bp.nile <- breakpoints(Nile ~ 1)
#' autoplot(bp.nile)
#' autoplot(bp.nile, is.date = TRUE)
#' autoplot(breakpoints(bp.nile, breaks = 2), data = Nile)
#' @export
autoplot.breakpoints <- function(object, data = NULL,
                                 cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                                 ...) {
  plot.data <- ggplot2::fortify(object, data = data)
  y <- 'Data'
  if (y %in% names(plot.data)) {
    p <- autoplot.ts(plot.data, columns = y, ...)
  } else {
    stop("'data' is mandatory for plotting breakpoints instance")
  }

  d <- dplyr::filter(plot.data, !is.na(Breaks))
  p <- p + ggplot2::geom_vline(xintercept = as.integer(d$Index),
                               colour = cpt.colour, linetype = cpt.linetype)
  p
}

#' @export
fortify.breakpointsfull <- fortify.breakpoints
