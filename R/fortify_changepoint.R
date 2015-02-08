#' Convert \code{changepoint::cpt} and \code{strucchange::breakpoints} to data.frame.
#'
#' @param model \code{chantepoint::cpt} or \code{strucchange::breakpoints} instance
#' @param data original dataset, if needed
#' @param original (Deprecated) use data
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @param ... other arguments passed to methods
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
#' ggplot2::fortify(breakpoints(bp.nile, breaks = 2), data = Nile)
#' @export
fortify.cpt <- function(model, data = NULL, original = NULL,
                        is.date = NULL, ...) {
  # changepoint::cpt is S4 class

  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }

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
  dplyr::tbl_df(d)
}

#' @export
fortify.breakpoints <- fortify.cpt

#' @export
fortify.breakpointsfull <- fortify.cpt

#' Autoplot \code{changepoint::cpt}.
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
#' ggplot2::autoplot(cpt.mean(AirPassengers))
#' ggplot2::autoplot(cpt.meanvar(AirPassengers))
#' @export
autoplot.cpt <- function(object, is.date = NULL,
                         cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                         ...) {
  plot.data <- ggplot2::fortify(object, is.date = is.date)
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
#' @param object \code{strucchange::breakpoints} or \code{strucchange::breakpointsfull} instance.
#' @param data Original time series. Mandatory for plotting \code{strucchange::breakpoints} instance.
#' @param original (Deprecated) use data
#' @param cpt.colour Line colour for changepoints
#' @param cpt.linetype Line type for changepoints
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @aliases autoplot.breakpointsfull
#' @examples
#' library(strucchange)
#' bp.nile <- breakpoints(Nile ~ 1)
#' ggplot2::autoplot(bp.nile)
#' ggplot2::autoplot(bp.nile, is.date = TRUE)
#' ggplot2::autoplot(breakpoints(bp.nile, breaks = 2), data = Nile)
#' @export
autoplot.breakpoints <- function(object, data = NULL, original = NULL,
                                 cpt.colour = '#FF0000', cpt.linetype = 'dashed',
                                 ...) {
  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }
  plot.data <- ggplot2::fortify(object, data = data)
  y = 'Data'
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
