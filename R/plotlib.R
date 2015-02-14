#' Attach confidence interval to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param data Data contains lower and upper confidence intervals
#' @param lower Column name for lower confidence interval
#' @param upper Column name for upper confidence interval
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param colour Line colour for confidence intervals
#' @param linetype Line type for confidence intervals
#' @param fill Fill colour for confidence intervals
#' @param alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' d <- fortify(stats::acf(AirPassengers, plot = FALSE))
#' p <- ggplot(data = d, mapping = aes(x = Lag))
#' ggfortify:::plot_confint(p, data = d)
plot_confint <- function (p, data = NULL, lower = 'lower', upper = 'upper',
                          conf.int = TRUE,
                          colour = '#0000FF', linetype = 'none',
                          fill = '#000000', alpha = 0.3) {

  if (missing(conf.int) && (!missing(colour) ||
                            !missing(linetype) ||
                            !missing(fill) ||
                            !missing(alpha))) {
    # if conf.int is missing but other options are specified, turn conf.in to TRUE
    conf.int <- TRUE
  }

  if (is.null(data)) {
    stop("Internal Error: 'data' must be provided to plot_confint")
  }

  if (conf.int) {
    if (!is.null(fill)) {
      p<- p + geom_factory(geom_ribbon, data, ymin = lower, ymax = upper,
                           fill = fill, alpha = alpha, na.rm = TRUE)
    }
    if (linetype != 'none') {
      p <- p + geom_factory(geom_line, data, y = lower,
                            colour = colour, linetype = linetype,
                            na.rm = TRUE)
      p <- p + geom_factory(geom_line, data, y = upper,
                     colour = colour, linetype = linetype,
                     na.rm = TRUE)
    }
  }
  p
}

#' Attach label to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param data Data contains text label
#' @param flag Logical value whether to display labels
#' @param label Column name used for label text
#' @param colour Text colour for labels
#' @param size Text size for labels
#' @return ggplot
plot.label <- function(p, data, flag = TRUE, label = 'rownames',
                       colour = NULL, size = 4) {

  if (!is.data.frame(data)) {
    stop(paste0('Unsupported class: ', class(data)))
  }

  if (!missing(colour) && !is.null(colour) && missing(flag)) {
    # if flag is missing but colour is specified, turn flag to TRUE
    flag <- TRUE
  }

  if (flag) {
    if (is.null(colour)) {
      # NULL may be explicitly passed from parent functions
      colour <- '#000000'
    }
    p <- p + geom_factory(ggplot2::geom_text, data,
                          label = label, colour = colour, size = size)
  }
  p
}

#' Factory function to control \code{ggplot2::geom_xxx} functions
#'
#' @param geom string representation of \code{ggplot2::geom_xxx} function
#' @param allowed character vector contains allowed values
#' @return function
#' @examples
#' ggfortify:::get_geom_function('point')
#' ggfortify:::get_geom_function('line', allowed = c('line'))
get_geom_function <- function(geom, allowed = c('line', 'bar', 'point')) {
  if (geom == 'line' && 'line' %in% allowed) {
    return(ggplot2::geom_line)
  } else if (geom == 'bar' && 'bar' %in% allowed) {
    return(ggplot2::geom_bar)
  } else if (geom == 'point' && 'point' %in% allowed) {
    return(ggplot2::geom_point)
  }
  stop(paste("Invalid geom is specified. Use", allowed))
}

#' Factory function to control \code{ggplot2::geom_xxx} functions
#'
#' @param geomfunc \code{ggplot2::geom_xxx} function
#' @param data plotting data
#' @param ... other arguments passed to methods
#' @return proto
geom_factory <- function(geomfunc, data, ...) {
  mapping <- list()
  option <- list()

  columns <- colnames(data)
  for (key in names(list(...))) {
      value <- list(...)[[key]]
      if (is.null(value)) {
        # pass
      } else if (value %in% columns) {
        mapping[[key]] <- value
      } else {
        option[[key]] <- value
      }
  }
  option[['data']] <- data
  option[['mapping']] <- do.call(ggplot2::aes_string, mapping)
  proto <- do.call(geomfunc, option)
  return(proto)
}

#' An S4 class to hold multiple \code{ggplot2::ggplot} instances
#'
#' @slot plots List of \code{ggplot2::ggplot} instances
#' @slot ncol Number of grid columns
#' @slot nrow Number of grid rows
setClass('ggmultiplot',
         representation(plots = 'list', ncol = 'numeric', 'nrow' = 'numeric'),
         prototype = list(ncol = 0, nrow = 0))

#' Generic add operator for \code{ggmultiplot}
#'
#' @param e1 first argument
#' @param e2 second argument
#' @return \code{ggmultiplot}
setMethod('+', c('ggmultiplot', 'ANY'),
  function(e1, e2) {
    plots <- lapply(e1@plots, function(x) { x + e2 })
    new('ggmultiplot', plots = plots,
        ncol = e1@ncol, nrow = e1@nrow)
})

#' Calcurate layout matrix for \code{ggmultiplot}
#'
#' @param nplots Number of plots
#' @param ncol Number of grid columns
#' @param nrow Number of grid rows
#' @return matrix
#' @examples
#' ggfortify:::get.layout(3, 2, 2)
get.layout <- function(nplots, ncol, nrow) {
  if (ncol == 0 && nrow == 0) {
    ncol <- 2
  } else if (ncol == 0 && nrow != 0) {
    ncol <- ceiling(nplots / nrow)
  }

  if (nrow == 0) {
    nrow <- ceiling(nplots / ncol)
  } else {
    nrow <- nrow
  }

  if (nrow * ncol < nplots) {
    message <- paste('nrow * ncol (', nrow, '*', ncol ,
                     ')must be larger than number of plots', nplots)
    stop(message)
  }

  t(matrix(1:(ncol * nrow), ncol = nrow, nrow = ncol))
}

#' Generic print function for \code{ggmultiplot}
#'
#' @param p \code{ggmultiplot}
#' @return NULL
print.ggmultiplot <- function(p) {
  nplots = length(p@plots)
  if (nplots==1) {
    print(p@plots[[1]])
  } else {
    layout <- get.layout(nplots, p@ncol, p@nrow)
    args <- c(p@plots, list(ncol = ncol(layout), nrow = nrow(layout)))
    do.call(gridExtra::grid.arrange, args)
  }
}

#' Generic show function for \code{ggmultiplot}
#'
#' @param object \code{ggmultiplot}
#' @return NULL
setMethod('show', 'ggmultiplot',
          function(object) { print(object) })

