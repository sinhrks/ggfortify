#' Attach confidence interval to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param data Data contains lower and upper confidence intervals
#' @param lower Column name for lower confidence interval
#' @param upper Column name for upper confidence interval
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.colour Line colour for confidence intervals
#' @param conf.int.linetype Line type for confidence intervals
#' @param conf.int.fill Fill colour for confidence intervals
#' @param conf.int.alpha Alpha for confidence intervals
#' @return ggplot
#' @examples
#' d <- fortify(stats::acf(AirPassengers, plot = FALSE))
#' p <- ggplot(data = d, mapping = aes(x = Lag))
#' ggfortify:::plot_confint(p, data = d)
plot_confint <- function (p, data = NULL, lower = 'lower', upper = 'upper',
                          conf.int = TRUE,
                          conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                          conf.int.fill = '#000000', conf.int.alpha = 0.3) {

  if (missing(conf.int) && (!missing(conf.int.colour) ||
                            !missing(conf.int.linetype) ||
                            !missing(conf.int.fill) ||
                            !missing(conf.int.alpha))) {
    # if conf.int is missing but other options are specified, turn conf.in to TRUE
    conf.int <- TRUE
  }

  if (is.null(data)) {
    stop("Internal Error: 'data' must be provided to plot_confint")
  }

  if (conf.int) {
    if (!is.null(conf.int.fill)) {
      p<- p + geom_factory(geom_ribbon, data, ymin = lower, ymax = upper,
                           fill = conf.int.fill, alpha = conf.int.alpha, na.rm = TRUE)
    }
    if (conf.int.linetype != 'none') {
      p <- p + geom_factory(geom_line, data, y = lower,
                            colour = conf.int.colour, linetype = conf.int.linetype,
                            na.rm = TRUE)
      p <- p + geom_factory(geom_line, data, y = upper,
                     colour = conf.int.colour, linetype = conf.int.linetype,
                     na.rm = TRUE)
    }
  }
  p
}

#' Attach label to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param data Data contains text label
#' @param label Logical value whether to display labels
#' @param label.label Column name used for label text
#' @param label.colour Colour for text labels
#' @param label.alpha Alpha for text labels
#' @param label.size Size for text labels
#' @param label.angle Angle for text labels
#' @param label.family Font family for text labels
#' @param label.fontface Fontface for text labels
#' @param label.lineheight Lineheight for text labels
#' @param label.hjust Horizontal adjustment for text labels
#' @param label.vjust Vertical adjustment for text labels
#' @return ggplot
plot_label <- function(p, data, label = TRUE, label.label = 'rownames',
                       label.colour = NULL, label.alpha = NULL,
                       label.size = NULL, label.angle = NULL,
                       label.family = NULL, label.fontface = NULL,
                       label.lineheight = NULL,
                       label.hjust = NULL, label.vjust = NULL) {

  if (!is.data.frame(data)) {
    stop(paste0('Unsupported class: ', class(data)))
  }

  if (!missing(label.colour) && !is.null(label.colour) && missing(label)) {
    # if flag is missing but colour is specified, turn flag to TRUE
    label <- TRUE
  }

  if (label) {
    if (is.null(label.colour)) {
      # NULL may be explicitly passed from parent functions
      label.colour <- '#000000'
    }
    p <- p + geom_factory(ggplot2::geom_text, data,
                          label = label.label,
                          colour = label.colour, alpha = label.alpha,
                          size = label.size, angle = label.angle,
                          family = label.family, fontface = label.fontface,
                          lineheight = label.lineheight,
                          hjust = label.hjust, vjust = label.vjust)
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
         representation(plots = 'list', ncol = 'numeric', nrow = 'numeric'),
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
#' @param x \code{ggmultiplot}
#' @return NULL
setMethod('print', 'ggmultiplot',
  function(x) {
    nplots = length(x@plots)
    if (nplots==1) {
      print(x@plots[[1]])
    } else {
      layout <- get.layout(nplots, x@ncol, x@nrow)
      args <- c(x@plots, list(ncol = ncol(layout), nrow = nrow(layout)))
      do.call(gridExtra::grid.arrange, args)
    }
})

#' Generic show function for \code{ggmultiplot}
#'
#' @param object \code{ggmultiplot}
#' @return NULL
setMethod('show', 'ggmultiplot',
          function(object) { print(object) })

