#' Attach confidence interval to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param data data contains lower and upper confidence intervals
#' @param lower column name for lower confidence interval
#' @param upper column name for upper confidence interval
#' @param conf.int Logical flag indicating whether to plot confidence intervals
#' @param conf.int.geom geometric string for confidence interval. 'line' or 'step'
#' @param conf.int.group name of grouping variable for confidence intervals
#' @param conf.int.colour line colour for confidence intervals
#' @param conf.int.linetype line type for confidence intervals
#' @param conf.int.fill fill colour for confidence intervals
#' @param conf.int.alpha alpha for confidence intervals
#' @return ggplot
#' @examples
#' d <- fortify(stats::acf(AirPassengers, plot = FALSE))
#' p <- ggplot(data = d, mapping = aes(x = Lag))
#' ggfortify:::plot_confint(p, data = d)
plot_confint <- function (p, data = NULL, lower = 'lower', upper = 'upper',
                          conf.int = TRUE, conf.int.geom = 'line',
                          conf.int.group = NULL,
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

  if (conf.int.geom == 'step') {
    ribbon_func <- geom_confint
    line_func <- geom_step
  } else {
    ribbon_func <- geom_ribbon
    line_func <- geom_line
  }

  if (conf.int) {
    if (!is.null(conf.int.fill)) {
      p <- p + geom_factory(ribbon_func, data, ymin = lower, ymax = upper,
                           group = conf.int.group,
                           fill = conf.int.fill, alpha = conf.int.alpha, na.rm = TRUE)
    }
    if (conf.int.linetype != 'none') {
      p <- p + geom_factory(line_func, data, y = lower,
                            group = conf.int.group,
                            colour = conf.int.colour, linetype = conf.int.linetype,
                            na.rm = TRUE)
      p <- p + geom_factory(line_func, data, y = upper,
                     group = conf.int.group,
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
#' @param x x coordinates for label
#' @param y y coordinates for label
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
#' @param label.repel Logical flag indicating whether to use \code{ggrepel}, enabling this may take some time for plotting
#' @param label.show.legend Logical value indicating whether to show the legend of the text labels
#' @param label.position Character or a position function
#' @return ggplot
plot_label <- function(p, data, x = NULL, y = NULL, label = TRUE, label.label = 'rownames',
                       label.colour = NULL, label.alpha = NULL,
                       label.size = NULL, label.angle = NULL,
                       label.family = NULL, label.fontface = NULL,
                       label.lineheight = NULL,
                       label.hjust = NULL, label.vjust = NULL,
                       label.repel = FALSE, label.show.legend = NA,
                       label.position = "identity") {

  if (!is.data.frame(data)) {
    stop(paste0('Unsupported class: ', class(data)))
  }

  if (!missing(label.colour) && !is.null(label.colour) && missing(label)) {
    # if flag is missing but colour is specified, turn flag to TRUE
    label <- TRUE
  }

  if (label || label.repel) {
    # user wants label if they enables repel
    if (is.null(label.colour)) {
      # NULL may be explicitly passed from parent functions
      label.colour <- '#000000'
    }
    if (label.repel && 'ggrepel' %in% rownames(installed.packages())) {
      textfunc <- ggrepel::geom_text_repel
    } else {
      textfunc <- ggplot2::geom_text
    }
    p <- p + geom_factory(textfunc, data, x = x, y = y,
                          label = label.label,
                          colour = label.colour, alpha = label.alpha,
                          size = label.size, angle = label.angle,
                          family = label.family, fontface = label.fontface,
                          lineheight = label.lineheight,
                          hjust = label.hjust, vjust = label.vjust,
                          position = label.position,
                          show.legend = label.show.legend)
  }
  p
}


#' Apply facets to to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param formula \code{stats::formula} instance
#' @param facets Logical value to specify use facets
#' @param nrow Number of facet/subplot rows
#' @param ncol Number of facet/subplot columns
#' @param scales Scale value passed to \code{ggplot2}
#' @param ... other arguments passed to methods
#' @return ggplot
apply_facets <- function(p, formula, facets = TRUE, nrow = NULL, ncol = 1,
                         scales = 'free_y', ...) {
  if (!is.null(formula)) {
    if (is.character(formula)) {
      formula <- formula(paste('~', formula))
    }
    p <- p + ggplot2::facet_wrap(formula, scales = scales,
                                 nrow = nrow, ncol = ncol)
  }
  return(p)
}


#' Apply grid to to \code{ggplot2::ggplot}
#'
#' @param p \code{ggplot2::ggplot} instance
#' @param formula \code{stats::formula} instance
#' @param scales Scale value passed to \code{ggplot2}
#' @param ... other arguments passed to methods
apply_grid <- function(p, formula, scales = 'free_y', ...) {
  if (!is.null(formula) & deparse(formula) != '. ~ .') {
    p <- p + ggplot2::facet_grid(formula, scales = scales)
  }
  return(p)
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

  if (!(geom %in% allowed)) {
    stop(paste("Invalid geom is specified. Use", paste(allowed, collapse=', ')))
  }
  # ToDo: may better to use layer directly?
  if (geom == 'line') {
    return(ggplot2::geom_line)
  } else if (geom == 'bar') {
    return(ggplot2::geom_bar)
  } else if (geom == 'point') {
    return(ggplot2::geom_point)
  } else if (geom == 'step') {
    return(ggplot2::geom_step)
  } else if (geom == 'ribbon') {
    return(ggplot2::geom_ribbon)
  } else if (geom == 'polygon') {
    return(ggplot2::geom_polygon)
  } else if (geom == 'path') {
    return(ggplot2::geom_path)
  }
  stop(paste("Invalid geom is specified. Use", paste(allowed, collapse=', ')))
}

#' Factory function to control \code{ggplot2::geom_xxx} functions
#'
#' @param geomfunc \code{ggplot2::geom_xxx} function
#' @param data plotting data
#' @param position A position function or character
#' @param ... other arguments passed to methods
#' @return proto
geom_factory <- function(geomfunc, data = NULL, position = NULL, ...) {
  mapping <- list()
  option <- list()

  columns <- colnames(data)
  for (key in names(list(...))) {
      value <- list(...)[[key]]
      if (is.null(value)) {
        # pass
      } else if (!(is.vector(value) && length(value) > 1L) &&
                 value %in% columns) {
        # check for length so that vectors of colours are supported
         mapping[[key]] <- value
      } else {
        option[[key]] <- value
      }
  }
  if (!is.null(data)) {
    option[['data']] <- data
  }
  if (!is.null(position)) {
    option[['position']] <- position
  }
  option[['mapping']] <- do.call(ggplot2::aes_string, mapping)
  return(do.call(geomfunc, option))
}

#' An S4 class to hold multiple \code{ggplot2::ggplot} instances
#'
#' @name ggmultiplot-class
#' @rdname ggmultiplot-class
#' @exportClass ggmultiplot
#'
#' @slot plots List of \code{ggplot2::ggplot} instances
#' @slot ncol Number of grid columns
#' @slot nrow Number of grid rows
setClass('ggmultiplot',
         representation(plots = 'list', ncol = 'numeric', nrow = 'numeric'),
         prototype = list(ncol = 0, nrow = 0))

# ref:
# https://stat.ethz.ch/R-manual/R-devel/library/methods/html/setMethod.html
# https://github.com/variani/pckdev/wiki/Documenting-with-roxygen2

#' Generic add operator for \code{ggmultiplot}
#'
#' @param e1 first argument
#' @param e2 second argument
#' @return \code{ggmultiplot}
setMethod('+', c('ggmultiplot', 'ANY'),
  function(e1, e2) {
    if (is(e2, 'ggmultiplot')) {
      # concat 2 ggmultiplots using lhs ncol / nrow
      plots <- c(e1@plots, e2@plots)
    } else if (is(e2, 'ggplot')) {
      plots <- c(e1@plots, list(e2))
    } else {
      # apply theme / add geom
      plots <- lapply(e1@plots, function(x) { x + e2 })
    }
    new('ggmultiplot', plots = plots,
        ncol = e1@ncol, nrow = e1@nrow)
})

#' @param x \code{ggmultiplot}
#'
#' @rdname ggmultiplot-class
#' @aliases length,ggmultiplot-method
setMethod('length', 'ggmultiplot',
  function(x) {
    return(length(x@plots))
})

#' @param i elements to extract or replace
#' @param j not used
#' @param ... not used
#' @param drop not used
#'
#' @rdname ggmultiplot-class
#' @aliases [,ggmultiplot-method
setMethod("[", signature(x = "ggmultiplot"),
  function(x, i, j, ..., drop) {
    new('ggmultiplot', plots = x@plots[i],
        ncol = x@ncol, nrow = x@nrow)
})

#' @rdname ggmultiplot-class
#' @aliases [[,ggmultiplot-method
setMethod("[[", signature(x = "ggmultiplot"),
  function(x, i, j, ..., drop) {
    x@plots[[i]]
})

#' @param value value to be set
#'
#' @rdname ggmultiplot-class
#' @aliases [<-,ggmultiplot-method
setMethod("[<-", signature(x = "ggmultiplot"),
  function(x, i, j, ..., value) {

    if (is(value, 'ggmultiplot')) {
      if (length(value) == length(x@plots[i])) {
        x@plots[i] <- value@plots
      } else {
        stop(paste('Unable to set value, length mismatch:', length(value)))
      }
    } else if (is(value, 'ggplot')) {
      if (length(x@plots[i]) == 1) {
        x@plots[[i]] <- value
      } else {
        stop(paste('Unable to set ggplot to multiple slice'))
      }
    } else {
      stop(paste('Unable to set type, unsupported type:', class(value)))
    }
    x
})

#' @rdname ggmultiplot-class
#' @aliases [[<-,ggmultiplot-method
setMethod("[[<-", signature(x = "ggmultiplot"),
  function(x, i, j, ..., value) {

    if (is(value, 'ggmultiplot')) {
      if (length(value) == 1) {
        x@plots[[i]] <- value@plots[[1]]
      } else {
        stop(paste('Unable to set value, length mismatch:', length(value)))
      }
    } else if (is(value, 'ggplot')) {
      x@plots[[i]] <- value
    } else {
      stop(paste('Unable to set type, unsupported type:', class(value)))
    }
    x
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
    message <- paste('nrow * ncol (', nrow, ' * ', ncol,
                     ') must be larger than number of plots', nplots)
    stop(message)
  }

  t(matrix(1:(ncol * nrow), ncol = nrow, nrow = ncol))
}

#' Generic print function for \code{ggmultiplot}
#'
#' @param x \code{ggmultiplot}
#' @return NULL
#'
#' @importFrom gridExtra grid.arrange
setMethod('print', 'ggmultiplot',
  function(x) {
    layout <- get.layout(length(x@plots), x@ncol, x@nrow)
    args <- c(x@plots, list(ncol = ncol(layout), nrow = nrow(layout)))
    do.call(gridExtra::grid.arrange, args)
})

#' Generic show function for \code{ggmultiplot}
#'
#' @param object \code{ggmultiplot}
#' @return NULL
setMethod('show', 'ggmultiplot', function(object) { print(object) })

#' The implemented grid.draw method for ggmultiplot, in order to work
#' with ggsave() properly
#'
#' @export
#' @method grid.draw ggmultiplot
#' @importFrom grid grid.draw
#' @importFrom gridExtra arrangeGrob
#' @param plot \code{ggmultiplot}
grid.draw.ggmultiplot <- function(plot) {
  grid::grid.draw(gridExtra::arrangeGrob(grobs = plot@plots))
}

#' Post process for fortify. Based on \code{ggplot2::qplot}
#'
#' @param p \code{ggplot2::ggplot} instances
#' @param xlim limits for x axis
#' @param ylim limits for y axis
#' @param log which variables to log transform ("x", "y", or "xy")
#' @param main character vector or expression for plot title
#' @param xlab character vector or expression for x axis label
#' @param ylab character vector or expression for y axis label
#' @param asp the y/x aspect ratio
#' @return data.frame
#' @examples
#' p <- qplot(Petal.Length, Petal.Width, data = iris)
#' ggfortify:::post_autoplot(p, xlim = c(1, 5), ylim = c(1, 5), log = 'xy', main = 'title',
#'                           xlab = 'x', ylab = 'y', asp = 1.5)
post_autoplot <- function(p, xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                          main = NULL, xlab = NULL, ylab = NULL, asp = NULL) {
  logv <- function(var) var %in% strsplit(log, "")[[1]]
  if (logv("x"))
    p <- p + scale_x_log10()
  if (logv("y"))
    p <- p + scale_y_log10()
  if (!is.null(asp))
    p <- p + ggplot2::theme(aspect.ratio = asp)
  if (!is.null(main))
    p <- p + ggplot2::ggtitle(main)
  if (!is.null(xlab))
    p <- p + ggplot2::xlab(xlab)
  if (!is.null(ylab))
    p <- p + ggplot2::ylab(ylab)
  if (!all(is.na(xlim)))
    p <- p + ggplot2::xlim(xlim)
  if (!all(is.na(ylim)))
    p <- p + ggplot2::ylim(ylim)
  p
}

#' Check if passed object is supported by \code{ggplot2::autoplot}
#'
#' @param obj object
#' @return logical
support_autoplot <- function(obj) {
  maybe_autoplot <- paste0('autoplot.', class(obj))
  return(any(sapply(maybe_autoplot, function(x) x %in% utils::methods('autoplot'))))
}


#' Autoplot \code{ggplot} instances.
#' It returns the passed instance as it is.
#'
#' @param object ggplot instance
#' @param ... Not used.
#' @return ggplot
#' @export
autoplot.ggplot <- function(object, ...) {
  return (object)
}

#' Autoplot \code{ggmultiplot} instances.
#' It returns the passed instance as it is.
#'
#' @param object ggmultiplot instance
#' @param ... Not used.
#' @return ggmultiplot
#' @export
autoplot.ggmultiplot <- function(object, ...) {
  return (object)
}

#' Draw \code{biplot} using \code{ggplot2}.
#'
#' @param plot.data data.frame
#' @param loadings.data data.frame
#' @param colour colour
#' @param size size
#' @param linetype line type
#' @param alpha alpha
#' @param fill fill
#' @param shape shape
#' @param label Logical value whether to display data labels
#' @inheritParams plot_label
#' @param loadings Logical value whether to display loadings arrows
#' @param loadings.arrow An arrow definition
#' @param loadings.colour Point colour for data
#' @param loadings.label Logical value whether to display loadings labels
#' @param loadings.label.label Column name used for loadings text labels
#' @param loadings.label.colour Colour for loadings text labels
#' @param loadings.label.alpha Alpha for loadings text labels
#' @param loadings.label.size Size for loadings text labels
#' @param loadings.label.angle Angle for loadings text labels
#' @param loadings.label.family Font family for loadings text labels
#' @param loadings.label.fontface Fontface for loadings text labels
#' @param loadings.label.lineheight Lineheight for loadings text labels
#' @param loadings.label.hjust Horizontal adjustment for loadings text labels
#' @param loadings.label.vjust Vertical adjustment for loadings text labels
#' @param loadings.label.repel Logical flag indicating whether to use \code{ggrepel} automatically
#' @param label.show.legend Logical value indicating whether to show the legend of text labels
#' @param frame Logical value whether to draw outliner convex / ellipse
#' @param frame.type Character specifying frame type.
#' 'convex' or types supporeted by \code{ggplot2::stat_ellipse} can be used.
#' @param frame.colour Colour for frame
#' @param frame.level Passed for \code{ggplot2::stat_ellipse} 's level. Ignored in 'convex'.
#' @param frame.alpha Alpha for frame
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @export
ggbiplot <- function(plot.data, loadings.data = NULL,
                     colour = NULL, size = NULL, linetype = NULL,
                     alpha = NULL, fill = NULL, shape = NULL,
                     label = FALSE,
                     label.label = 'rownames',
                     label.colour = colour,
                     label.alpha = NULL,
                     label.size = NULL,
                     label.angle = NULL,
                     label.family = NULL,
                     label.fontface = NULL,
                     label.lineheight = NULL,
                     label.hjust = NULL,
                     label.vjust = NULL,
                     label.repel = FALSE,
                     label.position = "identity",
                     loadings = FALSE,
                     loadings.arrow = grid::arrow(length = grid::unit(8, 'points')),
                     loadings.colour = '#FF0000',
                     loadings.label = FALSE,
                     loadings.label.label = 'rownames',
                     loadings.label.colour = '#FF0000',
                     loadings.label.alpha = NULL,
                     loadings.label.size = NULL,
                     loadings.label.angle = NULL,
                     loadings.label.family = NULL,
                     loadings.label.fontface = NULL,
                     loadings.label.lineheight = NULL,
                     loadings.label.hjust = NULL,
                     loadings.label.vjust = NULL,
                     loadings.label.repel = FALSE,
                     label.show.legend = NA,
                     frame = FALSE, frame.type = NULL,
                     frame.colour = colour, frame.level = 0.95,
                     frame.alpha = 0.2,
                     xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                     main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                     ...) {
#  print(label.position)

  plot.columns <- colnames(plot.data)
  mapping <- ggplot2::aes_string(x = plot.columns[1L], y = plot.columns[2L])

  if (is.logical(shape) && !shape && missing(label)) {
    # if shape=FALSE, turn label to TRUE
    label <- TRUE
  }

  p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
  if (!is.logical(shape) || shape) {
    p <- p + geom_factory(ggplot2::geom_point, plot.data,
                          colour = colour, size = size, linetype = linetype,
                          alpha = alpha, fill = fill, shape = shape)
  }
  p <- plot_label(p = p, data = plot.data, label = label,
                  label.label = label.label,
                  label.colour = label.colour,
                  label.alpha = label.alpha,
                  label.size = label.size,
                  label.angle = label.angle,
                  label.family = label.family,
                  label.fontface = label.fontface,
                  label.lineheight = label.lineheight,
                  label.hjust = label.hjust,
                  label.vjust = label.vjust,
                  label.repel = label.repel,
                  label.show.legend = label.show.legend,
                  label.position = label.position)

  if (loadings.label && !loadings) {
    # If loadings.label is TRUE, draw loadings
    loadings <- TRUE
  }
  if (loadings && !is.null(loadings.data)) {

    scaler <- min(max(abs(plot.data[, 1L])) / max(abs(loadings.data[, 1L])),
                  max(abs(plot.data[, 2L])) / max(abs(loadings.data[, 2L])))

    loadings.columns <- colnames(loadings.data)
    loadings.mapping <- ggplot2::aes_string(x = 0, y = 0,
                                            xend = loadings.columns[1L],
                                            yend = loadings.columns[2L])
    loadings.data[, 1L:2L] <- loadings.data[, 1L:2L] * scaler * 0.8

    p <- p + geom_segment(data = loadings.data,
                          mapping = loadings.mapping,
                          arrow = loadings.arrow,
                          colour = loadings.colour)
    p <- plot_label(p = p, data = loadings.data, label = loadings.label,
                    label.label = loadings.label.label,
                    label.colour = loadings.label.colour,
                    label.alpha = loadings.label.alpha,
                    label.size = loadings.label.size,
                    label.angle = loadings.label.angle,
                    label.family = loadings.label.family,
                    label.fontface = loadings.label.fontface,
                    label.lineheight = loadings.label.lineheight,
                    label.hjust = loadings.label.hjust,
                    label.vjust = loadings.label.vjust,
                    label.repel = loadings.label.repel,
                    label.show.legend = label.show.legend,
                    label.position = label.position)
  }

  if (missing(frame) && !is.null(frame.type)) {
    # if frame is missing but frame.type is specified, turn frame to TRUE
    frame <- TRUE
  }

  # dummy to solve "no visible binding for global variable '.'" warnings
  . <- NULL

  if (frame) {
    if (is.null(frame.type) || frame.type == 'convex') {
      if (is.null(frame.colour) || !(frame.colour %in% colnames(plot.data))) {
        hulls <- plot.data[grDevices::chull(plot.data[, 1L:2L]), ]
      } else {
        hulls <- plot.data %>%
          dplyr::group_by(.data[[frame.colour]]) %>%
          dplyr::do(.[grDevices::chull(.[, 1L:2L]), ])
      }
      mapping <- aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::geom_polygon(data = hulls, mapping = mapping,
                                     alpha = frame.alpha)
    } else if (frame.type %in% c('t', 'norm', 'euclid')) {
      mapping <- aes_string(colour = frame.colour, fill = frame.colour)
      p <- p + ggplot2::stat_ellipse(mapping = mapping,
                                     level = frame.level, type = frame.type,
                                     geom = 'polygon', alpha = frame.alpha)
    } else {
      stop('frame.type must be convex, t, norm or euclid')
    }
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  return(p)
}
