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
#' ggfortify:::plot.conf.int(p)
plot.conf.int <- function (p, data = NULL, lower = 'lower', upper = 'upper',
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

  if (conf.int) {
    mapping_ribbon = ggplot2::aes_string(ymin = lower, ymax = upper)
    mapping_lower = ggplot2::aes_string(y = lower)
    mapping_upper = ggplot2::aes_string(y = upper)
    if (!is.null(conf.int.fill)) {
      p <- p + ggplot2::geom_ribbon(data = data,
                                    mapping = mapping_ribbon,
                                    fill = conf.int.fill, alpha = conf.int.alpha)
    }
    if (conf.int.linetype != 'none') {
      p <- p + ggplot2::geom_line(data = data,
                                  mapping = mapping_lower,
                                  colour = conf.int.colour, linetype = conf.int.linetype,
                                  na.rm = TRUE) +
        ggplot2::geom_line(data = data,
                           mapping = mapping_upper,
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
  if (flag) {
    if (is.null(colour)) {
      # NULL may be explicitly passed from parent functions
      colour <- '#000000'
    }
    if (colour %in% colnames(data)) {
      mapping <- ggplot2::aes_string(label = label, colour = colour)
      p <- p + ggplot2::geom_text(data = data, mapping = mapping,
                                  size = size)
    } else {
      mapping <- ggplot2::aes_string(label = label)
      p <- p + ggplot2::geom_text(data = data, mapping = mapping,
                                  colour = colour, size = size)
    }
  }
  p
}

#' An S4 class to hold multiple \code{ggplot2::ggplot} instances.
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
  # Based on Multiple plot function
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)

  nplots = length(p@plots)

  if (nplots==1) {
    print(p@plots[[1]])
  } else {
    layout <- get.layout(nplots, p@ncol, p@nrow)
    grid::grid.newpage()
    vp <- grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout)))
    grid::pushViewport(vp)

    for (i in 1:nplots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      vp <- grid::viewport(layout.pos.row = matchidx$row,
                           layout.pos.col = matchidx$col)
      print(p@plots[[i]], vp = vp)
    }
  }
}

#' Generic show function for \code{ggmultiplot}
#'
#' @param object \code{ggmultiplot}
#' @return NULL
setMethod('show', 'ggmultiplot',
          function(object) { print(object) })

