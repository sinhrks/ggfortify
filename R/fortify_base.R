#' Convert \code{base::table} to \code{data.frame}
#'
#' @param model \code{base::table} instance
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return data.frame
fortify_base <- function(model, data, ...) {
  # the function should be base for all fortify roxygen
  as.data.frame(model)
}

#' Convert \code{base::table} to \code{data.frame}
#'
#' @param model \code{base::table} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(Titanic)
#' @export
fortify.table <- function(model, data, ...) {
  fortify_base(model, data, ...)
}

#' Convert \code{base::matrix} to \code{data.frame}
#'
#' Different from \code{as.data.frame}
#'
#' @param model \code{base::matrix} instance
#' @param data original dataset, if needed
#' @param compat Logical frag to specify the behaviour when converting matrix which has no column name.
#' If \code{FALSE}, result has character columns like c('1', '2', ...).
#' If \code{TRUE}, result has character columns like c('V1', 'V2', ...).
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' fortify(matrix(1:6, nrow=2, ncol=3))
#' @export
fortify.matrix <- function(model, data = NULL, compat = FALSE, ...) {
  d <- as.data.frame(model)
  if ( (!compat) && is.null(colnames(model)) ) {
    # set numeric column names
    colnames(d) <- 1:ncol(model)
  }
  # dplyr doesn't guarantee rownames
  d <- cbind_wraps(d, data)
  post_fortify(d)
}

#' Plot \code{base::matrix}
#'
#' @param object \code{base::matrix} instance
#' @param original Combined to data by column if provided. Intended to be used for stat functions which
#' returns not containing original data.
#' @param geom Geometric string for plotting. 'tile' or 'point'.
#' @param colour colour for points ('point' only)
#' @param size point size
#' @param alpha alpha
#' @param fill fill colour. Ignored if scale keyword is passed. ('tile' Only)
#' @param shape point shape
#' @inheritParams plot_label
#' @param scale (Deprecated) \code{ggplot2::scale} instance to plot. ('tile' Only)
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(matrix(rnorm(20), nc = 5))
#' autoplot(matrix(rnorm(20), nc = 5), fill = 'red')
#' autoplot(matrix(rnorm(20), nc = 2), geom = 'point')
#' @export
autoplot.matrix <- function (object, original = NULL, geom = 'tile',
                             colour = NULL, size = NULL, alpha = NULL,
                             fill = '#0000FF', shape = NULL,
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
                             scale = NULL,
                             xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                             main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                             ...) {
  if (geom == 'tile') {
    fortified <- ggplot2::fortify(object, original = original)
    fortified$Index <- rownames(fortified)
    cols <- colnames(fortified)
    gathered <- tidyr::gather_(fortified, 'variable', 'value',
                               cols[cols != 'Index'])

    if (is.null(scale)) {
      scale <- ggplot2::scale_fill_gradient(low = "white", high = fill)
    } else {
      message <- paste0("Argument 'scale' is being deprecated. Use + operator to returned ggplot instance.")
      warning(message, call. = FALSE)
    }
    ylim <- rev(levels(as.factor(gathered$Index)))
    mapping <- ggplot2::aes_string(x = 'variable', y = 'Index', fill = 'value')
    p <- ggplot2::ggplot(mapping = mapping) +
      geom_factory(geom_tile, gathered, alpha = alpha) +
      scale +
      xlab('Columns') + ylab('Rows') + ylim(ylim)
  } else if (geom == 'point') {
    if (ncol(object) != 2) {
      stop("Number of columns must be 2 to plot with 'geom = point'")
    }
    if (is.logical(shape) && !shape && missing(label)) {
      # if label is missing and shape=FALSE, turn label to TRUE
      label <- TRUE
    }
    plot.data <- ggplot2::fortify(object, original = original, compat = TRUE)
    plot.data$rownames <- rownames(plot.data)
    mapping <- ggplot2::aes_string(x = 'V1', y = 'V2')
    p <- ggplot2::ggplot(mapping = mapping)
    if (!is.logical(shape) || shape) {
      p <- p + geom_factory(geom_point, plot.data, colour = colour, size = size,
                            alpha = alpha, fill = fill, shape = shape)
    }
    p <- plot_label(p = p, data = plot.data,
                    label = label,
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
                    label.repel = label.repel)
  } else {
    stop("Invalid geom is specified. Use 'tile' or 'point'.")
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
