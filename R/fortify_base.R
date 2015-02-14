#' Convert \code{base::table} to data.frame.
#'
#' @param model \code{base::table} instance
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' fortify(Titanic)
#' @export
fortify.table <- function(model, data, ...) {
  as.data.frame(model)
}

#' Convert \code{base::matrix} to data.frame.
#'
#' Different from \code{as.data.frame},
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
  if ((!compat) && is.null(colnames(model))) {
    # set numeric column names
    colnames(d) <- 1:ncol(model)
  }
  # dplyr doesn't guarantee rownames
  d <- cbind_wraps(d, data)
  post.fortify(d)
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
#' @param label logical value whether to display data labels ('point' Only)
#' @param label.colour Text colour for data labels ('point' Only)
#' @param label.size Text size for data labels ('point' Only)

#' @param scale (Deprecated) \code{ggplot2::scale} instance to plot. ('tile' Only)
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
                             label = FALSE, label.colour = colour, label.size = 4,
                             scale = NULL, ...) {
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
    mapping = ggplot2::aes_string(x = 'variable', y = 'Index', fill = 'value')
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
    mapping = ggplot2::aes_string(x = 'V1', y = 'V2')
    p <- ggplot2::ggplot(mapping = mapping)
    if (!is.logical(shape) || shape) {
      p <- p + geom_factory(geom_point, plot.data, colour = colour, size = size,
                            alpha = alpha, fill = fill, shape = shape)
    }
    p <- plot.label(p = p, data = plot.data, flag = label, label = 'rownames',
                    colour = label.colour, size = label.size)
  } else {
    stop("Invalid geom is specified. Use 'tile' or 'point'.")
  }
  p
}
