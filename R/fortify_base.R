#' Convert \code{base::table} to data.frame.
#'
#' @param model \code{base::table} instance
#' @param data original dataset, if needed
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' ggplot2::fortify(Titanic)
#' @export
fortify.table <- function(model, data, ...) {
  dplyr::tbl_df(as.data.frame(model))
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
#' ggplot2::fortify(matrix(1:6, nrow=2, ncol=3))
#' @export
fortify.matrix <- function(model, data = NULL, compat = FALSE, ...) {
  df <- as.data.frame(model)
  if ((!compat) && is.null(colnames(model))) {
    # set numeric column names
    colnames(df) <- 1:ncol(model)
  }
  # dplyr doesn't guarantee rownames
  df <- cbind_wraps(df, data)
  df <- dplyr::tbl_df(df)
}

#' Plot \code{base::matrix}
#'
#' @param object \code{base::matrix} instance
#' @param original Combined to data by column if provided. Intended to be used for stat functions which
#' returns not containing original data.
#' @param fill Fill (high) colour. Ignored if scale keyword is passed. ('tile' Only)
#' @param scale \code{ggplot2::scale} instance to plot. ('tile' Only)
#' @param colour Column name string to specify colorize points ('point' Only)
#' @param label Logical value whether to display data labels ('point' Only)
#' @param label.colour Text colour for data labels ('point' Only)
#' @param label.size Text size for data labels ('point' Only)
#' @param geom Geometric string for plotting. 'tile' or 'point'.
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' ggplot2::autoplot(matrix(rnorm(20), nc = 5))
#' ggplot2::autoplot(matrix(rnorm(20), nc = 5), fill = 'red')
#' ggplot2::autoplot(matrix(rnorm(20), nc = 5),
#'                   scale = ggplot2::scale_fill_gradient(low = 'red', high = 'blue'))
#' ggplot2::autoplot(matrix(rnorm(20), nc = 2), geom = 'point')
#' @export
autoplot.matrix <- function (object, original = NULL,
                             fill = '#0000FF', scale = NULL,
                             colour = NULL,
                             label = FALSE, label.colour = colour, label.size = 4,
                             geom = 'tile', ...) {
  if (geom == 'tile') {
    df <- ggplot2::fortify(object, original = original)
    df$Index <- rownames(df)
    cols <- colnames(df)
    gathered <- tidyr::gather_(df, 'variable', 'value', cols[cols != 'Index'])

    if (is.null(scale)) {
      scale <- ggplot2::scale_fill_gradient(low = "white", high = fill)
    }
    ylim <- rev(levels(as.factor(gathered$Index)))
    mapping = ggplot2::aes_string(x = 'variable', y = 'Index', fill = 'value')
    p <- ggplot2::ggplot(gathered, mapping = mapping) +
      ggplot2::geom_tile() + scale +
      xlab('Columns') + ylab('Rows') + ylim(ylim)
  } else if (geom == 'point') {
    if (ncol(object) != 2) {
      stop("Number of columns must be 2 to plot with 'geom = point'")
    }
    plot.data <- ggplot2::fortify(object, original = original, compat = TRUE)
    plot.data$rownames <- rownames(plot.data)
    mapping = ggplot2::aes_string(x = 'V1', y = 'V2')
    p <- ggplot2::ggplot(plot.data, mapping = mapping)
    if (!is.null(colour)) {
      p <- p + ggplot2::geom_point(colour = colour)
    } else {
      p <- p + ggplot2::geom_point()
    }
    p <- plot.label(p = p, data = plot.data, flag = label, label = 'rownames',
                    colour = label.colour, size = label.size)
  } else {
    stop("Invalid geom is specified. Use 'tile' or 'point'.")
  }
  p
}
