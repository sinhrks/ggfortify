#' Convert \code{ROCR::performance} objects to \code{data.frame}
#'
#' @param model \code{performance} instances
#' @inheritParams fortify_base
#' @return data.frame
#' @export
fortify.performance <- function(model, data = NULL, ...) {
  if (is(model, 'performance')) {

    ## Get metadata

    # number of repetitions (bootstrapped, CV, etc)
    n_rep <- length(model@y.values)
    # repetition names
    rep_names <- paste0('Rep', seq(n_rep))
    # length of each repetition
    rep_lengths <- vapply(model@y.values, length, integer(1))

    ## Extract values

    # unlist in long form
    df <- data.frame(y = unlist(model@y.values))
    # label each data point with its repetition name
    df$repn <- as.factor(rep(rep_names, rep_lengths))
    # add x and alpha, which may or may not be there
    df$x <- unlist(model@x.values)
    df$alpha <- unlist(model@alpha.values)

    # Rename/reorder cols
    renames <- c(repn = 'Repetition.Number',
                 x = make.names(model@x.name),
                 y = make.names(model@y.name),
                 alpha = make.names(model@alpha.name))

    names(df) <- renames[names(df)]
    renames <- intersect(renames, names(df))
    df <- df[, renames]

  } else {
    stop(paste0('Unsupported class for fortify.performance: ', class(model)))
  }

  post_fortify(df, klass = model)
}




#' Autoplot \code{ROCR::performance}
#'
#' @param object \code{ROCR::performance} instance
#' @param p \code{ggplot2::ggplot} instances
#' @param bins If \code{object} represents a measure whose value is just a
#'        scalar (e.g. \code{performance(predObj, 'auc')}), a histogram will be
#'        plotted of this scalar's values for different runs. \code{bins}
#'        is the number of bins for this histogram.
#' @param ... other arguments passed to methods
#' @return ggplot
#' @export
autoplot.performance <- function(object, p = NULL,
                                 bins = 5, ...) {

  plot.data <- ggplot2::fortify(object)
  plot.names <- names(plot.data)

  # Three different plots depending on how many columns there are (2, 3, or 4)

  if (length(plot.names) == 2) {

    if (nrow(plot.data) == 1) {
      warning(paste('This histogram is more useful with multiple runs.',
                    'See ?ROCR::prediction'))
    }

    if (is.null(p)) {
      p <- geom_factory(ggplot2::ggplot, data = plot.data,
                        x = plot.names[2])
    }

    p <- p + geom_factory(ggplot2::geom_histogram, data = plot.data,
                          x = plot.names[2], bins = bins)
    p <- p + ggplot2::ggtitle(paste('Histogram of', plot.names[2]))

  } else if (length(plot.names) == 3) {

    if (is.null(p)) {
      p <- geom_factory(ggplot2::ggplot, data = plot.data,
                        x = plot.names[2], y = plot.names[3],
                        group = plot.names[1])
    }

    p <- p + geom_factory(ggplot2::geom_line, data = plot.data,
                          x = plot.names[2], y = plot.names[3],
                          group = plot.names[1])
    p <- p + ggplot2::ggtitle(paste(plot.names[3], 'vs', plot.names[2]))

  } else if (length(plot.names) == 4) {

    if (is.null(p)) {
      p <- geom_factory(ggplot2::ggplot, data = plot.data,
                        x = plot.names[2], y = plot.names[3],
                        group = plot.names[1], col = plot.names[4])
    }

    p <- p + geom_factory(ggplot2::geom_line, data = plot.data,
                          x = plot.names[2], y = plot.names[3],
                          group = plot.names[1], col = plot.names[4])
    p <- p + ggplot2::ggtitle(paste(plot.names[3], 'vs', plot.names[2]))

  }

  p <- post_autoplot(p = p, ...)
  return(p)
}
