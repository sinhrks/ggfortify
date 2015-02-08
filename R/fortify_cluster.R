#' Convert \code{stats::kmeans}, \code{cluster::clara}, \code{cluster::fanny} and
#' \code{cluster::pam} to data.frame.
#'
#' @param model Clustered instance
#' @param data original dataset, if needed
#' @param original (Deprecated) use data
#' @param ... other arguments passed to methods
#' @return data.frame
#' @examples
#' df <- iris[-5]
#' ggplot2::fortify(stats::kmeans(df, 3))
#' ggplot2::fortify(stats::kmeans(df, 3), data = iris)
#' ggplot2::fortify(cluster::clara(df, 3))
#' ggplot2::fortify(cluster::fanny(df, 3))
#' ggplot2::fortify(cluster::pam(df, 3), data = iris)
#' @export
fortify.kmeans <- function(model, data = NULL, original = NULL, ...) {

  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }

  if (is(model, 'kmeans')) {
    d <- data.frame(cluster = as.factor(model$cluster))
  } else if (is(model, 'partition')) {
    d <- data.frame(cluster = as.factor(model$cluster),
                    ggplot2::fortify(model$data))
  } else {
    stop(paste0('Unsupported class for fortify.kmeans: ', class(model)))
  }
  d <- cbind_wraps(d, data)
  dplyr::tbl_df(d)
}


#' Autoplot \code{stats::kmeans}, \code{cluster::clara}, \code{cluster::fanny} and
#' \code{cluster::pam}.
#'
#' @param object Clustered instance
#' @param data Original data used for clustering. Mandatory for \code{stats::kmeans}.
#' @param original (Deprecated) use data
#' @param ... other arguments passed to \code{autoplot::prcomp}
#' @return ggplot
#' @examples
#' df <- iris[-5]
#' ggplot2::autoplot(stats::kmeans(df, 3), data = iris)
#' ggplot2::autoplot(cluster::clara(df, 3), label = TRUE)
#' ggplot2::autoplot(cluster::fanny(df, 3))
#' ggplot2::autoplot(cluster::fanny(df, 3), frame = TRUE)
#' ggplot2::autoplot(cluster::pam(df, 3), data = iris)
#' ggplot2::autoplot(cluster::pam(df, 3), data = iris, frame = TRUE, frame.type = 't')
#' @export
autoplot.kmeans <- function(object, data = NULL, original = NULL, ...) {

  if (!is.null(original)) {
    deprecate.warning('original', 'data')
    data <- original
  }

  if (is(object, 'kmeans')) {
    if (is.null(data)) {
      stop("'data' is mandatory for plotting kmeans instance")
    }
    dots <- colnames(object$center)
  } else if (is(object, 'partition')) {
    dots <- colnames(object$data)
  } else {
    stop(paste0('Unsupported class for autoplot.kmeans: ', class(object)))
  }
  plot.data <- ggplot2::fortify(object, data = data)
  plot.data$rownames <- rownames(plot.data)

  pca.data <- dplyr::select_(plot.data, .dots = dots)
  p <- ggplot2::autoplot(stats::prcomp(pca.data), data = plot.data,
                         colour = 'cluster', ...)
  p
}

#' @export
fortify.partition <- fortify.kmeans

#' @export
fortify.clara <- fortify.partition

#' @export
fortify.fanny <- fortify.partition

#' @export
fortify.pam <- fortify.partition

#' @export
autoplot.partition <- autoplot.kmeans

#' @export
autoplot.clara <- autoplot.partition

#' @export
autoplot.fanny <- autoplot.partition

# @export
autoplot.pam <- autoplot.partition
