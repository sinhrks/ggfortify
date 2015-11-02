#' Convert cluster instances to \code{data.frame}
#'
#' @param model Clustered instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(stats::kmeans(iris[-5], 3))
#' fortify(stats::kmeans(iris[-5], 3), data = iris)
#' fortify(cluster::clara(iris[-5], 3))
#' fortify(cluster::fanny(iris[-5], 3))
#' fortify(cluster::pam(iris[-5], 3), data = iris)
#' @export
fortify.kmeans <- function(model, data = NULL, ...) {

  if (is(model, 'kmeans')) {
    d <- data.frame(cluster = as.factor(model$cluster))
  } else if (is(model, 'partition')) {
    d <- data.frame(ggplot2::fortify(model$data),
                    cluster = as.factor(model$cluster))
  } else {
    stop(paste0('Unsupported class for fortify.kmeans: ', class(model)))
  }
  d <- cbind_wraps(data, d)
  post_fortify(d)
}


#' Autoplot cluster instances
#'
#' @param object Clustered instance
#' @param data Original data used for clustering. Mandatory for \code{stats::kmeans}.
#' @param colour line colour for points
#' @param ... other arguments passed to \code{autoplot::prcomp}
#' @return ggplot
#' @examples
#' autoplot(stats::kmeans(iris[-5], 3), data = iris)
#' autoplot(cluster::clara(iris[-5], 3), label = TRUE)
#' autoplot(cluster::fanny(iris[-5], 3))
#' autoplot(cluster::fanny(iris[-5], 3), frame = TRUE)
#' autoplot(cluster::pam(iris[-5], 3), data = iris, colour = 'Species')
#' autoplot(cluster::pam(iris[-5], 3), data = iris, frame = TRUE, frame.type = 't')
#' @export
autoplot.kmeans <- function(object, data = NULL,
                            colour = 'cluster', ...) {

  if (is_derived_from(object, 'kmeans')) {
    if (is.null(data)) {
      stop("'data' is mandatory for plotting kmeans instance")
    }
    dots <- 'centers'
  } else if (is_derived_from(object, 'partition')) {
    dots <- 'data'
  } else {
    stop(paste0('Unsupported class for autoplot.kmeans: ', class(object)))
  }

  plot.data <- ggplot2::fortify(object, data = data)
  dots <- colnames(object[[dots]])
  plot.data$rownames <- rownames(plot.data)

  pca.data <- dplyr::select_(plot.data, .dots = dots)
  p <- ggplot2::autoplot(stats::prcomp(pca.data), data = plot.data,
                         colour = colour, ...)
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
