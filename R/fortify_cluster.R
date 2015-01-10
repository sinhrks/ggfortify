#' Convert \code{stats::kmeans}, \code{cluster::clara}, \code{cluster::fanny} and
#' \code{cluster::pam} to data.frame.
#' 
#' @param data Clustered instance
#' @param original Joined to data if provided. Intended to be used for attaching
#' cluster labels to the original
#' @return data.frame
#' @examples
#' df <- iris[-5]
#' ggplot2::fortify(stats::kmeans(df, 3))
#' ggplot2::fortify(stats::kmeans(df, 3), original = iris)
#' ggplot2::fortify(cluster::clara(df, 3))
#' ggplot2::fortify(cluster::fanny(df, 3))
#' ggplot2::fortify(cluster::pam(df, 3), original = iris)
#' @export
fortify.kmeans <- function(data, original = NULL) {
  if (is(data, 'kmeans')) {
    d <- data.frame(cluster = as.factor(data$cluster))
  } else if (is(data, 'partition')) {
    d <- data.frame(cluster = as.factor(data$cluster),
                    ggplot2::fortify(data$data))
  } else {
    stop(paste0('Unsupported class for fortify.kmeans: ', class(data)))
  }
  d <- cbind_wraps(d, original)
  dplyr::tbl_df(d)
}


#' Autoplot \code{stats::kmeans}, \code{cluster::clara}, \code{cluster::fanny} and
#' \code{cluster::pam}.
#' 
#' @param data Clustered instance
#' @param original Original data used for clustering. Mandatory for \code{stats::kmeans}.
#' @param ... Options supported in \code{autoplot::prcomp}
#' @return ggplot
#' @examples
#' df <- iris[-5]
#' ggplot2::autoplot(stats::kmeans(df, 3), original = iris)
#' ggplot2::autoplot(cluster::clara(df, 3), label = TRUE)
#' ggplot2::autoplot(cluster::fanny(df, 3))
#' ggplot2::autoplot(cluster::fanny(df, 3), frame = TRUE)
#' ggplot2::autoplot(cluster::pam(df, 3), original = iris)
#' ggplot2::autoplot(cluster::pam(df, 3), original = iris, frame = TRUE, frame.type = 't')
#' @export
autoplot.kmeans <- function(data, original = NULL, ...) {
  if (is(data, 'kmeans')) {
    if (is.null(original)) {
      stop("'original' data is mandatory for plotting kmeans instance")
    }
    dots <- colnames(data$center)
  } else if (is(data, 'partition')) {
    dots <- colnames(data$data)
  } else {
    stop(paste0('Unsupported class for autoplot.kmeans: ', class(data)))
  }
  plot.data <- ggplot2::fortify(data, original = original)
  plot.data$rownames <- rownames(plot.data)

  pca.data <- dplyr::select_(plot.data, .dots = dots)
  p <- ggplot2::autoplot(stats::prcomp(pca.data), original = plot.data,
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