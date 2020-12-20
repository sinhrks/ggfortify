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
  pca.data <- dplyr::select(plot.data, all_of(dots))
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

#' Convert \code{cluster::silhouette} to \code{data.frame}
#'
#' @param model Silhouette instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(cluster::silhouette(cluster::pam(iris[-5], 3)))
#' fortify(cluster::silhouette(cluster::clara(iris[-5], 3)))
#' fortify(cluster::silhouette(cluster::fanny(iris[-5], 3)))
#'
#' mod = stats::kmeans(iris[-5], 3)
#' fortify(cluster::silhouette(mod$cluster, stats::dist(iris[-5])))
#' @export
fortify.silhouette <- function(model, data = NULL, ...) {

  if (is(model, 'silhouette')) {
    d <- as.data.frame(unclass(model))
    d <- data.frame(cluster = as.factor(d$cluster),
                    sil_width = d$sil_width)
    d <- d[order(d$cluster, d$sil_width), ]
    d$id <- seq_len(nrow(d))
  } else {
    stop(paste0('Unsupported class for fortify.silhouette: ', class(model)))
  }
  post_fortify(d)
}

#' Autoplot silhouette instances
#'
#' @param object Silhouette instance
#' @param colour reference line color
#' @param linetype reference line type
#' @param size reference line size
#' @param bar.width bar width
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' model = cluster::pam(iris[-5], 3L)
#' sil = cluster::silhouette(model)
#' autoplot(sil)
#'
#' autoplot(cluster::silhouette(cluster::clara(iris[-5], 3)))
#' autoplot(cluster::silhouette(cluster::fanny(iris[-5], 3)))
#'
#' model = stats::kmeans(iris[-5], 3)
#' sil = cluster::silhouette(model$cluster, stats::dist(iris[-5]))
#' autoplot(sil)
#' @export
autoplot.silhouette <- function(object, colour = "red",
                                linetype = "dashed", size = 0.5,
                                bar.width = 1, ...) {

  if (!is_derived_from(object, 'silhouette')) {
    stop(paste0('Unsupported class for autoplot.silhouette: ', class(object)))
  }

  plot.data <- ggplot2::fortify(object)
  mapping <- aes_string(x = 'id', y = 'sil_width', fill = 'cluster')

  min.y <- if(min(plot.data$sil_width) < 0) min(plot.data$sil_width) else 0
  ylim <- c(min.y, 1)
  yinter <- round(mean(plot.data$sil_width), 2)

  geomfunc <- get_geom_function("bar")

  p <- ggplot(plot.data, mapping = mapping)
  p <- p + geom_factory(geomfunc, plot.data, stat = "identity",
                        width = bar.width) +
    geom_hline(yintercept = yinter, colour = colour,
                      linetype = linetype, size = size)

  p <- post_autoplot(p = p, ylim = ylim, xlab = "Observations", ylab = "Silhouette Values") +
    coord_flip()
  p
}
