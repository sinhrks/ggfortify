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
    d <- d[order(d$cluster), ]
    d$id <- seq_len(nrow(d))
  } else {
    stop(paste0('Unsupported class for fortify.silhouette: ', class(model)))
  }
  post_fortify(d)
}

#' Autoplot silhouette instances
#'
#' @param object Silhouette instance
#' @param line.color reference line color
#' @param line.type reference line type
#' @param line.size reference line size
#' @param bar.width bar width
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
autoplot.silhouette <- function(object, line.color = "red",
                                line.type = "dashed", line.size = 0.5,
                                bar.width = 1, ...) {

  if (!is_derived_from(object, 'silhouette')) {
    stop(paste0('Unsupported class for autoplot.silhouette: ', class(object)))
  }

  plot.data <- ggplot2::fortify(object)

  min.y <- if(min(plot.data$sil_width) < 0) min(plot.data$sil_width) else 0
  yinter <- round(mean(plot.data$sil_width), 2)
  p <- ggplot(plot.data, aes(x = id, y = sil_width, fill = cluster)) +
    geom_bar(stat = "identity", width = bar.width) +
    ylim(min.y, 1) +
    labs(x = "Observations", y = "Silhouette Values") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5)) +
    geom_hline(yintercept = yinter, color = line.color, linetype = line.type,
               size = line.size) +
    coord_flip()
  p
}
