#' Connect observations by stairs.
#'
#' @param mapping the aesthetic mapping
#' @param data a layer specific dataset
#' @param stat the statistical transformation to use on the data for this layer
#' @param position the position adjustment to use for overlapping points on this layer
#' @param na.rm logical frag whether silently remove missing values
geom_confint <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, ...) {
  GeomConfint$new(mapping = mapping, data = data, stat = stat,
                  position = position, na.rm = na.rm, ...)
}

GeomConfint <- proto(ggplot2:::GeomRibbon, {
  # mostly derived from ggplot2
  # - geom-path-step.r
  # - geom-ribbon.r
  # Authors: Hadley Wickham, Winston Chang
  # License: GPL-2
  objname <- "confint"
  required_aes <- c("x", "ymin", "ymax")

  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    if (na.rm) data <- data[complete.cases(data[required_aes]), ]
    data <- data[order(data$group, data$x), ]
    data <- .$stairstep_confint(data)
    ggplot2:::GeomRibbon$draw(data, scales, coordinates, na.rm = FALSE, ...)
  }
  stairstep_confint <- function (., data) {
    data <- as.data.frame(data)[order(data$x), ]
    n <- nrow(data)
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
    data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
               data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
  }
})
