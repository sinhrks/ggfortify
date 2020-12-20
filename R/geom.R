#' Connect observations by stairs.
#'
#' @param mapping the aesthetic mapping
#' @param data a layer specific dataset
#' @param stat the statistical transformation to use on the data for this layer
#' @param position the position adjustment to use for overlapping points on this layer
#' @param na.rm logical frag whether silently remove missing values
#' @param ... other arguments passed to methods
geom_confint <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, ...) {
  ggplot2::layer(mapping = mapping,
                 data = data,
                 stat = stat,
                 geom = GeomConfint,
                 position = position,
                 params = list(na.rm = na.rm, ...))
}

GeomConfint <- ggplot2::ggproto('GeomConfint', ggplot2::GeomRibbon,
  required_aes = c("x", "ymin", "ymax"),
  draw_group = function(self, data, panel_scales, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
    data <- data[order(data$group, data$x), ]
    data <- self$stairstep_confint(data)
    ggplot2:::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
  },
  stairstep_confint = function (data) {
    data <- as.data.frame(data)[order(data$x), ]
    n <- nrow(data)
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
    data.frame(x = data$x[xs], ymin = data$ymin[ys], ymax = data$ymax[ys],
               data[xs, setdiff(names(data), c("x", "ymin", "ymax"))])
  }
)
