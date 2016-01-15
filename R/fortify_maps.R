#' Convert \code{maps::map} to \code{data.frame}.
#'
#' @param model \code{maps::map} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @export
fortify_map <- function(model, data = NULL, ...) {
  # ggplot2::fortify.map cannot handle maps retrieved by fill = FALSE properly
  df <- as.data.frame(model[c("x", "y")])
  names(df) <- c("long", "lat")
  df$group <- cumsum(is.na(df$long) & is.na(df$lat)) + 1
  df$order <- 1:nrow(df)
  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  if (max(df$group) <= length(names)) {
    df$region <- names[df$group, 1]
    df$subregion <- names[df$group, 2]
  } else {
    df$region <- df$group
    df$subregion <- NA
  }
  df <- df[complete.cases(df$lat, df$long), ]
  post_fortify(df)
}

#' Autoplot \code{maps::map}
#'
#' @param object \code{maps::map} instance
#' @param p \code{ggplot2::ggplot} instance
#' @param geom geometric string for map. 'path', 'point' or 'polygon'
#' @param group key for grouping geoms
#' @param colour line colour
#' @param size point size
#' @param linetype line type
#' @param alpha alpha
#' @param fill fill colour
#' @param shape point shape
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @export
autoplot.map <- function(object, p = NULL, geom = 'path', group = 'group',
                         colour = 'black', size = NULL, linetype = NULL,
                         alpha = NULL, fill = NULL, shape = NULL,
                         xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                         main = NULL, xlab = '', ylab = '', asp = NULL,
                         ...) {
  geomfunc <- get_geom_function(geom, allowed = c('path', 'point', 'polygon'))
  plot.data <- fortify_map(object)

  # create ggplot instance if not passed
  if (is.null(p)) {
    mapping <- ggplot2::aes_string(x = 'long', y = 'lat')
    p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
    # ToDo: allow data to be none. ggplot instance should be
    # passed to factory to check its data
    p <- p + geom_factory(geomfunc, data = plot.data, group = group,
                          colour = colour, size = size, linetype = linetype,
                          alpha = alpha, fill = fill, shape = shape)
  } else {
    p <- p + geom_factory(geomfunc, data = plot.data,
                          x = 'long', y = 'lat', group = group,
                          colour = colour, size = size, linetype = linetype,
                          alpha = alpha, fill = fill, shape = shape)
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
