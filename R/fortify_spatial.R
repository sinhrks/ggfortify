#' Convert \code{sp} instances to \code{data.frame}.
#'
#' @param model \code{sp} instances
#' @inheritParams fortify_base
#' @param rename logical flag indicating whether to rename coordinates to long and lat
#' @return data.frame
#' @export
fortify.SpatialCommon <- function(model, data = NULL,
                                  rename = TRUE, ...) {
  if (is(model, 'SpatialPoints') ||
      is(model, 'SpatialPointsDataFrame')) {
    df <- as.data.frame(model)

  } else if (is(model, 'SpatialLines')) {
    dfs <- lapply(model@lines, fortify)
    suppressWarnings(
      # bind_row against factors with different levels
      df <- do.call(dplyr::bind_rows, dfs)
    )
    df[['piece']] <- as.factor(df[['piece']])
    df[['group']] <- as.factor(df[['group']])
  } else {
    # Line, Lines, SpatialLinesDataFrame are defined in ggplot2
    stop(paste0('Unsupported class for fortify.SpatialCommon: ', class(model)))
  }

  if (rename) {
    if (!is(model, 'SpatialLines')) {
      # SpatialLines are renamed in lapply
      coords <- colnames(sp::coordinates(model))
      names(coords) <- c('long', 'lat')
      df <- dplyr::rename(df, coords)
      }
  }
  post_fortify(df, klass = model)
}

#' @export
fortify.SpatialPoints <- fortify.SpatialCommon

#' @export
fortify.SpatialPointsDataFrame <- fortify.SpatialCommon

#' @export
fortify.SpatialLines <- fortify.SpatialCommon

# followings are defined in ggplot2
# fortify.Line, fortify.Lines, SpatialLinesDataFrame, fortify.Polygon
# fortify.Polygons, fortify.SpatialPolygons, fortify.SpatialPolygonsDataFrame

#' Autoplot \code{maps::map}
#'
#' @param object \code{maps::map} instance
#' @param p \code{ggplot2::ggplot} instance
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
autoplot.SpatialCommon <- function(object, p = NULL, group = NULL,
                                   colour = 'black', size = NULL, linetype = NULL,
                                   alpha = NULL, fill = NULL, shape = NULL,
                                   xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                                   main = NULL, xlab = '', ylab = '', asp = NULL,
                                   ...) {

  plot.data <- ggplot2::fortify(object)

  if (is_derived_from(object, 'SpatialPoints') ||
      is_derived_from(object, 'SpatialPointsDataFrame')) {
    geomfunc <- ggplot2::geom_point
  } else if (is_derived_from(object, 'Polygon') ||
             is_derived_from(object, 'Polygons') ||
             is_derived_from(object, 'SpatialPolygons') ||
             is_derived_from(object, 'SpatialPolygonsDataFrame')) {
    # Polygon must be checked before Line
    geomfunc <- ggplot2::geom_polygon
  } else if (is_derived_from(object, 'Line') ||
             is_derived_from(object, 'Lines') ||
             is_derived_from(object, 'SpatialLines') ||
             is_derived_from(object, 'SpatialLinesDataFrame')) {
    geomfunc <- ggplot2::geom_line
    if (is_derived_from(object, 'Lines') ||
        is_derived_from(object, 'SpatialLines') ||
        is_derived_from(object, 'SpatialLinesDataFrame')) {
      # when line contains multiple segments
      group <- 'group'
    }
  } else {
    stop(paste0('Unsupported class for autoplot.SpatialCommon: ', class(object)))
  }
  if (is_derived_from(object, 'Polygons') ||
      is_derived_from(object, 'SpatialPolygons') ||
      is_derived_from(object, 'SpatialPolygonsDataFrame') ||
      is_derived_from(object, 'Lines') ||
      is_derived_from(object, 'SpatialLines') ||
      is_derived_from(object, 'SpatialLinesDataFrame')) {
      # when object contains multiple segments
    group <- 'group'
  }
  if (is_derived_from(object, 'SpatialPolygonsDataFrame') ||
      is_derived_from(object, 'SpatialLinesDataFrame')) {
    df2 <- as.data.frame(object)
    df2[['id']] <- rownames(df2)
    plot.data <- dplyr::inner_join(plot.data, df2, by = 'id')
  }

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
    p <- p + geom_factory(geomfunc, data = plot.data, x = 'long', y = 'lat',
                          group = group, colour = colour, size = size,
                          linetype = linetype,
                          alpha = alpha, fill = fill, shape = shape)
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}

#' @export
autoplot.SpatialPointsDataFrame <- autoplot.SpatialCommon

#' @export
autoplot.SpatialPoints <- autoplot.SpatialCommon

#' @export
autoplot.Line <- autoplot.SpatialCommon

#' @export
autoplot.Lines <- autoplot.SpatialCommon

#' @export
autoplot.SpatialLines <- autoplot.SpatialCommon

#' @export
autoplot.SpatialLinesDataFrame <- autoplot.SpatialCommon

#' @export
autoplot.Polygon <- autoplot.SpatialCommon

#' @export
autoplot.Polygons <- autoplot.SpatialCommon

#' @export
autoplot.SpatialPolygons <- autoplot.SpatialCommon

#' @export
autoplot.SpatialPolygonsDataFrame <- autoplot.SpatialCommon
