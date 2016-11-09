#' Convert \code{raster} to \code{data.frame}
#'
#' @param model \code{raster} instances
#' @inheritParams fortify_base
#' @param maxpixels number of pixels for resampling
#' @param rename logical flag indicating whether to rename coordinates to long and lat
#' @return data.frame
#' @export
fortify.RasterCommon <- function(model, data = NULL, maxpixels = 100000,
                                rename = TRUE, ...) {
  if (is(model, 'RasterLayer')) {

    # Resample if necessary (raster too big)
    model <- sampleRegular(model, maxpixels, asRaster=TRUE)
    # Get coordinates
    coords <- xyFromCell(model, seq_len(ncell(model)))

    ## Extract values
    dat <- as.data.frame(getValues(model))

    dat <- cbind(coords, dat)
    names(dat)[3] <- names(model)
    df <- as.data.frame(dat)

  } else if (is(model, 'RasterBrick') || is(model, 'RasterStack')) {

    nl <- nlayers(model)
    model <- sampleRegular(model, maxpixels, asRaster=TRUE)
    coords <- xyFromCell(model, seq_len(ncell(model)))
    ## Extract values
    dat <- as.data.frame(getValues(model))
    dat <- cbind(coords, dat)
    names(dat)[3:(nl+2)] <- names(model)
    df <- as.data.frame(dat)

  } else {
    stop(paste0('Unsupported class for fortify.RasterCommon: ', class(model)))
  }

  if (rename) {
    colnames(df)[1:2] <- c("long", "lat")
  }

  post_fortify(df, klass = model)
}
#' @export
fortify.RasterLayer <- fortify.RasterCommon

#' @export
fortify.RasterBrick <- fortify.RasterCommon

#' @export
fortify.RasterStack <- fortify.RasterCommon

#' Autoplot \code{raster::raster}
#'
#' @param object \code{raster::raster} instance
#' @param p \code{ggplot2::ggplot} instance
#' @param raster.layer name of the layer to plot
#' @param alpha alpha
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @export
autoplot.RasterCommon <- function(object, raster.layer = NULL, p = NULL,
                                  alpha = NULL, xlim = c(NA, NA),
                                  ylim = c(NA, NA), log = "", main = NULL,
                                  xlab = '', ylab = '', asp = NULL, ...) {
  plot.data <- ggplot2::fortify(object)
  layer_names <- names(object)

  if (is.null(p)) {
    mapping <- ggplot2::aes_string(x = 'long', y = 'lat')
    p <- ggplot2::ggplot(data = plot.data, mapping = mapping)
    p <- p + geom_factory(geomfunc, data = plot.data, colour = colour,
                          size = size, linetype = linetype, alpha = alpha,
                          fill = fill, shape = shape)
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
