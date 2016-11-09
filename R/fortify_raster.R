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
