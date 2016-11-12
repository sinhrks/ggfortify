test_that('test fortify.RasterCommon', {

  library(raster)

  # RasterLayer object
  r <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10)
  r[] <- 0

  # Corresponding data frame
  coords <- seq(0.5, 9.5, by = 1)  # Code for center coordinates
  coords <- expand.grid(coords, coords)
  colnames(coords) <- c('long', 'lat')
  exp <- data.frame(coords, layer = 0)
  exp <- exp[with(exp, order(-lat, long)),]  # Reorder df to get expected df

  res <- fortify(r)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterLayer')

  res <- fortify(res, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('x', 'y'))
  expect_equal(res$x, exp$long)
  expect_equal(res$y, exp$lat)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterLayer')

  # RasterStack
  rs <- stack(r, r)
  expect_true(is(rs, 'RasterStack'))

  res <- fortify(rs)
  exp$layer.2 <- exp$layer
  colnames(exp)[3] <- 'layer.1'
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat', 'layer.1', 'layer.2'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(res$layer.1, exp$layer.1)
  expect_equal(res$layer.2, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterStack')

  res <- fortify(rs, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('x', 'y', 'layer.1', 'layer.2'))
  expect_equal(res$x, exp$long)
  expect_equal(res$y, exp$lat)
  expect_equal(res$layer.1, exp$layer.1)
  expect_equal(res$layer.2, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterStack')

  # layers with custom names
  names(rs) <- c('first_layer', 'second_layer')

  res <- fortify(rs)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat', 'first_layer', 'second_layer'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(res$first_layer, exp$layer.1)
  expect_equal(res$second_layer, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterStack')


  # Raster Brick
  rb <- brick(r, r)
  expect_true(is(rb, 'RasterBrick'))

  res <- fortify(rb)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat', 'layer.1', 'layer.2'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(res$layer.1, exp$layer.1)
  expect_equal(res$layer.2, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterBrick')

  res <- fortify(rb, rename = FALSE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('x', 'y', 'layer.1', 'layer.2'))
  expect_equal(res$x, exp$long)
  expect_equal(res$y, exp$lat)
  expect_equal(res$layer.1, exp$layer.1)
  expect_equal(res$layer.2, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterBrick')

  # layers with custom names
  names(rb) <- c('first_layer', 'second_layer')

  res <- fortify(rb)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('long', 'lat', 'first_layer', 'second_layer'))
  expect_equal(res$long, exp$long)
  expect_equal(res$lat, exp$lat)
  expect_equal(res$firbt_layer, exp$layer.1)
  expect_equal(res$second_layer, exp$layer.2)
  expect_equal(attr(res, 'base_class')[[1]], 'RasterBrick')
})
