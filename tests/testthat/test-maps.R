test_that('test fortify_map', {
  skip_if_not_installed("mapdata")
  library(mapdata)

  jp <- map('japan', plot = FALSE)
  jpe <- map('japan', fill = TRUE, exact = FALSE, plot = FALSE)
  cn <- map('china', plot = FALSE)

  result <- head(ggfortify:::fortify_map(jp))
  expect_equal(result$long, c(139.7707, 139.7706, 139.7773, 139.7783, 139.7829, 139.7819), tolerance=1e-3)
  expect_equal(result$lat, c(42.3018, 42.3001, 42.2956, 42.2916, 42.2838, 42.2833), tolerance=1e-3)
  expect_equal(result$group, c(1, 1, 1, 1, 1, 1))

  result <- head(ggfortify:::fortify_map(jpe))
  expect_equal(result$long, c(139.7707, 139.7706, 139.7773, 139.7783, 139.7829, 139.7819), tolerance=1e-3)
  expect_equal(result$lat, c(42.3018, 42.3001, 42.2956, 42.2916, 42.2838, 42.2833), tolerance=1e-3)
  expect_equal(result$group, rep(1, 6))
  expect_equal(result$region, rep('Hokkaido', 6))

  result <- head(ggfortify:::fortify_map(cn))
  expect_equal(result$long, c(123.1802, 123.1928, 123.2037, 123.2146, 123.2266, 123.2386), tolerance=1e-3)
  expect_equal(result$lat, c(46.25580, 46.25987, 46.26560, 46.27115, 46.27585, 46.27901), tolerance=1e-3)
  expect_equal(result$group, rep(1, 6))
})

test_that('test autoplot.map', {
  skip_if_not_installed("mapdata")
  library(mapdata)

  jp <- map('japan', plot = FALSE)
  jpe <- map('japan', fill = TRUE, exact = FALSE, plot = FALSE)
  cn <- map('china', plot = FALSE)

  p <- autoplot(jp)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPath'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(139.7707, 139.7706, 139.7773, 139.7783, 139.7829, 139.7819), tolerance=1e-3)
  expect_equal(ld$y, c(42.3018, 42.3001, 42.2956, 42.2916, 42.2838, 42.2833), tolerance=1e-3)
  expect_equal(ld$group, rep(1, 6))

  p <- autoplot(jpe)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPath'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(139.7707, 139.7706, 139.7773, 139.7783, 139.7829, 139.7819), tolerance=1e-3)
  expect_equal(ld$y, c(42.3018, 42.3001, 42.2956, 42.2916, 42.2838, 42.2833), tolerance=1e-3)
  expect_equal(ld$group, rep(1, 6))

  p <- autoplot(cn)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomPath'))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, c(123.1802, 123.1928, 123.2037, 123.2146, 123.2266, 123.2386), tolerance=1e-3)
  expect_equal(ld$y, c(46.25580, 46.25987, 46.26560, 46.27115, 46.27585, 46.27901), tolerance=1e-2)
  expect_equal(ld$group, rep(1, 6))

  # add cities
  cities <- get('world.cities')
  cities[cities$country.etc == 'China', ]
  # no need to specify aes
  p <- p + geom_point(data = cities)
  expect_equal(length(p$layers), 2)
  expect_true(is(p$layers[[2]]$geom, 'GeomPoint'))
  # add additional map layer
  p <- autoplot(jp, p = p)
  expect_equal(length(p$layers), 3)
  expect_true(is(p$layers[[3]]$geom, 'GeomPath'))
})
