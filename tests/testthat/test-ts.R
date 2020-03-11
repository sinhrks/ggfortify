context('test timeseries')

test_that('fortify.ts works for AirPassengers', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  fortified <- ggplot2::fortify(AirPassengers)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  fortified <- ggplot2::fortify(AirPassengers, index.name = 'time', data.name = 'orig')
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'orig')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.ts works for Canada', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  data(Canada, package = 'vars')
  fortified <- ggplot2::fortify(Canada)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1980-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('2000-10-01'))

  # In multivariate, data.name will not be applied
  fortified <- ggplot2::fortify(Canada, index.name = 'time', data.name = 'orig')
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.timeSeries works for AirPassengers', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  fortified <- ggplot2::fortify(timeSeries::as.timeSeries(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.POSIXct('1949-01-31'))
  expect_equal(fortified$Index[nrow(fortified)], as.POSIXct('1960-12-31'))

  fortified <- ggplot2::fortify(AirPassengers, index.name = 'time', data.name = 'orig')
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'orig')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.timeSeries works for Canada', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  data(Canada, package = 'vars')
  fortified <- ggplot2::fortify(timeSeries::as.timeSeries(Canada))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.POSIXct('1980-03-31'))
  expect_equal(fortified$Index[nrow(fortified)], as.POSIXct('2000-12-31'))

  fortified <- ggplot2::fortify(Canada, index.name = 'time')
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
})

test_that('autoplot ts works for univariate timeseries', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  sts <- as.ts(c(1, 2, 3, 4))

  p <- autoplot(sts)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$y, c(1, 2, 3, 4))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$colour, rep('black', 4))

  p <- autoplot(sts, facets = TRUE, stacked = TRUE, colour = 'blue')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$y, c(1, 2, 3, 4))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$colour, rep('blue', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  p <- autoplot(sts, facets = TRUE, stacked = TRUE, colour = 'blue', geom = 'ribbon')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomRibbon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$ymin, c(0, 0, 0, 0))
  expect_equal(ld$ymax, c(1, 2, 3, 4))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$colour, rep('blue', 4))
  expect_equal(ld$alpha, rep(NA, 4))

  p <- autoplot(sts, geom = 'bar')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomBar'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$y, c(1, 2, 3, 4))
  expect_equal(ld$x, c(1, 2, 3, 4))
  expect_equal(ld$fill, rep('grey35', 4))
  expect_equal(ld$alpha, rep(NA, 4))

})

test_that('autoplot ts works for multivariate timeseries', {
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("vars")
  library(timeSeries)
  library(vars)
  df <- data.frame(A=c(1, 2, 3, 4), B=c(5, 6, 7, 8))
  mts <- as.ts(df)

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'bar')
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomBar'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(length(ld$y), 8) # not stacked
  expect_equal(length(ld$colour), 8)
  expect_equal(length(ld$fill), 8)
  expect_true(all(is.na(ld$alpha)))

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'bar', stacked = TRUE)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomBar'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(length(ld$y), 8) # not stacked
  expect_equal(length(ld$colour), 8)
  expect_equal(length(ld$fill), 8)
  expect_true(all(is.na(ld$alpha)))

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'ribbon', stacked = FALSE)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomRibbon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$ymin, c(0, 0, 0, 0, 0, 0, 0, 0)) # not stacked
  expect_equal(ld$ymax, c(1, 2, 3, 4, 5, 6, 7, 8)) # not stacked
  expect_equal(ld$x, c(1, 2, 3, 4, 1, 2, 3, 4))
  expect_equal(ld$colour, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_equal(ld$fill, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_equal(ld$alpha, rep(0.5, 8))
  expect_true('GeomRibbon' %in% class(p$layers[[1]]$geom))

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'ribbon', stacked = TRUE)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomRibbon'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$ymin, c(0, 0, 0, 0, 1, 2, 3, 4)) # stacked
  expect_equal(ld$ymax, c(1, 2, 3, 4, 6, 8, 10, 12)) # stacked
  expect_equal(ld$x, c(1, 2, 3, 4, 1, 2, 3, 4))
  expect_equal(ld$colour, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_equal(ld$fill, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_true(all(is.na(ld$alpha)))

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'line', stacked = FALSE)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$y, c(1, 2, 3, 4, 5, 6, 7, 8)) # not stacked
  expect_equal(ld$x, c(1, 2, 3, 4, 1, 2, 3, 4))
  expect_equal(ld$colour, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_true(all(is.na(ld$alpha)))

  p <- ggfortify:::autoplot.ts(mts, facets=FALSE, geom = 'line', stacked = TRUE)
  expect_equal(length(p$layers), 1)
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))
  ld <- ggplot2:::layer_data(p, 1)
  expect_equal(ld$y, c(1, 2, 3, 4, 6, 8, 10, 12)) # stacked
  expect_equal(ld$x, c(1, 2, 3, 4, 1, 2, 3, 4))
  expect_equal(ld$colour, rep(c('#F8766D',  '#00BFC4'), c(4, 4)))
  expect_true(all(is.na(ld$alpha)))

  # error cases
  expect_error(ggfortify:::autoplot.ts(mts, geom = 'xxx'))
})
