context('test timeseries')

test_that('fortify.ts works for AirPassengers', {
  fortified <- ggplot2::fortify(AirPassengers)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
  expected_names <- c('Index', 'Data')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
  
  fortified <- ggplot2::fortify(AirPassengers, index.name = 'time', data.name = 'orig')
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('time', 'orig')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.ts works for Canada', {
  data(Canada, package = 'vars')
  fortified <- ggplot2::fortify(Canada)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
  expected_names <- c('Index', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1980-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('2000-10-01'))
  
  # In multivariate, data.name will not be applied
  fortified <- ggplot2::fortify(Canada, index.name = 'time', data.name = 'orig')
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('time', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.timeSeries works for AirPassengers', {
  fortified <- ggplot2::fortify(timeSeries::as.timeSeries(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
  expected_names <- c('Index', 'Data')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.POSIXct('1949-01-31'))
  expect_equal(fortified$Index[nrow(fortified)], as.POSIXct('1960-12-31'))
  
  fortified <- ggplot2::fortify(AirPassengers, index.name = 'time', data.name = 'orig')
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('time', 'orig')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.timeSeries works for Canada', {
  data(Canada, package = 'vars')
  fortified <- ggplot2::fortify(timeSeries::as.timeSeries(Canada))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
  expected_names <- c('Index', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.POSIXct('1980-03-31'))
  expect_equal(fortified$Index[nrow(fortified)], as.POSIXct('2000-12-31'))
  
  fortified <- ggplot2::fortify(Canada, index.name = 'time')
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('time', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
})

