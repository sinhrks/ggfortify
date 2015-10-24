library(timeSeries)
library(vars)

context('test timeseries')

test_that('fortify.ts works for AirPassengers', {
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


test_that('fortify.forecast works for AirPassengers', {
  d.arima <- forecast::auto.arima(AirPassengers)
  d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)

  fortified <- fortify(d.forecast)
  expected_names <- c('Index', 'Data', 'Fitted', 'Point Forecast',
                      'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[194], as.Date('1965-02-01'))
  p <- autoplot(d.forecast)
  expect_true(inherits(p, 'ggplot'))

  fortified <- fortify(d.forecast, ts.connect = TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Point Forecast',
                      'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[194], as.Date('1965-02-01'))
  p <- autoplot(d.forecast, ts.connect = TRUE)
  expect_true(inherits(p, 'ggplot'))
})


test_that('fortify.ets works for AirPassengers', {
  d.arima <- forecast::auto.arima(AirPassengers)
  d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)

  fortified <- fortify(d.forecast)
  expected_names <- c('Index', 'Data', 'Fitted', 'Point Forecast',
                      'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[194], as.Date('1965-02-01'))
  p <- autoplot(d.forecast)
  expect_true(inherits(p, 'ggplot'))

  fortified <- fortify(d.forecast, ts.connect = TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Point Forecast',
                      'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[194], as.Date('1965-02-01'))
  p <- autoplot(d.forecast, ts.connect = TRUE)
  expect_true(inherits(p, 'ggplot'))
})