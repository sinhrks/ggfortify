context('test forecast')

test_that('fortify.forecast works for AirPassengers', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  d.arima <- forecast::auto.arima(AirPassengers)
  d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)

  p <- ggplot2::autoplot(d.forecast)
  expect_true(is(p, 'ggplot'))

  fortified <- ggplot2::fortify(d.forecast)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'Fitted',
                      'Point Forecast', 'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1965-02-01'))

  d.forecast <- forecast::forecast(d.arima, level = c(95, 80), h = 50)
  fortified <- ggplot2::fortify(d.forecast)

  expect_equal(is.data.frame(fortified), TRUE)
  if (packageVersion('forecast') > '7.3') {
    # forecast 8.0 sorts columns in alphabetical order
    expect_equal(names(fortified),
                 c('Index', 'Data', 'Fitted', 'Point Forecast',
                   'Lo 80', 'Hi 80', 'Lo 95', 'Hi 95'))
  } else {
    expect_equal(names(fortified), c(expected_names, 'Lo 80', 'Hi 80'))
  }
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1965-02-01'))

  p <- autoplot(d.forecast)
  expect_true(inherits(p, 'ggplot'))

  p <- autoplot(d.forecast, ts.connect = TRUE)
  expect_true(inherits(p, 'ggplot'))
})

test_that('fortify.arfima works for austres', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  fortified <- ggplot2::fortify(forecast::arfima(austres))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))

  fortified <- ggplot2::fortify(forecast::nnetar(austres))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))
})

test_that('fortify.ets works for UKgas', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  result <- forecast::ets(UKgas)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1960-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1986-10-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.bats works for UKgas', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  result <- forecast::bats(UKgas, use.parallel = FALSE)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1960-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1986-10-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.ets works for austres', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  result <- forecast::ets(austres)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  if (result$components[3] == "N")
    expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope')
  else
    expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.bats works for austres', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
  result <- forecast::bats(austres, use.parallel = FALSE)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.forecast works for AirPassengers', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
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
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("forecast")
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
