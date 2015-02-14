context('test forecast')

test_that('fortify.forecast works for AirPassengers', {
  d.arima <- forecast::auto.arima(AirPassengers)
  d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)

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
  expect_equal(names(fortified), c(expected_names, 'Lo 80', 'Hi 80'))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1965-02-01'))

})

skip_on_travis <- function() {
  # use testthat::skip_on_travis once released
  skip('Skip')
}

test_that('fortify.arfima works for austres', {
  skip_on_travis()

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
  skip_on_travis()
  result <- forecast::ets(UKgas)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1960-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1986-10-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))

  result <- forecast::bats(UKgas)
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
  skip_on_travis()
  result <- forecast::ets(austres)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope', 'Season')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))

  result <- forecast::bats(austres)
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals', 'Level', 'Slope')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1971-04-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1993-04-01'))

  p <- ggplot2::autoplot(result)
  expect_true(is(p, 'ggplot'))
})
