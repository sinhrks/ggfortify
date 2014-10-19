library(forecast)

context('test forecast')

test_that('fortify.forecast works for AirPassengers', {
  d.arima <- forecast::auto.arima(AirPassengers)
  d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
  
  fortified <- ggplot2::fortify(d.forecast)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
 
  expected_names <- c('Time', 'Original', 'Fitted',
                      'Point Forecast', 'Lo 95', 'Hi 95')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Time[1], as.Date('1949-01-01'))
  expect_equal(fortified$Time[nrow(fortified)], as.Date('1965-02-01'))

  d.forecast <- forecast::forecast(d.arima, level = c(95, 80), h = 50)
  fortified <- ggplot2::fortify(d.forecast)

  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(expected_names, 'Lo 80', 'Hi 80'))
  expect_equal(fortified$Time[1], as.Date('1949-01-01'))
  expect_equal(fortified$Time[nrow(fortified)], as.Date('1965-02-01'))
  
})