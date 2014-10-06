context('test stats')

test_that('fortify.stl works for AirPassengers', {
  fortified <- ggplot2::fortify(stats::stl(AirPassengers, s.window = 'periodic'))
  expect_equal(is.data.frame(fortified), TRUE)
  
  expected_names <- c('time', 'data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
})