library(changepoint)

context('test changepoint')

test_that('fortify.cpt works for AirPassengers', {
  fortified <- ggplot2::fortify(changepoint::cpt.mean(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('time', 'cpts', 'mean'))
  expect_equal(fortified$time[1], as.Date('1955-05-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
  fortified <- ggplot2::fortify(changepoint::cpt.var(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('time', 'cpts', 'variance'))
  expect_equal(fortified$time[1], as.Date('1959-04-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
  fortified <- ggplot2::fortify(changepoint::cpt.meanvar(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('time', 'cpts', 'mean', 'variance'))
  expect_equal(fortified$time[1], as.Date('1955-04-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
})