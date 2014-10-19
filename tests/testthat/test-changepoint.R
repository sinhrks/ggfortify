library(changepoint)

context('test changepoint')

test_that('fortify.cpt works for AirPassengers', {
  
  # mean
  fortified <- ggplot2::fortify(changepoint::cpt.mean(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('time', 'x', 'mean'))
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
  filtered <- dplyr::filter(fortified, !is.na(mean))
  expect_equal(filtered$time[1], as.Date('1955-05-01'))
  expect_equal(filtered$time[nrow(filtered)], as.Date('1960-12-01'))
  
  # keep.original = FALSE
  fortified <- ggplot2::fortify(changepoint::cpt.mean(AirPassengers), keep.original = FALSE)
  expect_equal(fortified$time[1], as.Date('1955-05-01'))
  expect_equal(fortified$time[nrow(filtered)], as.Date('1960-12-01'))
  
  # var
  fortified <- ggplot2::fortify(changepoint::cpt.var(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('time', 'x', 'variance'))
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
  filtered <- dplyr::filter(fortified, !is.na(variance))
  expect_equal(filtered$time[1], as.Date('1959-04-01'))
  expect_equal(filtered$time[nrow(filtered)], as.Date('1960-12-01'))
  
  # meanvar
  fortified <- ggplot2::fortify(changepoint::cpt.meanvar(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('time', 'x', 'mean', 'variance'))
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
  
  filtered <- dplyr::filter(fortified, !is.na(mean) | !is.na(variance))
  expect_equal(filtered$time[1], as.Date('1955-04-01'))
  expect_equal(filtered$time[nrow(filtered)], as.Date('1960-12-01'))
  
})