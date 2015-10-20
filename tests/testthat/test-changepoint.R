library(changepoint)
library(strucchange)

context('test changepoint')

test_that('fortify.cpt works for AirPassengers', {

  # mean
  result <- changepoint::cpt.mean(AirPassengers)
  expect_that(ggplot2::autoplot(result), not(throws_error()))
  fortified <- ggplot2::fortify(result)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Data', 'mean'))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  filtered <- dplyr::filter(fortified, !is.na(mean))
  expect_equal(filtered$Index[1], as.Date('1955-05-01'))
  expect_equal(filtered$Index[nrow(filtered)], as.Date('1960-12-01'))

  # var
  fortified <- ggplot2::fortify(changepoint::cpt.var(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Data', 'variance'))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  filtered <- dplyr::filter(fortified, !is.na(variance))
  expect_equal(filtered$Index[1], as.Date('1959-04-01'))
  expect_equal(filtered$Index[nrow(filtered)], as.Date('1960-12-01'))

  # meanvar
  fortified <- ggplot2::fortify(changepoint::cpt.meanvar(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Data', 'mean', 'variance'))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  filtered <- dplyr::filter(fortified, !is.na(mean) | !is.na(variance))
  expect_equal(filtered$Index[1], as.Date('1955-05-01'))
  expect_equal(filtered$Index[nrow(filtered)], as.Date('1960-12-01'))

  
})


test_that('fortify.breakpoints works for Nile', {

  bp.nile <- strucchange::breakpoints(Nile ~ 1)
  expect_that(ggplot2::autoplot(bp.nile), data = Nile)
  fortified <- ggplot2::fortify(bp.nile, is.date = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Data', 'Breaks'))
  expect_equal(fortified$Index[1], as.Date('1871-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1970-01-01'))

  filtered <- dplyr::filter(fortified, !is.na(Breaks))
  expect_equal(filtered$Index[1], as.Date('1898-01-01'))

  bp.pts <- strucchange::breakpoints(bp.nile, breaks = 2)
  fortified <- ggplot2::fortify(bp.pts, is.date = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Breaks'))
  expect_equal(fortified$Index[1], as.Date('1871-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1970-01-01'))

  filtered <- dplyr::filter(fortified, !is.na(Breaks))
  expect_equal(filtered$Index[1], as.Date('1898-01-01'))
  expect_equal(filtered$Index[nrow(filtered)], as.Date('1953-01-01'))

  fortified <- ggplot2::fortify(bp.pts, data = Nile, is.date = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c('Index', 'Data', 'Breaks'))
  expect_equal(fortified$Index[1], as.Date('1871-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1970-01-01'))
})
