context('test tslib')

test_that('get.dtindex works for AirPassengers', {
  result <- ggfortify:::get.dtindex(AirPassengers)
  expect_equal(result[1], as.Date('1949-01-01'))
  expect_equal(result[length(result)], as.Date('1960-12-01'))
})

test_that('get.dtindex works for UKgas', {
  result <- ggfortify:::get.dtindex(UKgas)
  expect_equal(result[1], as.Date('1960-01-01'))
  expect_equal(result[length(result)], as.Date('1986-10-01'))

  result <- ggfortify:::get.dtindex(UKgas, is.date = FALSE)
  expect_equal(result[1], 1960)
  expect_equal(result[length(result)], 1986.75)

  result <- ggfortify:::get.dtindex(UKgas, is.date = TRUE)
  expect_equal(result[1], as.Date('1960-01-01'))
  expect_equal(result[length(result)], as.Date('1986-10-01'))
})

test_that('get.dtindex works for Nile', {
  result <- ggfortify:::get.dtindex(Nile)
  expect_equal(result[1], 1871)
  expect_equal(result[length(result)], 1970)

  result <- ggfortify:::get.dtindex(Nile, is.date = FALSE)
  expect_equal(result[1], 1871)
  expect_equal(result[length(result)], 1970)

  result <- ggfortify:::get.dtindex(Nile, is.date = TRUE)
  expect_equal(result[1], as.Date('1871-01-01'))
  expect_equal(result[length(result)], as.Date('1970-01-01'))
})

test_that('get.dtindex.continuous works for AirPassengers', {
  result <- ggfortify:::get.dtindex.continuous(AirPassengers, length = 18)
  expect_equal(result[1], as.Date('1961-01-01'))
  expect_equal(result[length(result)], as.Date('1962-06-01'))
})

test_that('get.dtindex.continuous works for UKgas', {
  result <- ggfortify:::get.dtindex.continuous(UKgas, length = 6)
  expect_equal(result[1], as.Date('1987-01-01'))
  expect_equal(result[length(result)], as.Date('1988-04-01'))
})

test_that('confint.acf works for AirPassengers', {
  result <- ggfortify:::confint.acf(stats::acf(AirPassengers, plot = FALSE))
  expect_equal(result, 0.1633303, tolerance = .000001)

  result <- ggfortify:::confint.acf(stats::acf(AirPassengers, plot = FALSE), ci.type = 'ma')
  expected <- c(NA, 0.1633303, 0.2731862, 0.3399018, 0.3876238, 0.4248224, 0.4556929,
                0.4821335, 0.5058641, 0.5280447, 0.5503176, 0.5737563, 0.5988899,
                0.6241140, 0.6454578, 0.6624988, 0.6761814, 0.6875039, 0.6971273,
                0.7054849, 0.7130967, 0.7203560)
  expect_equal(result, expected, tolerance = .000001)
})

test_that('fitted/residuals works for Arima/ar', {
  skip_if_not_installed("forecast")
  m <- stats::ar(UKgas)
  resid <- residuals(m)
  testthat::expect_false(is.null(resid))
  fit <- fitted(m)
  testthat::expect_false(is.null(fitted))

  original <- as.vector(UKgas)[7:length(UKgas)]
  calcurated <- as.vector(fit + resid)[7:length(UKgas)]
  testthat::expect_equal(original, calcurated)

  m <- stats::arima(UKgas)
  resid <- residuals(m)
  testthat::expect_false(is.null(resid))
  fit <- fitted(m)
  testthat::expect_false(is.null(fitted))

  original <- as.vector(UKgas)[7:length(UKgas)]
  calcurated <- as.vector(fit + resid)[7:length(UKgas)]
  testthat::expect_equal(original, calcurated)
})

test_that('ggfreqplot', {
  p <- ggfreqplot(AirPassengers)
  expect_true(inherits(p, 'ggplot'))

  p <- ggfreqplot(AirPassengers, facet.labeller = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2))
  expect_true(inherits(p, 'ggplot'))
})


test_that('ggcpgram', {
  p <- ggcpgram(AirPassengers)
  expect_true(inherits(p, 'ggplot'))
})

test_that('ggtsdiag', {
  p <- ggtsdiag(arima(AirPassengers))
  expect_true(inherits(p, 'ggmultiplot'))
})
