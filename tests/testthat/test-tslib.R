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
  result <- ggfortify:::confint.acf(acf(AirPassengers, plot = FALSE))
  expect_equal(result, 0.1633303, tolerance = .000001)
  
  result <- ggfortify:::confint.acf(acf(AirPassengers, plot = FALSE), ci.type = 'ma')
  expected <- c(NA, 0.1633303, 0.2731862, 0.3399018, 0.3876238, 0.4248224, 0.4556929,
                0.4821335, 0.5058641, 0.5280447, 0.5503176, 0.5737563, 0.5988899,
                0.6241140, 0.6454578, 0.6624988, 0.6761814, 0.6875039, 0.6971273,
                0.7054849, 0.7130967, 0.7203560)
  expect_equal(result, expected, tolerance = .000001)
})
               
