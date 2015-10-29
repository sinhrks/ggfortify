context('test base_ts')

test_that('fortify.ts works for timeserieses', {
  data(Canada, package = 'vars')
  library(zoo)

  p <- autoplot(Canada, columns = 'e', is.date = TRUE)
  expect_true(is(p, 'ggplot'))

  p <- autoplot(UKgas)
  expect_true(is(p, 'ggplot'))

  p <- autoplot(xts::as.xts(UKgas))
  expect_true(is(p, 'ggplot'))

  p <- autoplot(xts::as.xts(Canada))
  expect_true(is(p, 'ggplot'))

  p <- autoplot(timeSeries::as.timeSeries(Canada))
  expect_true(is(p, 'ggplot'))

})
