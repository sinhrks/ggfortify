context('test base_ts')

test_that('fortify.ts works for timeserieses', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("zoo")
  skip_if_not_installed("vars")
  data(Canada, package = 'vars')
  library(zoo)

  p <- autoplot(Canada, columns = 'e', is.date = TRUE)
  expect_true(is(p, 'ggplot'))

  p <- autoplot(UKgas)
  expect_true(is(p, 'ggplot'))
})
