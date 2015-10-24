library(vars)

context('test vars')

test_that('vars.varored works for Canada', {
  data(Canada, package = 'vars')
  d.vselect <- vars::VARselect(Canada, lag.max = 5, type = 'const')$selection[1]
  d.var <- vars::VAR(Canada, p = d.vselect, type = 'const')

  d.predicted <- stats::predict(d.var, n.ahead = 50)
  fortified <- ggplot2::fortify(d.predicted)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(nrow(fortified), 50 + nrow(Canada))

  expected_names <- c('Index', 'e', 'prod', 'rw', 'U',
                      'e.fcst', 'e.lower', 'e.upper', 'e.CI',
                      'prod.fcst', 'prod.lower', 'prod.upper', 'prod.CI',
                      'rw.fcst', 'rw.lower', 'rw.upper', 'rw.CI',
                      'U.fcst', 'U.lower', 'U.upper', 'U.CI')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1980-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('2013-04-01'))

  p <- ggplot2::autoplot(d.predicted)
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(d.predicted, ts.connect = FALSE)
  expect_true(is(p, 'ggplot'))

  fortified <- ggplot2::fortify(d.predicted, melt = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(nrow(fortified), (50 + nrow(Canada)) * 4)

  expected_names <- c('Index', 'Data', 'fcst', 'lower', 'upper', 'CI', 'variable')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$Index[1], as.Date('1980-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('2013-04-01'))

})
