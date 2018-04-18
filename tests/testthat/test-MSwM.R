context('test MSwM')

test_that('fortify.MSwM works for sample data', {
  skip_if_not_installed("MSwM")
  library(MSwM)
  d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
                  exog = cos(seq(-pi / 2, pi / 2, length.out = 100)))
  d.mswm <- MSwM::msmFit(lm(Data ~ .-1, data = d), k = 2, sw = rep(TRUE, 2),
                        control = list(parallelization = FALSE))

  fortified <- fortify(d.mswm)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'exog', 'Fitted', 'Residuals',
                      'FiltProb.1', 'FiltProb.2', 'SmoProb.1', 'SmoProb.2', 'ProbableModel')
  expect_equal(names(fortified), expected_names)

  p <- autoplot(d.mswm)
  expect_true(inherits(p, 'ggplot'))
})
