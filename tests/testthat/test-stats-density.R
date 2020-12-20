context('test stats density')

test_that('fortify.density works for rnorm', {
  dens <- stats::density(stats::rnorm(1:50))
  fortified <- ggplot2::fortify(dens)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('x', 'y')
  expect_equal(names(fortified), expected_names)
  expect_equal(nrow(fortified), 512)

  p <- ggplot2::autoplot(dens)
  expect_true(is(p, 'ggplot'))
})

test_that('ggdistribution', {
  p <- ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
  p <- ggdistribution(ppois, seq(0, 30), lambda = 20)

  # repeast
  p <- ggdistribution(pchisq, 0:20, df = 7, fill = 'blue')
  expect_true(is(p, 'ggplot'))
  p <- ggdistribution(pchisq, 0:20, p = p, df = 9, fill = 'red')
  expect_true(is(p, 'ggplot'))
})
