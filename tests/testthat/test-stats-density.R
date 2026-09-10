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
  expect_true(is(p$layers[[1]]$geom, 'GeomLine'))

  p <- ggdistribution(ppois, seq(0, 30), lambda = 20)
  expect_true(is(p$layers[[1]]$geom, 'GeomStep'))

  p <- ggdistribution(ppois, seq(0, 30), lambda = 20,
                      fill = 'blue')
  expect_true(is(p$layers[[1]]$geom, 'GeomConfint'))

  p <- ggdistribution(dpois, 0:30, lambda = 20)
  expect_true(is(p$layers[[1]]$geom, 'GeomBar'))

  p <- ggdistribution(dpois, 0:30, lambda = 20, colour = 'red')
  expect_equal(p$layers[[1]]$aes_params$fill, 'red')

  # repeast
  p <- ggdistribution(pchisq, 0:20, df = 7, fill = 'blue')
  expect_true(is(p, 'ggplot'))
  p <- ggdistribution(pchisq, 0:20, p = p, df = 9, fill = 'red')
  expect_true(is(p, 'ggplot'))
})

test_that('ggdistribution axis labels can be customized', {
  p <- ggdistribution(dnorm, seq(-3, 3, 0.1),
                      mean = 0, sd = 1,
                      xlab = 'direct x', ylab = 'direct y')
  expect_equal(p$labels$x, 'direct x')
  expect_equal(p$labels$y, 'direct y')

  p <- p + ggplot2::labs(x = 'later x', y = 'later y')
  expect_equal(p$labels$x, 'later x')
  expect_equal(p$labels$y, 'later y')
  expect_null(p$scales$get_scales('x'))
  expect_null(p$scales$get_scales('y'))
})

test_that('standard discrete CDFs are detected', {
  discrete_cdfs <- list(pbinom, pgeom, phyper, pnbinom,
                        ppois, psignrank, pwilcox)
  detected <- vapply(discrete_cdfs, ggfortify:::is_discrete_cdf, logical(1L))
  expect_true(all(detected))
  expect_true(ggfortify:::is_discrete_cdf(ecdf(0:3)))
  expect_false(ggfortify:::is_discrete_cdf(pnorm))
  expect_false('geom' %in% names(formals(ggdistribution)))
})

test_that('standard discrete PMFs are detected', {
  discrete_pmfs <- list(dbinom, dgeom, dhyper, dnbinom,
                        dpois, dsignrank, dwilcox)
  detected <- vapply(discrete_pmfs, ggfortify:::is_discrete_pmf, logical(1L))
  expect_true(all(detected))
  expect_false(ggfortify:::is_discrete_pmf(dnorm))
})
