context('test base-infer')

test_that('infer works for MDS-likes', {
  skip_on_cran()
  skip_on_travis()
  # MDS
  data(eurodist)
  expect_equal(infer(cmdscale(eurodist, eig = TRUE)), 'mds-like')
  expect_equal(infer(cmdscale(eurodist, add = TRUE)), 'mds-like')
  expect_equal(infer(cmdscale(eurodist, x.ret = TRUE)), 'mds-like')
  library(MASS)
  expect_equal(infer(isoMDS(eurodist)), 'mds-like')
  expect_equal(infer(sammon(eurodist)), 'mds-like')
})

test_that('fortify works for MDS-likes', {
  skip_on_cran()
  skip_on_travis()
  # MDS
  data(eurodist)
  fortified <- fortify(cmdscale(eurodist, eig = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2'))
  expect_equal(nrow(fortified), 21)

  fortified <- fortify(cmdscale(eurodist, x.ret = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2'))
  expect_equal(nrow(fortified), 21)

  fortified <- fortify(cmdscale(eurodist, k = 4, add = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3', '4'))
  expect_equal(nrow(fortified), 21)

  library(MASS)
  expect_equal(infer(isoMDS(eurodist)), 'mds-like')
  fortified <- fortify(cmdscale(eurodist, k = 3, add = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(nrow(fortified), 21)

  expect_equal(infer(sammon(eurodist)), 'mds-like')
  fortified <- fortify(cmdscale(eurodist, k = 3, add = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(nrow(fortified), 21)
})

test_that('infer, fortify and autoplot works for KFAS::signal', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("KFAS")
  nile_fortified <- fortify(Nile)

  library(KFAS)
  model <- SSModel(
    Nile ~ SSMtrend(degree=1, Q=matrix(NA)), H=matrix(NA)
  )
  fit <- fitSSM(model=model, inits=c(log(var(Nile)), log(var(Nile))), method="BFGS")

  smoothed <- KFS(fit$model)
  fortified <- fortify(smoothed)
  expect_equal(colnames(fortified), c('Index', 'Data', 'Fitted', 'Residuals'))
  expect_equal(fortified$Index, nile_fortified$Index)

  filtered <- KFS(fit$model, filtering="mean", smoothing='none')
  fortified <- fortify(filtered)
  expect_equal(colnames(fortified), c('Index', 'Data', 'Fitted', 'Residuals'))
  expect_equal(fortified$Index, nile_fortified$Index)

  trend <- signal(smoothed, states="trend")
  expect_equal(infer(trend), 'KFASSignal')

  fortified <- fortify(trend)
  expect_equal(colnames(fortified), c('Index', 'Data'))
  expect_equal(fortified$Index, nile_fortified$Index)

  p <- autoplot(filtered)
  expect_true(inherits(p, 'ggplot'))

  p <- autoplot(smoothed)
  expect_true(inherits(p, 'ggplot'))

  p <- autoplot(trend)
  expect_true(inherits(p, 'ggplot'))
})
