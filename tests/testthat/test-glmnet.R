context('test glmnet')

test_that('fortify.glmnet works for Orange', {
  skip_if_not_installed("glmnet")
  library(glmnet)
  set.seed(1)
  res <- glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3]))
  fortified <- ggplot2::fortify(res)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Tree', 'age', 'Df', 'Lambda', 'dev.ratio')
  expect_equal(colnames(fortified), expected_names)
  expect_equal(dim(fortified), c(58, 5))

  p <- ggplot2::autoplot(res)
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(res, xvar = 'norm')
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(res, xvar = 'lambda')
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(res, xvar = 'dev')
  expect_true(is(p, 'ggplot'))

  res <- cv.glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3]))
  fortified <- ggplot2::fortify(res)
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('lambda', 'cvm', 'cvup', 'cvlo', 'nz')
  expect_equal(colnames(fortified), expected_names)
  # TODO: Add `suppressWarnings(RNGversion("3.5.0"))` to all tests and examples
  # expect_equal(dim(fortified), c(57, 5))

  p <- ggplot2::autoplot(res)
  expect_true(is(p, 'ggplot'))
})


test_that('fortify.glmnet works for QuickStartExample', {
  skip_if_not_installed("glmnet")
  library(glmnet)
  set.seed(1)
  data(QuickStartExample)
  res <- glmnet(QuickStartExample$x, QuickStartExample$y)
  fortified <- ggplot2::fortify(res)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(dim(fortified), c(67, 23))

  p <- ggplot2::autoplot(res)
  expect_true(is(p, 'ggplot'))

  res <- cv.glmnet(QuickStartExample$x, QuickStartExample$y)
  fortified <- ggplot2::fortify(res)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_true(nrow(fortified) >= 65)
  expect_true(ncol(fortified) == 5)

  p <- ggplot2::autoplot(res)
  expect_true(is(p, 'ggplot'))
})
