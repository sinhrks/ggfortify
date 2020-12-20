context('test base')

test_that('fortify.table works for Titanic', {
  skip_on_cran()
  skip_on_travis()
  fortified <- ggplot2::fortify(Titanic)
  expect_equal(is.data.frame(fortified), TRUE)

})

test_that('fortify.matrix works', {
  skip_on_cran()
  skip_on_travis()
  m <- matrix(1:6, nrow=2, ncol=3)
  fortified <- fortify(m)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(rownames(fortified), c('1', '2'))

  p <- autoplot(m)
  expect_true(inherits(p, 'ggplot'))

  fortified <- fortify(m, compat = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('V1', 'V2', 'V3'))
  expect_equal(rownames(fortified), c('1', '2'))

  p <- autoplot(m, compat = TRUE)
  expect_true(inherits(p, 'ggplot'))

  m <- matrix(1:6, nrow=2, ncol=3)
  colnames(m) <- c('A', 'B', 'C')
  # dplyr doesn't guarantee rownames
  rownames(m) <- c('X', 'Y')

  fortified <- fortify(m)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('A', 'B', 'C'))
  expect_equal(rownames(fortified), c('X', 'Y'))

  p <- autoplot(m)
  expect_true(inherits(p, 'ggplot'))
  expect_that(autoplot(m, geom = 'point'), throws_error())

  m <- matrix(1:6, nrow=3, ncol=2)
  colnames(m) <- c('A', 'B')
  # dplyr doesn't guarantee rownames
  rownames(m) <- c('X', 'Y', 'Z')

  fortified <- fortify(m)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('A', 'B'))
  expect_equal(rownames(fortified), c('X', 'Y', 'Z'))

  p <- autoplot(m)
  expect_true(inherits(p, 'ggplot'))

  p <- autoplot(m, geom = 'point')
  expect_true(inherits(p, 'ggplot'))
})
