context('test cluster')

test_that('fortify.kmeans works for iris', {
  df <- iris[-5]

  fortified <- ggplot2::fortify(stats::kmeans(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::kmeans(df, 3), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(iris), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  expect_that(ggplot2::autoplot(stats::kmeans(df, 3), data = df), not(throws_error()))
  expect_that(ggplot2::autoplot(stats::kmeans(df, 3)), throws_error())
})

test_that('fortify.partition works for iris', {
  df <- iris[-5]
  # clara
  fortified <- ggplot2::fortify(cluster::clara(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::clara(df, 3), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(iris), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  # fanny
  fortified <- ggplot2::fortify(cluster::fanny(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::fanny(df, 3), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(iris), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  # pam
  fortified <- ggplot2::fortify(cluster::pam(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::pam(df, 3), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(iris), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))
})

test_that('fortify.partition works for USArrests', {
  df <- USArrests

  # clara
  fortified <- ggplot2::fortify(cluster::clara(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::clara(df, 3), data = USArrests)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  # fanny
  fortified <- ggplot2::fortify(cluster::fanny(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::fanny(df, 3), data = USArrests)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  # pam
  fortified <- ggplot2::fortify(cluster::pam(df, 3))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(cluster::pam(df, 3), data = USArrests)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c(colnames(df), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(df))
})
