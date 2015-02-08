context('test cluster')

test_that('fortify.kmeans works for iris', {
  df <- iris[-5]

  fortified <- ggplot2::fortify(stats::kmeans(df, 3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)

  fortified <- ggplot2::fortify(stats::kmeans(df, 3), data = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('cluster', colnames(iris)))
  expect_equal(is.factor(fortified$cluster), TRUE)
})

test_that('fortify.partition works for iris', {
  df <- iris[-5]
  expected_names <- c('cluster', colnames(df))

  # clara
  fortified <- ggplot2::fortify(cluster::clara(df, 3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(is.factor(fortified$cluster), TRUE)

  fortified <- ggplot2::fortify(cluster::clara(df, 3), data = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(expected_names, 'Species'))
  expect_equal(is.factor(fortified$cluster), TRUE)

  # fanny
  fortified <- ggplot2::fortify(cluster::fanny(df, 3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(is.factor(fortified$cluster), TRUE)

  fortified <- ggplot2::fortify(cluster::fanny(df, 3), data = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(expected_names, 'Species'))
  expect_equal(is.factor(fortified$cluster), TRUE)

  # pam
  fortified <- ggplot2::fortify(cluster::pam(df, 3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(is.factor(fortified$cluster), TRUE)

  fortified <- ggplot2::fortify(cluster::pam(df, 3), data = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(expected_names, 'Species'))
  expect_equal(is.factor(fortified$cluster), TRUE)
})
