context('test cluster')

test_that('fortify.kmeans works for iris', {
  skip_on_cran()
  skip_on_travis()
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

  p <- ggplot2::autoplot(stats::kmeans(df, 3), data = df)
  expect_true(is(p, 'ggplot'))

  expect_that(ggplot2::autoplot(stats::kmeans(df, 3)), throws_error())
})

test_that('fortify.partition works for iris', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("cluster")
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

  p <- autoplot(cluster::clara(df, 3), data = iris)
  expect_true(inherits(p, 'ggplot'))

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

  p <- autoplot(cluster::fanny(df, 3), data = iris)
  expect_true(inherits(p, 'ggplot'))

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

  p <- autoplot(cluster::pam(df, 3), data = iris)
  expect_true(inherits(p, 'ggplot'))
})

test_that('fortify.partition works for USArrests', {
  skip_on_cran()
  skip_on_travis()
  skip_if_not_installed("cluster")
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

test_that('fortify.silhouette works', {
  skip_on_cran()
  skip_on_travis()
  df <- iris[-5]
  mod <- stats::kmeans(df, 3)
  sil <- cluster::silhouette(mod$cluster, stats::dist(df))
  sil.df <- as.data.frame(unclass(sil))

  fortified <- ggplot2::fortify(sil)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(colnames(fortified), c('cluster', 'sil_width', 'id'))
  expect_equal(is.factor(fortified$cluster), TRUE)
  expect_equal(rownames(fortified), rownames(sil.df[order(sil.df$cluster), ]))

  p <- ggplot2::autoplot(sil)
  expect_true(is(p, 'ggplot'))

  class(sil) <- 'sil'
  expect_that(ggplot2::autoplot(sil), throws_error())
})

test_that('autoplot.kmeans works for iris', {
  skip_on_cran()
  skip_on_travis()

  obj <- stats::kmeans(iris[-5], 3)

  p <- ggplot2::autoplot(obj, data = iris)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))

  p <- ggplot2::autoplot(obj, data = iris, frame.type = 'norm')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))

})

test_that('autoplot.silhouette works', {
  skip_on_cran()
  skip_on_travis()

  df <- iris[-5]
  model <- stats::kmeans(df, 3)
  obj <- cluster::silhouette(model$cluster, stats::dist(df))

  p <- ggplot2::autoplot(obj)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomBar' %in% class(p$layers[[1]]$geom))

  p <- ggplot2::autoplot(obj, colour = 'black')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomBar' %in% class(p$layers[[1]]$geom))
  expect_true('black' == p$layers[[2]]$aes_params$colour)

})
