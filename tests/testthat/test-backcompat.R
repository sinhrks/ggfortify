context('backward compatibility for deprecated keywords')

test_that('deprecated in v0.0.1:: ts.facet', {
  data(Canada, package = 'vars')
  expect_warning(p1 <- ggplot2::autoplot(Canada, facet = FALSE))
  p2 <- ggplot2::autoplot(Canada, facets = FALSE)
  expect_equal(p1, p2)
})

test_that('deprecated in v0.0.2:: tsmodel.original', {
  library(forecast)
  library(ggfortify)

  x <- AirPassengers
  m <- stats::ar(x) # create model with temporary variable
  x <- NULL
  expect_warning(f1 <- ggplot2::fortify(m, original = AirPassengers))
  f2 <- ggplot2::fortify(m, data = AirPassengers)
  expect_equal(f1, f2)
  expect_warning(p1 <- ggplot2::autoplot(m, original = AirPassengers))
  p2 <- ggplot2::autoplot(m, data = AirPassengers)
  expect_equal(p1, p2)

  x <- AirPassengers
  m <- stats::arima(x) # create model with temporary variable
  x <- NULL
  expect_warning(f1 <- ggplot2::fortify(m, original = AirPassengers))
  f2 <- ggplot2::fortify(m, data = AirPassengers)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(m, original = AirPassengers))
  p2 <- ggplot2::autoplot(m, data = AirPassengers)
  expect_equal(p1, p2)
})

test_that('deprecated in v0.0.2:: tsmodel.original examples', {
  expect_warning(f1 <- ggplot2::fortify(stats::arima(UKgas),
                                        original = UKgas, is.date = TRUE))
  f2 <- ggplot2::fortify(stats::arima(UKgas), data = UKgas, is.date = TRUE)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(stats::arima(UKgas),
                                         original = UKgas))
  p2 <- ggplot2::autoplot(stats::arima(UKgas), data = UKgas)
  expect_equal(p1, p2)
})

test_that('deprecated in v0.0.2:: prcomp.original', {
  df <- iris[-5]

  # prcomp
  expect_warning(f1 <- ggplot2::fortify(stats::prcomp(df), original = iris))
  f2 <- ggplot2::fortify(stats::prcomp(df), data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(stats::prcomp(df), original = iris))
  p2 <- ggplot2::autoplot(stats::prcomp(df), data = iris)
  expect_equal(p1, p2)

  # princomp
  expect_warning(f1 <- ggplot2::fortify(stats::princomp(df), original = iris))
  f2 <- ggplot2::fortify(stats::princomp(df), data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(stats::princomp(df), original = iris))
  p2 <- ggplot2::autoplot(stats::princomp(df), data = iris)
  expect_equal(p1, p2)

  # factanal
  d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
  expect_warning(f1 <- ggplot2::fortify(d.factanal, original = state.x77))
  f2 <- ggplot2::fortify(d.factanal, data = state.x77)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(d.factanal, original = state.x77))
  p2 <- ggplot2::autoplot(d.factanal, data = state.x77)
  expect_equal(p1, p2)
})

test_that('deprecated in v0.0.2:: cluster.original', {
  df <- iris[-5]

  # kmeans
  clustered <- stats::kmeans(df, 3)
  expect_warning(f1 <- ggplot2::fortify(clustered, original = iris))
  f2 <- ggplot2::fortify(clustered, data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(clustered, original = iris))
  p2 <- ggplot2::autoplot(clustered, data = iris)
  expect_equal(p1, p2)

  # clara
  clustered <- cluster::clara(df, 3)
  expect_warning(f1 <- ggplot2::fortify(clustered, original = iris))
  f2 <- ggplot2::fortify(clustered, data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(clustered, original = iris))
  p2 <- ggplot2::autoplot(clustered, data = iris)
  expect_equal(p1, p2)

  # fanny
  clustered <- cluster::fanny(df, 3)
  expect_warning(f1 <- ggplot2::fortify(clustered, original = iris))
  f2 <- ggplot2::fortify(clustered, data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(clustered, original = iris))
  p2 <- ggplot2::autoplot(clustered, data = iris)
  expect_equal(p1, p2)

  # pam
  clustered <- cluster::pam(df, 3)
  expect_warning(f1 <- ggplot2::fortify(clustered, original = iris))
  f2 <- ggplot2::fortify(clustered, data = iris)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(clustered, original = iris))
  p2 <- ggplot2::autoplot(clustered, data = iris)
  expect_equal(p1, p2)
})

test_that('deprecated in v0.0.2:: breakpoints.original', {

  bp.nile <- strucchange::breakpoints(Nile ~ 1)
  bp.pts <- strucchange::breakpoints(bp.nile, breaks = 2)

  expect_warning(f1 <- ggplot2::fortify(bp.pts, original = Nile, is.date = TRUE))
  f2 <- ggplot2::fortify(bp.pts, data = Nile, is.date = TRUE)
  expect_equal(f1, f2)

  expect_warning(p1 <- ggplot2::autoplot(bp.pts, original = Nile, is.date = TRUE))
  p2 <- ggplot2::autoplot(bp.pts, data = Nile, is.date = TRUE)
  expect_equal(f1, f2)
})

test_that('deprecated in v0.0.2:: matrix.scale', {
  expect_warning(autoplot(matrix(rnorm(20), nc = 5),
                          scale = scale_fill_gradient(low = 'red', high = 'blue')))
})
