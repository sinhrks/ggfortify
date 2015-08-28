context('test stats')

test_that('fortify.stl works for AirPassengers', {
  fortified <- ggplot2::fortify(stats::stl(AirPassengers, s.window = 'periodic'))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  fortified <- ggplot2::fortify(stats::decompose(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
})

test_that('fortify.Arima works for AirPassengers', {
  library(forecast)
  library(ggfortify)

  fortified <- ggplot2::fortify(ar(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  x <- AirPassengers
  m <- stats::ar(x) # create model with temporary variable
  x <- NULL
  fortified2 <- ggplot2::fortify(m, data = AirPassengers)
  expect_equal(fortified, fortified2)
  ggplot2::autoplot(m, data = AirPassengers)

  fortified <- ggplot2::fortify(stats::arima(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
  ggplot2::autoplot(stats::arima(AirPassengers))

  x <- AirPassengers
  m <- stats::arima(x) # create model with temporary variable
  x <- NULL
  fortified2 <- ggplot2::fortify(m, data = AirPassengers)
  expect_equal(fortified, fortified2)
  ggplot2::autoplot(m, data = AirPassengers)

  fortified <- ggplot2::fortify(stats::HoltWinters(AirPassengers))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Index', 'Data', 'xhat', 'level', 'trend', 'season', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  library(fGarch)
  d.fGarch <- fGarch::garchFit(formula = ~arma(1, 1) + garch(1, 1), data = UKgas, trace = FALSE)
  fortified <- ggplot2::fortify(d.fGarch)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.prcomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('PC1', 'PC2', 'PC3', 'PC4')
  expected_names <- c(names(df), pcs)

  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::prcomp(df), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
  expect_equal(rownames(fortified), rownames(df))
})

test_that('fortify.princomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(df), pcs)

  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::princomp(df), data = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
  expect_equal(rownames(fortified), rownames(df))
})

test_that('fortify.factanal works for state.x77', {
  d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
  pcs <- c('Factor1', 'Factor2', 'Factor3')

  fortified <- ggplot2::fortify(d.factanal)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), pcs)
  expect_equal(rownames(fortified), rownames(state.x77))

  # attach original
  fortified <- ggplot2::fortify(d.factanal, data = state.x77)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), c(colnames(state.x77), pcs))
  expect_equal(rownames(fortified), rownames(state.x77))
})

test_that('fortify.prcomp works for USArrests', {
  pcs <- c('PC1', 'PC2', 'PC3', 'PC4')
  expected_names <- c(names(USArrests), pcs)

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  # attach original
  fortified <- ggplot2::fortify(stats::prcomp(USArrests), data = USArrests)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('fortify.princomp works for USArrests', {
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(USArrests), pcs)

  fortified <- ggplot2::fortify(stats::princomp(USArrests, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::princomp(USArrests, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::princomp(USArrests, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::princomp(USArrests, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  # attach original
  fortified <- ggplot2::fortify(stats::princomp(USArrests), data = USArrests)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('fortify.dist works for eurodist', {
  fortified <- ggplot2::fortify(eurodist)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(dim(fortified), c(21, 21))
})

test_that('fortify.lfda works for iris', {
  library(lfda)
  k <- iris[,-5]
  y <- iris[,5]
  r <- 3
  model <- lfda(k, y, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_equal(is.data.frame(fortified), TRUE)

  model <- klfda(kmatrixGauss(k), y, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_equal(is.data.frame(fortified), TRUE)

  model <- self(k, y, beta=0.1, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_equal(is.data.frame(fortified), TRUE)
})

test_that('autoplot.lfda works for iris', {
  k <- iris[,-5]
  y <- iris[,5]
  r <- 4
  model <- lfda::lfda(k,y,r,metric="plain")
  p <- autoplot(model, data=iris, frame = TRUE, frame.colour='Species')
  expect_true(is(p, 'ggplot'))
})

test_that('autoplot.acf works', {

  p <- autoplot(stats::acf(AirPassengers, plot = FALSE))
  expect_true(is(p, 'ggplot'))

  p <- autoplot(stats::acf(AirPassengers, plot = FALSE), conf.int.type = 'ma')
  expect_true(is(p, 'ggplot'))

  p <- autoplot(stats::pacf(AirPassengers, plot = FALSE))
  expect_true(is(p, 'ggplot'))

  p <- autoplot(stats::ccf(AirPassengers, AirPassengers, plot = FALSE))
  expect_true(is(p, 'ggplot'))
})

test_that('autoplot.stepfun works', {
  fortified <- fortify(stepfun(c(1, 2, 3), c(4, 5, 6, 7)))
  expected <- data.frame(x = c(0, 1, 1, 2, 2, 3, 3, 4),
                         y = c(4, 4, 5, 5, 6, 6, 7, 7))
  expect_equal(fortified, expected)

  fortified <- fortify(stepfun(c(1), c(4, 5)))
  expected <- data.frame(x = c(0.9375, 1.0000, 1.0000, 1.0625),
                         y = c(4, 4, 5, 5))
  expect_equal(fortified, expected)

  fortified <- fortify(stepfun(c(1, 3, 4, 8), c(4, 5, 2, 3, 5)))
  expected <- data.frame(x = c(-1, 1, 1, 3, 3, 4, 4, 8, 8, 10),
                         y = c(4, 4, 5, 5, 2, 2, 3, 3, 5, 5))
  expect_equal(fortified, expected)

  fortified <- fortify(stepfun(c(1, 2, 3, 4, 5, 6, 7, 8, 10),
                               c(4, 5, 6, 7, 8, 9, 10, 11, 12, 9)))
  expected <- data.frame(x = c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 10, 10, 11),
                         y = c(4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 9, 9))
  expect_equal(fortified, expected)
})
