context('test stats')

test_that('fortify.stl works for AirPassengers', {
  fortified <- ggplot2::fortify(stats::stl(AirPassengers, s.window = 'periodic'))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
  expected_names <- c('Index', 'Data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
  
  fortified <- ggplot2::fortify(stats::decompose(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
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
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  x <- AirPassengers
  m <- stats::ar(x) # create model with temporary variable
  x <- NULL
  fortified2 <- ggplot2::fortify(m, original = AirPassengers)
  expect_equal(fortified, fortified2)
  
  fortified <- ggplot2::fortify(stats::arima(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expected_names <- c('Index', 'Data', 'Fitted', 'Residuals')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
  
  x <- AirPassengers
  m <- stats::arima(x) # create model with temporary variable
  x <- NULL
  fortified2 <- ggplot2::fortify(m, original = AirPassengers)
  expect_equal(fortified, fortified2)
  
  fortified <- ggplot2::fortify(stats::HoltWinters(AirPassengers))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
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
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  
  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = FALSE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  
  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = FALSE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
 
  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::prcomp(df), original = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
})

test_that('fortify.princomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(df), pcs)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = FALSE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = FALSE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
 
  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::princomp(df), original = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
})

test_that('fortify.factanal works for state.x77', {
  d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
  pcs <- c('Factor1', 'Factor2', 'Factor3')
  
  fortified <- ggplot2::fortify(d.factanal)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), pcs)
 
  # attach original
  fortified <- ggplot2::fortify(d.factanal, original = state.x77)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(colnames(state.x77), pcs))
})

test_that('fortify.kmeans works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  expected_names <- c('cluster')
  
  fortified <- ggplot2::fortify(stats::kmeans(df, 3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c('cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
 
  fortified <- ggplot2::fortify(stats::kmeans(df, 3), original = iris)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(names(fortified), c(names(iris), 'cluster'))
  expect_equal(is.factor(fortified$cluster), TRUE)
})