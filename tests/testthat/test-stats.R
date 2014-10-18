context('test stats')

test_that('fortify.ts works for AirPassengers', {
  fortified <- ggplot2::fortify(AirPassengers)
  expect_equal(is.data.frame(fortified), TRUE)
  
  expected_names <- c('time', 'x')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
})

test_that('fortify.ts works for Canada', {
  data(Canada, package = 'vars')
  fortified <- ggplot2::fortify(Canada)
  expect_equal(is.data.frame(fortified), TRUE)
  
  expected_names <- c('time', 'e', 'prod', 'rw', 'U')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$time[1], as.Date('1980-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('2000-10-01'))
})

test_that('fortify.stl works for AirPassengers', {
  fortified <- ggplot2::fortify(stats::stl(AirPassengers, s.window = 'periodic'))
  expect_equal(is.data.frame(fortified), TRUE)
  
  expected_names <- c('time', 'data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified$time[1], as.Date('1949-01-01'))
  expect_equal(fortified$time[nrow(fortified)], as.Date('1960-12-01'))
})

test_that('fortify.prcomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('PC1', 'PC2', 'PC3', 'PC4')
  expected_names <- c(names(df), pcs)
  
  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
  
  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
  
  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
 
  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::prcomp(df), original = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4, 5)], iris)
})

test_that('fortify.princomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(df), pcs)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = TRUE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = TRUE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
  
  fortified <- ggplot2::fortify(stats::princomp(df, center = FALSE, scale = FALSE))
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4)], df)
 
  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::princomp(df), original = iris)
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(names(fortified), expected_names)
  expect_equal(fortified[c(1, 2, 3, 4, 5)], iris)
})