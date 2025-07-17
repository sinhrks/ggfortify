context('test stats')

test_that('fortify.stl works for AirPassengers', {
  skip_if_not_installed("zoo")
  fortified <- ggplot2::fortify(stats::stl(AirPassengers, s.window = 'periodic'))
  expect_true(is.data.frame(fortified))

  expected_names <- c('Index', 'Data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))

  fortified <- ggplot2::fortify(stats::decompose(AirPassengers))
  expect_true(is.data.frame(fortified))

  expected_names <- c('Index', 'Data', 'seasonal', 'trend', 'remainder')
  expect_equal(names(fortified), expected_names)
  expect_equal(as.vector(AirPassengers), as.vector(fortified[['Data']]))
  expect_equal(fortified$Index[1], as.Date('1949-01-01'))
  expect_equal(fortified$Index[nrow(fortified)], as.Date('1960-12-01'))
})

test_that('fortify.Arima works for AirPassengers', {
  skip_if_not_installed("forecast")
  skip_if_not_installed("fGarch")
  skip_if_not_installed("zoo")
  fortified <- ggplot2::fortify(ar(AirPassengers))
  expect_true(is.data.frame(fortified))
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
  expect_true(is.data.frame(fortified))
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
  expect_true(is.data.frame(fortified))

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
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = TRUE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = TRUE, scale = FALSE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  fortified <- ggplot2::fortify(stats::prcomp(df, center = FALSE, scale = FALSE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::prcomp(df), data = iris)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
  expect_equal(rownames(fortified), rownames(df))

  tmp <- stats::prcomp(df)
  class(tmp) <- 'unsupportedClass'
  expect_error(ggplot2::fortify(tmp, data = iris))
})


test_that('fortify.princomp works for iris', {
  df <- iris[c(1, 2, 3, 4)]
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(df), pcs)

  fortified <- ggplot2::fortify(stats::princomp(df))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), df)
  expect_equal(rownames(fortified), rownames(df))

  # attach original
  expected_names <- c(names(df), 'Species', pcs)
  fortified <- ggplot2::fortify(stats::princomp(df), data = iris)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4, 5)]), iris)
  expect_equal(rownames(fortified), rownames(df))

  p <- ggplot2::autoplot(stats::princomp(df), data = iris, colour = 'Species')
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(stats::princomp(df), data = iris, loadings.label = TRUE)
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(stats::princomp(df), data = iris, frame.type = 'convex')
  expect_true(is(p, 'ggplot'))

  expect_error(ggplot2::autoplot(stats::princomp(df), frame.type = 'invalid'))
})

test_that('fortify.factanal works for state.x77', {
  d.factanal <- stats::factanal(state.x77, factors = 3, scores = 'regression')
  pcs <- c('Factor1', 'Factor2', 'Factor3')

  fortified <- ggplot2::fortify(d.factanal)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), pcs)
  expect_equal(rownames(fortified), rownames(state.x77))

  # attach original
  fortified <- ggplot2::fortify(d.factanal, data = state.x77)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), c(colnames(state.x77), pcs))
  expect_equal(rownames(fortified), rownames(state.x77))
})

test_that('fortify.prcomp works for USArrests', {
  pcs <- c('PC1', 'PC2', 'PC3', 'PC4')
  expected_names <- c(names(USArrests), pcs)

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = TRUE, scale = TRUE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = FALSE, scale = TRUE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = TRUE, scale = FALSE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(stats::prcomp(USArrests, center = FALSE, scale = FALSE))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  # attach original
  fortified <- ggplot2::fortify(stats::prcomp(USArrests), data = USArrests)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('fortify.princomp works for USArrests', {
  pcs <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4')
  expected_names <- c(names(USArrests), pcs)

  fortified <- ggplot2::fortify(stats::princomp(USArrests))
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))

  # attach original
  fortified <- ggplot2::fortify(stats::princomp(USArrests), data = USArrests)
  expect_true(is.data.frame(fortified))
  expect_equal(names(fortified), expected_names)
  expect_equal(data.frame(fortified[c(1, 2, 3, 4)]), USArrests)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('fortify.prcomp handles PC column name conflicts correctly', {
  # Create test data
  test_data <- iris[1:4]

  # Create PCA model
  pca_model <- stats::prcomp(test_data, center = TRUE, scale. = TRUE)

  # Create conflicting data with PC column names containing different values
  conflicting_data <- data.frame(
    PC1 = rep(999, nrow(test_data)),  # Fake PC1 values
    PC2 = rep(888, nrow(test_data)),  # Fake PC2 values
    Species = iris$Species,
    OtherVar = 1:nrow(test_data)
  )

  # Fortify with conflicting data
  fortified <- ggplot2::fortify(pca_model, data = conflicting_data)

  # The bug: PC columns should come from the model, not from the data
  # Currently, the function incorrectly uses PC values from conflicting_data

  # Test that PC columns match the model's principal components
  model_pc1 <- pca_model$x[, "PC1"]
  model_pc2 <- pca_model$x[, "PC2"]

  # This test will FAIL with the current bug - PC columns come from conflicting_data
  expect_equal(fortified$PC1, model_pc1, tolerance = 1e-10,
               info = "PC1 should come from PCA model, not from data argument")

  expect_equal(fortified$PC2, model_pc2, tolerance = 1e-10,
               info = "PC2 should come from PCA model, not from data argument")

  # Non-PC columns should come from the data argument
  expect_equal(fortified$Species, conflicting_data$Species)
  expect_equal(fortified$OtherVar, conflicting_data$OtherVar)

  # Also test with partial PC conflicts (only some PC columns conflict)
  partial_conflict_data <- data.frame(
    PC1 = rep(777, nrow(test_data)),  # Only PC1 conflicts
    Species = iris$Species,
    ExtraCol = letters[1:nrow(test_data)]
  )

  fortified_partial <- ggplot2::fortify(pca_model, data = partial_conflict_data)

  # All PC columns should come from model
  expect_equal(fortified_partial$PC1, model_pc1, tolerance = 1e-10,
               info = "PC1 should come from PCA model even with partial conflicts")
  expect_equal(fortified_partial$PC2, model_pc2, tolerance = 1e-10,
               info = "PC2 should come from PCA model even with partial conflicts")

  # Non-PC columns should come from data
  expect_equal(fortified_partial$Species, partial_conflict_data$Species)
  expect_equal(fortified_partial$ExtraCol, partial_conflict_data$ExtraCol)
})

test_that('fortify.prcomp handles PC column name conflicts correctly', {
  # Create test data
  test_data <- iris[1:4]

  # Create PCA model
  pca_model <- stats::prcomp(test_data, center = TRUE, scale. = TRUE)

  # Create conflicting data with PC column names containing different values
  conflicting_data <- data.frame(
    PC1 = rep(999, nrow(test_data)),  # Fake PC1 values
    PC2 = rep(888, nrow(test_data)),  # Fake PC2 values
    Species = iris$Species,
    OtherVar = 1:nrow(test_data)
  )

  # Fortify with conflicting data
  fortified <- ggplot2::fortify(pca_model, data = conflicting_data)

  # The bug: PC columns should come from the model, not from the data
  # Currently, the function incorrectly uses PC values from conflicting_data

  # Test that PC columns match the model's principal components
  model_pc1 <- pca_model$x[, "PC1"]
  model_pc2 <- pca_model$x[, "PC2"]

  # This test will FAIL with the current bug - PC columns come from conflicting_data
  expect_equal(fortified$PC1, model_pc1, tolerance = 1e-10,
               info = "PC1 should come from PCA model, not from data argument")

  expect_equal(fortified$PC2, model_pc2, tolerance = 1e-10,
               info = "PC2 should come from PCA model, not from data argument")

  # Non-PC columns should come from the data argument
  expect_equal(fortified$Species, conflicting_data$Species)
  expect_equal(fortified$OtherVar, conflicting_data$OtherVar)

  # Also test with partial PC conflicts (only some PC columns conflict)
  partial_conflict_data <- data.frame(
    PC1 = rep(777, nrow(test_data)),  # Only PC1 conflicts
    Species = iris$Species,
    ExtraCol = letters[1:nrow(test_data)]
  )

  fortified_partial <- ggplot2::fortify(pca_model, data = partial_conflict_data)

  # All PC columns should come from model
  expect_equal(fortified_partial$PC1, model_pc1, tolerance = 1e-10,
               info = "PC1 should come from PCA model even with partial conflicts")
  expect_equal(fortified_partial$PC2, model_pc2, tolerance = 1e-10,
               info = "PC2 should come from PCA model even with partial conflicts")

  # Non-PC columns should come from data
  expect_equal(fortified_partial$Species, partial_conflict_data$Species)
  expect_equal(fortified_partial$ExtraCol, partial_conflict_data$ExtraCol)
})

test_that('fortify.princomp handles component column name conflicts correctly', {
  # Create test data
  test_data <- iris[1:4]

  # Create princomp model
  princomp_model <- stats::princomp(test_data)

  # Create conflicting data with component column names containing different values
  conflicting_data <- data.frame(
    Comp.1 = rep(999, nrow(test_data)),  # Fake Comp.1 values
    Comp.2 = rep(888, nrow(test_data)),  # Fake Comp.2 values
    Species = iris$Species,
    OtherVar = 1:nrow(test_data)
  )

  # Fortify with conflicting data
  fortified <- ggplot2::fortify(princomp_model, data = conflicting_data)

  # The bug: Component columns should come from the model, not from the data
  # Currently, the function incorrectly uses component values from conflicting_data

  # Test that component columns match the model's principal components
  model_comp1 <- princomp_model$scores[, "Comp.1"]
  model_comp2 <- princomp_model$scores[, "Comp.2"]

  # This test will FAIL with the current bug - component columns come from conflicting_data
  expect_equal(fortified$Comp.1, model_comp1, tolerance = 1e-10,
               info = "Comp.1 should come from princomp model, not from data argument")

  expect_equal(fortified$Comp.2, model_comp2, tolerance = 1e-10,
               info = "Comp.2 should come from princomp model, not from data argument")

  # Non-component columns should come from the data argument
  expect_equal(fortified$Species, conflicting_data$Species)
  expect_equal(fortified$OtherVar, conflicting_data$OtherVar)

  # Also test with partial component conflicts (only some component columns conflict)
  partial_conflict_data <- data.frame(
    Comp.1 = rep(777, nrow(test_data)),  # Only Comp.1 conflicts
    Species = iris$Species,
    ExtraCol = letters[1:nrow(test_data)]
  )

  fortified_partial <- ggplot2::fortify(princomp_model, data = partial_conflict_data)

  # All component columns should come from model
  expect_equal(fortified_partial$Comp.1, model_comp1, tolerance = 1e-10,
               info = "Comp.1 should come from princomp model even with partial conflicts")
  expect_equal(fortified_partial$Comp.2, model_comp2, tolerance = 1e-10,
               info = "Comp.2 should come from princomp model even with partial conflicts")

  # Non-component columns should come from data
  expect_equal(fortified_partial$Species, partial_conflict_data$Species)
  expect_equal(fortified_partial$ExtraCol, partial_conflict_data$ExtraCol)
})

test_that('fortify.factanal handles factor column name conflicts correctly', {
  # Create factanal model
  factanal_model <- stats::factanal(state.x77, factors = 3, scores = 'regression')

  # Create conflicting data with factor column names containing different values
  conflicting_data <- data.frame(
    Factor1 = rep(999, nrow(state.x77)),  # Fake Factor1 values
    Factor2 = rep(888, nrow(state.x77)),  # Fake Factor2 values
    Factor3 = rep(777, nrow(state.x77)),  # Fake Factor3 values
    State = rownames(state.x77),
    ExtraVar = 1:nrow(state.x77)
  )
  rownames(conflicting_data) <- rownames(state.x77)

  # Fortify with conflicting data
  fortified <- ggplot2::fortify(factanal_model, data = conflicting_data)

  # Test that factor columns match the model's factor scores
  model_factor1 <- unname(factanal_model$scores[, "Factor1"])
  model_factor2 <- unname(factanal_model$scores[, "Factor2"])
  model_factor3 <- unname(factanal_model$scores[, "Factor3"])

  # This test will FAIL with the current bug - factor columns come from conflicting_data
  expect_equal(fortified$Factor1, model_factor1, tolerance = 1e-10,
               info = "Factor1 should come from factanal model, not from data argument")

  expect_equal(fortified$Factor2, model_factor2, tolerance = 1e-10,
               info = "Factor2 should come from factanal model, not from data argument")

  expect_equal(fortified$Factor3, model_factor3, tolerance = 1e-10,
               info = "Factor3 should come from factanal model, not from data argument")

  # Non-factor columns should come from the data argument
  expect_equal(fortified$State, conflicting_data$State)
  expect_equal(fortified$ExtraVar, conflicting_data$ExtraVar)

  # Also test with partial factor conflicts (only some factor columns conflict)
  partial_conflict_data <- data.frame(
    Factor1 = rep(666, nrow(state.x77)),  # Only Factor1 conflicts
    Factor2 = rep(555, nrow(state.x77)),  # Only Factor2 conflicts
    State = rownames(state.x77),
    AnotherCol = LETTERS[1:nrow(state.x77)]
  )
  rownames(partial_conflict_data) <- rownames(state.x77)

  fortified_partial <- ggplot2::fortify(factanal_model, data = partial_conflict_data)

  # All factor columns should come from model
  expect_equal(fortified_partial$Factor1, model_factor1, tolerance = 1e-10,
               info = "Factor1 should come from factanal model even with partial conflicts")
  expect_equal(fortified_partial$Factor2, model_factor2, tolerance = 1e-10,
               info = "Factor2 should come from factanal model even with partial conflicts")
  expect_equal(fortified_partial$Factor3, model_factor3, tolerance = 1e-10,
               info = "Factor3 should come from factanal model even with partial conflicts")

  # Non-factor columns should come from data
  expect_equal(fortified_partial$State, partial_conflict_data$State)
  expect_equal(fortified_partial$AnotherCol, partial_conflict_data$AnotherCol)
})

test_that('autoplot.prcomp works for iris with scale (default)', {

  # fails on CRAN i386 because components are inversed.
  skip_on_cran()

  obj <- stats::prcomp(iris[-5])

  exp_x <- c(-0.10658039, -0.10777226, -0.11471510, -0.10901118, -0.10835099, -0.09056763)
  exp_y <- c(-0.05293913, 0.02933742, 0.02402493, 0.05275710, -0.05415858, -0.12287329)

  p <- ggplot2::autoplot(obj)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep('black', 6))

  p <- ggplot2::autoplot(obj, data = iris, colour = 'Species')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("#F8766D", 6))

  p <- ggplot2::autoplot(obj, data = iris, loadings = TRUE, loadings.label = FALSE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- ggplot2:::layer_data(p, 2)
  expect_equal(ld$x, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$xend, c(0.05086374, -0.01189621, 0.12057301, 0.05042779), tolerance = 1e-4)
  expect_equal(ld$y, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$colour, rep("#FF0000", 4))

  p <- ggplot2::autoplot(obj, data = iris, loadings.label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 3)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))
  expect_true('GeomText' %in% class(p$layers[[3]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- ggplot2:::layer_data(p, 2)
  expect_equal(ld$x, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$xend, c(0.05086374, -0.01189621, 0.12057301, 0.05042779), tolerance = 1e-4)
  expect_equal(ld$y, rep(0, 4))
  expect_equal(ld$colour, rep("#FF0000", 4))
  ld <- ggplot2:::layer_data(p, 3)
  expect_equal(ld$x, c(0.05086374, -0.01189621, 0.12057301, 0.05042779), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.09241228, -0.10276734, 0.02440152, 0.01062366), tolerance = 1e-4)
  expect_equal(ld$colour, rep("#FF0000", 4))
  expect_equal(ld$label, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

  p <- ggplot2::autoplot(obj, data = iris, frame.type = 'convex')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, c(0.15071626, 0.13846286, 0.12828254, -0.09474406, -0.10501689, -0.12769748), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.04265051, -0.19487526, -0.22776373, -0.22177981, -0.19537669, -0.02212193), tolerance = 1e-4)
  # expect_equal(ld$fill, rep("grey20", 6))
  expect_equal(ld$alpha, rep(0.2, 6))

  p <- ggplot2::autoplot(obj, data = iris, frame.type = 'convex', shape = FALSE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  # label will be turned on
  expect_true('GeomText' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("#000000", 6))
  expect_equal(ld$label, c('1', '2', '3', '4', '5', '6'))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, c(0.15071626, 0.13846286, 0.12828254, -0.09474406, -0.10501689, -0.12769748), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.04265051, -0.19487526, -0.22776373, -0.22177981, -0.19537669, -0.02212193), tolerance = 1e-4)
  # expect_equal(ld$fill, rep("grey20", 6))
  expect_equal(ld$alpha, rep(0.2, 6))
})

test_that('autoplot.prcomp works for iris without scale', {

  # fails on CRAN i386 because components are inversed.
  skip_on_cran()

  obj <- stats::prcomp(iris[-5])

  exp_x <- c(-2.684126, -2.714142, -2.888991, -2.745343, -2.728717, -2.280860)
  exp_y <- c(-0.3193972, 0.1770012, 0.1449494, 0.3182990, -0.3267545, -0.7413304)

  p <- ggplot2::autoplot(obj, scale = 0.)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep('black', 6))

  p <- ggplot2::autoplot(obj, scale = 0., data = iris, colour = 'Species')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("#F8766D", 6))

  p <- ggplot2::autoplot(obj, scale = 0, data = iris,
                         loadings = TRUE, loadings.label = FALSE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- ggplot2:::layer_data(p, 2)
  expect_equal(ld$x, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$xend, c(0.5441042, -0.1272572, 1.2898045, 0.5394407), tolerance = 1e-4)
  expect_equal(ld$y, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$colour, rep("#FF0000", 4))

  p <- ggplot2::autoplot(obj, scale = 0., data = iris,
                         loadings.label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 3)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))
  expect_true('GeomText' %in% class(p$layers[[3]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- ggplot2:::layer_data(p, 2)
  expect_equal(ld$x, rep(0, 4), tolerance = 1e-4)
  expect_equal(ld$xend, c(0.5441042, -0.1272572, 1.2898045, 0.5394407), tolerance = 1e-4)
  expect_equal(ld$y, rep(0, 4))
  expect_equal(ld$colour, rep("#FF0000", 4))
  ld <- ggplot2:::layer_data(p, 3)
  expect_equal(ld$x, c(0.5441042, -0.1272572, 1.2898045, 0.5394407), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.9885610, -1.0993321, 0.2610301, 0.1136443), tolerance = 1e-4)
  expect_equal(ld$colour, rep("#FF0000", 4))
  expect_equal(ld$label, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

  p <- ggplot2::autoplot(obj, scale = 0., data = iris,
                         frame.type = 'convex')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, c(3.795645, 3.487055, 3.230674, -2.386039, -2.644750, -3.215939), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.2573230, -1.1757393, -1.3741651, -1.3380623, -1.1787646, -0.1334681), tolerance = 1e-4)
  # expect_equal(ld$fill, rep("grey20", 6))
  expect_equal(ld$alpha, rep(0.2, 6))

  p <- ggplot2::autoplot(obj, scale = 0., data = iris,
                         frame.type = 'convex', shape = FALSE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  # label will be turned on
  expect_true('GeomText' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("#000000", 6))
  expect_equal(ld$label, c('1', '2', '3', '4', '5', '6'))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, c(3.795645, 3.487055, 3.230674, -2.386039, -2.644750, -3.215939), tolerance = 1e-4)
  expect_equal(ld$y, c(-0.2573230, -1.1757393, -1.3741651, -1.3380623, -1.1787646, -0.1334681), tolerance = 1e-4)
  # expect_equal(ld$fill, rep("grey20", 6))
  expect_equal(ld$alpha, rep(0.2, 6))
})

test_that('autoplot.prcomp works for USArrests', {

  # fails on CRAN SPARC because components are inversed.
  skip_on_cran()

  obj <- stats::prcomp(USArrests)

  # scale
  exp_x <- c(0.10944879, 0.15678261, 0.20954726, 0.03097574, 0.18143395, 0.05907333)
  exp_y <- c(-0.11391408, -0.17894035, 0.08786745, -0.16621327, 0.22408730, 0.13651754)
  p <- ggplot2::autoplot(obj, label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomText' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$label, c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado"))
  expect_equal(ld$colour, rep("#000000", 6))

  # not scale
  exp_x <- c(64.80216, 92.82745, 124.06822, 18.34004, 107.42295, 34.97599)
  exp_y <- c(-11.448007, -17.982943, 8.830403, -16.703911, 22.520070, 13.719584)
  p <- ggplot2::autoplot(obj, scale = 0., label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomText' %in% class(p$layers[[2]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep("black", 6))
  ld <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$label, c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado"))
  expect_equal(ld$colour, rep("#000000", 6))
})

test_that('autoplot.princomp works for iris', {

  obj <- stats::princomp(iris[-5])

  p <- ggplot2::autoplot(obj, data = iris, colour = 'Species')
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))

  p <- ggplot2::autoplot(obj, data = iris, loadings = TRUE, loadings.label = FALSE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))

  p <- ggplot2::autoplot(obj, data = iris, loadings.label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 3)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomSegment' %in% class(p$layers[[2]]$geom))
  expect_true('GeomText' %in% class(p$layers[[3]]$geom))

  p <- ggplot2::autoplot(obj, data = iris, frame.type = 'convex')
  expect_true(is(p, 'ggplot'))
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomPolygon' %in% class(p$layers[[2]]$geom))

})

test_that('autoplot.prcomp plots the desired components', {

  # fails on CRAN SPARC because components are inversed.
  skip_on_cran()

  obj <- stats::prcomp(iris[-5])

  exp_x <- c(-0.0529391329513015, 0.0293374206773287, 0.0240249314006331,
             0.0527570984441502, -0.0541585777198945, -0.1228732921883)
  exp_y <- c(0.00815003672083961, 0.0614473273321588, -0.00522617400592586,
             -0.00921410146450414, -0.0262996114135432, -0.0492472720451544
  )

  p <- ggplot2::autoplot(obj, x = 2, y = 3)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld$x, exp_x, tolerance = 1e-4)
  expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep('black', 6))
  expect_equal(p$labels$x, "PC2 (5.31%)")
  expect_equal(p$labels$y, "PC3 (1.71%)")

})

test_that('autoplot.princomp plots the desired components', {

  # fails on CRAN and Travis because components are inversed.
  skip_on_travis()
  skip_on_cran()

  obj <- stats::princomp(iris[-5])

  exp_x <- c(-0.0531164839772263, 0.0294357037689672, 0.0241054171584106,
             0.052933839637522, -0.0543400139993682, -0.123284929161055)
  exp_y <- c(-0.00817734010291634, -0.0616531816016784, 0.00524368217559065,
             0.00924496956257505, 0.0263877175612184, 0.0494122549931978)


  p <- ggplot2::autoplot(obj, x = 2, y = 3)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  ld <- head(ggplot2:::layer_data(p, 1))
  # expect_equal(ld$x, exp_x, tolerance = 1e-4)
  # expect_equal(ld$y, exp_y, tolerance = 1e-4)
  expect_equal(ld$colour, rep('black', 6))
  expect_equal(p$labels$x, "Comp.2 (5.31%)")
  expect_equal(p$labels$y, "Comp.3 (1.71%)")

})

test_that('autoplot.factanal works for state.x77', {

  obj <- stats::factanal(state.x77, factors = 3, scores = 'regression')

  p <- ggplot2::autoplot(obj)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))

  p <- ggplot2::autoplot(obj, label = TRUE)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 2)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_true('GeomText' %in% class(p$layers[[2]]$geom))

})


test_that('autoplot.factanal plots the desired components', {

  obj <- stats::factanal(state.x77, factors = 3, scores = 'regression')

  p <- ggplot2::autoplot(obj, x = 2, y = 3)
  expect_true(is(p, 'ggplot'))
  expect_equal(length(p$layers), 1)
  expect_true('GeomPoint' %in% class(p$layers[[1]]$geom))
  expect_equal(p$labels$x, "Factor2 (20.15%)")
  expect_equal(p$labels$y, "Factor3 (18.24%)")
})

test_that('fortify.dist works for eurodist', {
  fortified <- ggplot2::fortify(eurodist)
  expect_true(is.data.frame(fortified))
  expect_equal(dim(fortified), c(21, 21))
})

test_that('fortify.lfda works for iris', {
  skip_on_cran()
  library(lfda)
  k <- iris[, -5]
  y <- iris[, 5]
  r <- 3
  model <- lfda(k, y, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_true(is.data.frame(fortified))
  model <- klfda(kmatrixGauss(k), y, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_true(is.data.frame(fortified))
  model <- self(k, y, beta=0.1, r, metric = "plain")
  fortified <- ggplot2::fortify(model)
  expect_true(is.data.frame(fortified))
})

test_that('autoplot.lfda works for iris', {
  skip_on_cran()

  k <- iris[, -5]
  y <- iris[, 5]
  r <- 4
  model <- lfda::lfda(k, y, r, metric = "plain")
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

  p <- autoplot(stepfun(c(1, 2, 3), c(4, 5, 6, 7)))
  expect_true(is(p, 'ggplot'))

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


test_that('autoplot.spec works', {
  result <- stats::spec.ar(AirPassengers)
  p <- autoplot(result)
  expect_true(is(p, 'ggplot'))

  expect_equal(sum(fortify(result)[1]), 1500, tolerance = 0.01)
  expect_equal(sum(fortify(result)[2]), 684799.7, tolerance = 0.01)
})
