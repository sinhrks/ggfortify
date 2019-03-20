context('test plotlib')

test_that('Check ggmultiplot arithmetics', {
  skip_on_cran()
  skip_on_travis()
  p1 <- autoplot(lm(Petal.Width ~ Petal.Length, data = iris))
  p2 <- autoplot(lm(Sepal.Width ~ Sepal.Length, data = iris))
  expect_true(is(p1, 'ggmultiplot'))
  expect_true(is(p2, 'ggmultiplot'))

  res <- p1 + p2
  expect_true(is(res, 'ggmultiplot'))
  expect_equal(length(res@plots), 8)

  res <- p1 + (ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) + geom_point())
  expect_true(is(res, 'ggmultiplot'))
  expect_equal(length(res@plots), 5)

  res <- res + theme_bw()
  expect_true(is(res, 'ggmultiplot'))
  expect_equal(length(res@plots), 5)
})


test_that('Check ggmultiplot extraction', {
  skip_on_cran()
  skip_on_travis()
  p <- autoplot(lm(Petal.Width ~ Petal.Length, data = iris))
  expect_equal(length(p), 4)

  # getter

  res <- p[1]
  expect_true(is(res, 'ggmultiplot'))
  expect_equal(length(res), 1)

  res <- p[2:3]
  expect_true(is(res, 'ggmultiplot'))
  expect_equal(length(res), 2)

  res <- p[[1]]
  expect_true(is(res, 'ggplot'))

  # setter
  p[1] <- p[1] # same length
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 4)

  p[2:3] <- p[1:2] # same length
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 4)

  p[1] <- p[[1]] # same length (set ggplot)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 4)

  # different length
  temp <- function(x) {
    x[2:4] <- x[1:2]
  }
  expect_error(temp(p), 'Unable to set value, length mismatch')

  # different length (set ggplot to slice)
  temp <- function(x) {
    x[2:4] <- x[[1]]
  }
  expect_error(temp(p), 'Unable to set ggplot to multiple slice')

  # invalid value
  temp <- function(x) {
    x[2:4] <- 'xxx'
  }
  expect_error(temp(p), 'Unable to set type, unsupported type')

  # setter
  p[[1]] <- p[1] # same length
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 4)

  p[[1]] <- p[[1]] # same length (set ggplot)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 4)

  # different length
  temp <- function(x) {
    x[[1]] <- x[1:2]
  }
  expect_error(temp(p), 'Unable to set value, length mismatch')

  # invalid value
  temp <- function(x) {
    x[2:4] <- 'xxx'
  }
  expect_error(temp(p), 'Unable to set type, unsupported type')
})

test_that('Check ggmultiplot multiple instances', {
  skip_on_cran()
  skip_on_travis()
  res <- lapply(c(3, 4, 5), function(x) kmeans(iris[-5], x))
  p <- autoplot(res, data = iris[-5])
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 3)

  p <- autoplot(list(a = AirPassengers, b = AirPassengers))
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 2)

  library(survival)
  sf <- survfit(Surv(time, status) ~ sex, data = lung)
  res <- list(a = sf, b = sf, c = sf)
  p <- autoplot(res)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 3)

  res <- list(a = lm(Sepal.Width ~ Sepal.Length, data = iris),
              b = lm(Petal.Width ~ Petal.Length, data = iris))
  p <- autoplot(res, ncol = 4)
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 8)

  p <- autoplot(list(a=Canada, b=AirPassengers))
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p), 2)
})

test_that('Check get.layout works', {
  skip_on_cran()
  skip_on_travis()
  expect_equal(ggfortify:::get.layout(5, 2, 0), t(matrix(1:6, 2, 3)))
  expect_equal(ggfortify:::get.layout(1, 2, 0), t(matrix(1:2, 2, 1)))
  expect_equal(ggfortify:::get.layout(2, 2, 0), t(matrix(1:2, 2, 1)))
  expect_equal(ggfortify:::get.layout(3, 2, 0), t(matrix(1:4, 2, 2)))

  expect_equal(ggfortify:::get.layout(8, 3, 0), t(matrix(1:9, 3, 3)))
  expect_equal(ggfortify:::get.layout(2, 3, 4), t(matrix(1:12, 3, 4)))

  expect_equal(ggfortify:::get.layout(5, 0, 3), t(matrix(1:6, 2, 3)))
  expect_equal(ggfortify:::get.layout(1, 0, 2), t(matrix(1:2, 1, 2)))
  expect_equal(ggfortify:::get.layout(2, 0, 3), t(matrix(1:3, 1, 3)))
  expect_equal(ggfortify:::get.layout(3, 0, 2), t(matrix(1:4, 2, 2)))

  expect_equal(ggfortify:::get.layout(3, 2, 2), t(matrix(1:4, 2, 2)))
  expect_equal(ggfortify:::get.layout(2, 1, 3), t(matrix(1:3, 1, 3)))
  expect_equal(ggfortify:::get.layout(3, 1, 3), t(matrix(1:3, 1, 3)))

})

test_that('Check geom_factory works', {
  skip_on_cran()
  skip_on_travis()
  # Unable to compare geom_xxx each other, because it is an environment variable
  # Thus, capture the printed result and check equalities

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species')
  expected <- c("mapping: shape = ~Species ",
                "geom_point: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")
  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species', size = 10)
  expected <- c("mapping: shape = ~Species ",
                "geom_point: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species',
                                     size = 'Sepal.Width')
  expected <- c("mapping: shape = ~Species, size = ~Sepal.Width ",
                "geom_point: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 2,
                                     colour = 'Species', size = 'Sepal.Width')
  expected <- c("mapping: colour = ~Species, size = ~Sepal.Width ",
                "geom_point: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris)
  expected <- c("mapping:  ",
                "geom_line: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed')
  expected <- c("mapping:  ",
                "geom_line: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed',
                                     colour = 'Species')
  expected <- c("mapping: colour = ~Species ",
                "geom_line: na.rm = FALSE",
                "stat_identity: na.rm = FALSE",
                "position_identity ")

  if (utils::packageVersion("ggplot2") >= "2.3.0") expect_equal(capture.output(print(result)), expected)
})

test_that('Check autoplot works for list of ggplot', {
  skip_on_cran()
  skip_on_travis()
  library(dplyr)
  p <- iris %>% ggplot(aes(Sepal.Length, Petal.Length)) + geom_point()
  plots <- iris %>% group_by(Species) %>%
    do(plots = p %+% . + facet_wrap(~Species))
  p <- autoplot(plots$plots)
  expect_true(inherits(p, 'ggmultiplot'))
})

test_that('Check autoplot works for list of ggpmultilot', {
  skip_on_cran()
  skip_on_travis()
  library(dplyr)
  plots <- iris %>% group_by(Species) %>%
    do(plots = autoplot(lm(Petal.Width ~ Petal.Length, data = .)))
  p <- autoplot(plots$plots)
  expect_true(inherits(p, 'ggmultiplot'))
})
