context('test plotlib')

test_that('Check get.layout works', {

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

  # Unable to compare geom_xxx each other, because it is an environment variable
  # Thus, capture the printed result and check equalities

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species')
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping: shape = Species ",
                  "geom_point: na.rm = FALSE ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping: shape = Species ",
                  "geom_point: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species', size = 10)
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping: shape = Species ",
                  "geom_point: na.rm = FALSE, size = 10 ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping: shape = Species ",
                  "geom_point: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species',
                                     size = 'Sepal.Width')
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping: shape = Species, size = Sepal.Width ",
                  "geom_point: na.rm = FALSE ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping: shape = Species, size = Sepal.Width ",
                  "geom_point: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 2,
                                     colour = 'Species', size = 'Sepal.Width')
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping: colour = Species, size = Sepal.Width ",
                  "geom_point: na.rm = FALSE, shape = 2 ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping: colour = Species, size = Sepal.Width ",
                  "geom_point: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris)
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping:  ",
                  "geom_line:  ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping:  ",
                  "geom_line: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed')
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping:  ",
                  "geom_line: linetype = dashed ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping:  ",
                  "geom_line: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed',
                                     colour = 'Species')
  if (packageVersion("ggplot2") <= '1.0.1') {
    expected <- c("mapping: colour = Species ",
                  "geom_line: linetype = dashed ",
                  "stat_identity:  ",
                  "position_identity: (width = NULL, height = NULL)")
  } else {
    expected <- c("mapping: colour = Species ",
                  "geom_line: na.rm = FALSE",
                  "stat_identity: na.rm = FALSE",
                  "position_identity ")
  }
  expect_equal(capture.output(print(result)), expected)
})
