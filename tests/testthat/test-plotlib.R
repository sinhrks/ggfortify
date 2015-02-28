context('test plotlib')

test_that('Check get.layout works', {
  p <- ggplot(iris, aes(Petal.Width, Petal.Length)) + geom_point()
  mp <- ggmultiplot$new(plots = list(), ncol = 2)
  expect_equal(mp$get_layout(5), t(matrix(0, 2, 3)))
  expect_equal(mp$get_layout(1), t(matrix(0, 2, 1)))
  expect_equal(mp$get_layout(2), t(matrix(0, 2, 1)))
  expect_equal(mp$get_layout(3), t(matrix(0, 2, 2)))

  mp <- ggmultiplot$new(plots = list(), ncol = 3)
  expect_equal(mp$get_layout(8), t(matrix(0, 3, 3)))
  expect_equal(mp$get_layout(12), t(matrix(0, 3, 4)))

  mp <- ggmultiplot$new(plots = list(), nrow = 3)
  expect_equal(mp$get_layout(5), t(matrix(0, 2, 3)))
  expect_equal(mp$get_layout(2), t(matrix(0, 1, 3)))

  mp <- ggmultiplot$new(plots = list(), nrow = 2)
  expect_equal(mp$get_layout(1), t(matrix(0, 1, 2)))
  expect_equal(mp$get_layout(3), t(matrix(0, 2, 2)))

  mp <- ggmultiplot$new(plots = list(), ncol = 2, nrow = 2)
  expect_equal(mp$get_layout(3), t(matrix(0, 2, 2)))

  mp <- ggmultiplot$new(plots = list(), ncol = 1, nrow = 3)
  expect_equal(mp$get_layout(2), t(matrix(0, 1, 3)))
  expect_equal(mp$get_layout(3), t(matrix(0, 1, 3)))

})

test_that('Check geom_factory works', {

  # Unable to compare geom_xxx each other, because it is an environment variable
  # Thus, capture the printed result and check equalities

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species')
  expected <- c("mapping: shape = Species ",
                "geom_point: na.rm = FALSE ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species', size = 10)
  expected <- c("mapping: shape = Species ",
                "geom_point: na.rm = FALSE, size = 10 ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 'Species',
                                     size = 'Sepal.Width')
  expected <- c("mapping: shape = Species, size = Sepal.Width ",
                "geom_point: na.rm = FALSE ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_point, iris, shape = 2,
                                     colour = 'Species', size = 'Sepal.Width')
  expected <- c("mapping: colour = Species, size = Sepal.Width ",
                "geom_point: na.rm = FALSE, shape = 2 ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris)
  expected <- c("mapping:  ",
                "geom_line:  ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed')
  expected <- c("mapping:  ",
                "geom_line: linetype = dashed ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)

  result <- ggfortify:::geom_factory(geom_line, iris, linetype = 'dashed',
                                     colour = 'Species')
  expected <- c("mapping: colour = Species ",
                "geom_line: linetype = dashed ",
                "stat_identity:  ",
                "position_identity: (width = NULL, height = NULL)")
  expect_equal(capture.output(print(result)), expected)
})
