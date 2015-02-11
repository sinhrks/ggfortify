context('test stats')

test_that('fortify.lm works for USArrests', {
  fortified <- ggplot2::fortify(lm(Murder ~ Assault + UrbanPop, data = USArrests))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Murder', 'Assault', 'UrbanPop', '.hat', '.sigma',
                      '.cooksd', '.fitted', '.resid', '.stdresid')
  expect_equal(colnames(fortified), expected_names)
  expect_equal(rownames(fortified), rownames(USArrests))

  fortified <- ggplot2::fortify(glm(Murder ~ Assault + UrbanPop, data = USArrests))
  expect_equal(is.data.frame(fortified), TRUE)

  expected_names <- c('Murder', 'Assault', 'UrbanPop', '.hat', '.sigma',
                      '.cooksd', '.fitted', '.resid', '.stdresid')
  expect_equal(colnames(fortified), expected_names)
  expect_equal(rownames(fortified), rownames(USArrests))
})

test_that('autplot.lm can accept + operator', {
  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), size = 5) + theme_bw()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 4)

  p <- autoplot(lm(Petal.Width~Petal.Length, data = iris), which = 1:6) + scale_colour_brewer()
  expect_true(is(p, 'ggmultiplot'))
  expect_equal(length(p@plots), 6)
})
