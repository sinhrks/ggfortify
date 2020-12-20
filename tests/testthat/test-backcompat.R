test_that('deprecated in v0.0.2:: matrix.scale', {
  skip_on_cran()
  skip_on_travis()
  expect_warning(autoplot(matrix(rnorm(20), nc = 5),
                          scale = scale_fill_gradient(low = 'red', high = 'blue')))
})
