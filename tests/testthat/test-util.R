context('test util')

test_that('unscale', {
  df <- iris[c(1, 2, 3, 4)]
  expect_equal(ggfortify::unscale(scale(df)), df)
  expect_equal(ggfortify::unscale(scale(df, center = FALSE)), df)
  expect_equal(ggfortify::unscale(scale(df, scale = FALSE)), df)
  expect_equal(ggfortify::unscale(scale(df, center = FALSE, scale = FALSE)), df)

  scaled <- scale(df)
  center <- attr(scaled, 'scaled:center')
  scale <- attr(scaled, 'scaled:scale')
  expect_equal(ggfortify::unscale(scaled, center = center, scale = scale), df)

  scaled <- scale(df, center = FALSE)
  scale <- attr(scaled, 'scaled:scale')
  expect_equal(ggfortify::unscale(scaled, center = FALSE, scale = scale), df)

  scaled <- scale(df, scale = FALSE)
  center <- attr(scaled, 'scaled:center')
  expect_equal(ggfortify::unscale(scaled, center = center, scale = FALSE), df)

  scaled <- scale(df, center = FALSE, scale = FALSE)
  expect_equal(ggfortify::unscale(scaled, center = FALSE, scale = FALSE), df)
})


