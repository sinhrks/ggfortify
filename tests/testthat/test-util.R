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

test_that('post_fortify', {
  df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  res <- ggfortify:::post_fortify(df)
  expect_equal(res, df)

  tbl <- dplyr::tibble(x = c(1, 2, 3), y = c(4, 5, 6))
  expect_true(is(tbl, 'tbl_df'))
  res <- ggfortify:::post_fortify(tbl)
  expect_equal(res, df)
  expect_true(!is(res, 'tbl_df'))

  res <- ggfortify:::post_fortify(tbl, klass = as.ts(tbl))
  expect_equal(res$x, df$x)
  expect_equal(res$y, df$y)
  expect_true('mts' %in% attr(res, 'base_class'))
})
