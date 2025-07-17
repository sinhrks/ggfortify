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

test_that('remove_conflicting_columns works correctly', {
  # Test basic functionality
  df1 <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  df2 <- data.frame(b = 10:12, d = 13:15)
  df3 <- data.frame(c = 16:18, e = 19:21)

  result <- remove_conflicting_columns(df1, df2, df3)

  # Should only keep column 'a' since 'b' and 'c' conflict
  expect_equal(names(result), "a")
  expect_equal(result$a, df1$a)
  expect_equal(nrow(result), nrow(df1))
  expect_equal(rownames(result), rownames(df1))
})

test_that('remove_conflicting_columns handles NULL data', {
  df1 <- data.frame(a = 1:3, b = 4:6)

  # NULL data should return NULL
  expect_null(remove_conflicting_columns(NULL, df1))

  # NULL in ... should be ignored
  result <- remove_conflicting_columns(df1, NULL, data.frame(c = 1:3))
  expect_equal(names(result), c("a", "b"))
  expect_equal(result, df1)
})

test_that('remove_conflicting_columns handles no conflicts', {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(c = 7:9, d = 10:12)

  result <- remove_conflicting_columns(df1, df2)

  # No conflicts, should return original data
  expect_equal(result, df1)
  expect_equal(names(result), names(df1))
})

test_that('remove_conflicting_columns handles all columns conflicting', {
  df1 <- data.frame(a = 1:3, b = 4:6, row.names = paste0("row", 1:3))
  df2 <- data.frame(a = 7:9, b = 10:12)

  result <- remove_conflicting_columns(df1, df2)

  # All columns conflict, should return empty data frame with same row structure
  expect_equal(ncol(result), 0)
  expect_equal(nrow(result), 0)  # The [FALSE, , drop = FALSE] creates 0 rows
  expect_true(is.data.frame(result))
})

test_that('remove_conflicting_columns handles no other dataframes', {
  df1 <- data.frame(a = 1:3, b = 4:6)

  result <- remove_conflicting_columns(df1)

  # No other dataframes, should return original
  expect_equal(result, df1)
})

test_that('remove_conflicting_columns handles matrices via fortify', {
  df1 <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  mat1 <- matrix(10:15, ncol = 2, dimnames = list(NULL, c("b", "x")))
  mat2 <- matrix(16:21, ncol = 2, dimnames = list(NULL, c("c", "y")))

  result <- remove_conflicting_columns(df1, mat1, mat2)
  expect_equal(names(result), "a")
  expect_equal(result$a, df1$a)

  result <- remove_conflicting_columns(mat1, df1)
  expect_equal(names(result), "x")
  expect_equal(result$x, mat1[,"x"])
})

test_that('remove_conflicting_columns preserves row names and structure', {
  df1 <- data.frame(a = 1:3, b = 4:6, c = 7:9, row.names = paste0("sample_", 1:3))
  df2 <- data.frame(b = 10:12, x = 13:15)

  result <- remove_conflicting_columns(df1, df2)

  # Should preserve row names
  expect_equal(rownames(result), rownames(df1))
  expect_equal(names(result), c("a", "c"))
  expect_equal(result$a, df1$a)
  expect_equal(result$c, df1$c)
})

test_that('remove_conflicting_columns handles partial conflicts', {
  df1 <- data.frame(PC1 = 1:4, PC2 = 5:8, Species = letters[1:4], extra = 9:12)
  df2 <- data.frame(PC1 = 101:104, PC3 = 105:108)  # Only PC1 conflicts

  result <- remove_conflicting_columns(df1, df2)

  # Should remove only PC1, keep PC2, Species, extra
  expect_equal(names(result), c("PC2", "Species", "extra"))
  expect_equal(result$PC2, df1$PC2)
  expect_equal(result$Species, df1$Species)
  expect_equal(result$extra, df1$extra)
})

test_that('remove_conflicting_columns handles multiple conflicting dataframes', {
  df1 <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15)
  df2 <- data.frame(a = 101:103, x = 104:106)
  df3 <- data.frame(b = 201:203, y = 204:206)
  df4 <- data.frame(c = 301:303, z = 304:306)

  result <- remove_conflicting_columns(df1, df2, df3, df4)

  # Should remove a, b, c but keep d, e
  expect_equal(names(result), c("d", "e"))
  expect_equal(result$d, df1$d)
  expect_equal(result$e, df1$e)
})

test_that('remove_conflicting_columns handles empty dataframes', {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df_empty <- data.frame()

  result <- remove_conflicting_columns(df1, df_empty)

  # Empty dataframe has no column names, so no conflicts
  expect_equal(result, df1)
})

test_that('remove_conflicting_columns preserves data types', {
  df1 <- data.frame(
    numeric_col = 1:3,
    character_col = c("a", "b", "c"),
    logical_col = c(TRUE, FALSE, TRUE),
    factor_col = factor(c("x", "y", "z")),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(conflicting_col = 4:6)

  result <- remove_conflicting_columns(df1, df2)

  # Should preserve all original columns and their types
  expect_equal(result, df1)
  expect_equal(sapply(result, class), sapply(df1, class))
})

test_that('remove_conflicting_columns handles mixed object types via fortify', {
  df1 <- data.frame(PC1 = 1:4, PC2 = 5:8, Species = letters[1:4])
  mat1 <- matrix(101:108, ncol = 2, dimnames = list(NULL, c("PC1", "NewCol")))
  mat2 <- matrix(201:208, ncol = 2, dimnames = list(NULL, c("PC2", "AnotherCol")))

  result <- remove_conflicting_columns(df1, mat1, mat2)
  expect_equal(names(result), "Species")
  expect_equal(result$Species, df1$Species)

  result <- remove_conflicting_columns(mat1, mat2, df1)
  expect_equal(names(result), "NewCol")
  expect_equal(result$NewCol, mat1[, "NewCol"])

})

test_that('remove_conflicting_columns handles matrices without column names', {
  df1 <- data.frame(a = 1:3, b = 4:6)
  mat_no_names <- matrix(1:6, ncol = 2)  # No column names

  result <- remove_conflicting_columns(df1, mat_no_names)

  # Matrix without column names should not cause conflicts
  expect_equal(result, df1)
})

test_that('remove_conflicting_columns handles edge case with duplicate column names in ...', {
  df1 <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  df2 <- data.frame(a = 10:12, x = 13:15)
  df3 <- data.frame(a = 16:18, y = 19:21)  # 'a' appears in both df2 and df3

  result <- remove_conflicting_columns(df1, df2, df3)

  # Should remove 'a' only once, keep 'b' and 'c'
  expect_equal(names(result), c("b", "c"))
  expect_equal(result$b, df1$b)
  expect_equal(result$c, df1$c)
})
