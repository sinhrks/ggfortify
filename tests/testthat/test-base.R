context('test base')

test_that('fortify.table works for Titanic', {
  
  fortified <- ggplot2::fortify(Titanic)
  expect_equal(is(fortified, 'tbl_df'), TRUE)

})

test_that('fortify.matrix works', {
  
  m <- matrix(1:6, nrow=2, ncol=3)
  fortified <- ggplot2::fortify(m)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(rownames(fortified), c('1', '2'))
  
  fortified <- ggplot2::fortify(m, compat = TRUE)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('V1', 'V2', 'V3'))
  expect_equal(rownames(fortified), c('1', '2'))
  
  m <- matrix(1:6, nrow=2, ncol=3)
  colnames(m) <- c('A', 'B', 'C')
  # dplyr doesn't guarantee rownames
  # rownames(m) <- c('X', 'Y')
  
  fortified <- ggplot2::fortify(m)
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('A', 'B', 'C'))
  # expect_equal(rownames(fortified), c('X', 'Y'))
})