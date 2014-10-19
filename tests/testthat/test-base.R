context('test base')

test_that('fortify.table works for Titanic', {
  
  fortified <- ggplot2::fortify(Titanic)
  expect_equal(is(fortified, 'tbl_df'), TRUE)

})

test_that('fortify.matrix works', {
  
  fortified <- ggplot2::fortify(matrix(1:6, nrow=2, ncol=3))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  
})