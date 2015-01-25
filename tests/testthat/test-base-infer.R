context('test base-infer')

test_that('infer works for MDS-likes', {
  # MDS
  data(eurodist)
  expect_equal(infer(cmdscale(eurodist, eig = TRUE)), 'mds-like')
  expect_equal(infer(cmdscale(eurodist, add = TRUE)), 'mds-like')
  expect_equal(infer(cmdscale(eurodist, x.ret = TRUE)), 'mds-like')
  library(MASS)
  expect_equal(infer(isoMDS(eurodist)), 'mds-like')
  expect_equal(infer(sammon(eurodist)), 'mds-like')  
})

test_that('fortify works for MDS-likes', {
  # MDS
  data(eurodist)
  fortified <- fortify(cmdscale(eurodist, eig = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2'))
  expect_equal(nrow(fortified), 21)
  
  fortified <- fortify(cmdscale(eurodist, x.ret = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2'))
  expect_equal(nrow(fortified), 21)
  
  fortified <- fortify(cmdscale(eurodist, k = 4, add = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3', '4'))
  expect_equal(nrow(fortified), 21)
  
  library(MASS)
  expect_equal(infer(isoMDS(eurodist)), 'mds-like')
  fortified <- fortify(cmdscale(eurodist, k = 3, add = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(nrow(fortified), 21)
 
  expect_equal(infer(sammon(eurodist)), 'mds-like')
  fortified <- fortify(cmdscale(eurodist, k = 3, add = TRUE))
  expect_equal(is(fortified, 'tbl_df'), TRUE)
  expect_equal(colnames(fortified), c('1', '2', '3'))
  expect_equal(nrow(fortified), 21) 
})