context('test base')

test_that('fortify.table works for Titanic', {
  # MDS
  data(eurodist)
  expect_equal(infer(cmdscale(eurodist, eig = TRUE)), 'mds-like')
  library(MASS)
  expect_equal(infer(isoMDS(eurodist)), 'mds-like')
  expect_equal(infer(sammon(eurodist)), 'mds-like')  
})
