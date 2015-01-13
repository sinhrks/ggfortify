context('test plotlib')

test_that('Check get.layout works', {
  
  expect_equal(ggfortify:::get.layout(5, 2, 0), t(matrix(1:6, 2, 3)))
  expect_equal(ggfortify:::get.layout(1, 2, 0), t(matrix(1:2, 2, 1)))
  expect_equal(ggfortify:::get.layout(2, 2, 0), t(matrix(1:2, 2, 1)))
  expect_equal(ggfortify:::get.layout(3, 2, 0), t(matrix(1:4, 2, 2)))
  
  expect_equal(ggfortify:::get.layout(8, 3, 0), t(matrix(1:9, 3, 3)))
  expect_equal(ggfortify:::get.layout(2, 3, 4), t(matrix(1:12, 3, 4)))
  
  expect_equal(ggfortify:::get.layout(5, 0, 3), t(matrix(1:6, 2, 3)))
  expect_equal(ggfortify:::get.layout(1, 0, 2), t(matrix(1:2, 1, 2)))
  expect_equal(ggfortify:::get.layout(2, 0, 3), t(matrix(1:3, 1, 3)))
  expect_equal(ggfortify:::get.layout(3, 0, 2), t(matrix(1:4, 2, 2)))
  
  expect_equal(ggfortify:::get.layout(3, 2, 2), t(matrix(1:4, 2, 2)))
  expect_equal(ggfortify:::get.layout(2, 1, 3), t(matrix(1:3, 1, 3)))
  expect_equal(ggfortify:::get.layout(3, 1, 3), t(matrix(1:3, 1, 3)))

})
