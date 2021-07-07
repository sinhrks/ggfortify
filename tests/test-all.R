library(testthat)

suppressWarnings(RNGversion("3.5.0"))
set.seed(1, sample.kind = "Rejection")

test_check('ggfortify')
