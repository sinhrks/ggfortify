context('test ROCR performance')

test_that('fortify.performance works for ROCR demo object', {

  # Load demo object
  library(ROCR)
  data("ROCR.xval")
  # Create prediction object
  pred <- ROCR::prediction(ROCR.xval$predictions,
                           ROCR.xval$labels)

  # Four possible kinds of ROCR performance objects
  perf1 <- ROCR::performance(pred, 'acc')
  perf2 <- ROCR::performance(pred, 'tpr', 'fpr')
  perf3 <- ROCR::performance(pred, 'ecost')
  perf4 <- ROCR::performance(pred, 'auc')

  # fortified1 <- ggplot2::fortify(perf1)
  # fortified2 <- ggplot2::fortify(perf2)
  # fortified3 <- ggplot2::fortify(perf3)
  # fortified4 <- ggplot2::fortify(perf4)

  fortified1 <- fortify.performance(perf1)
  fortified2 <- fortify.performance(perf2)
  fortified3 <- fortify.performance(perf3)
  fortified4 <- fortify.performance(perf4)

  # Check if data.frame
  expect_true(is.data.frame(fortified1))
  expect_true(is.data.frame(fortified2))
  expect_true(is.data.frame(fortified3))
  expect_true(is.data.frame(fortified4))

  # Check if names are good
  expect_equal(names(fortified1),
               c('Repetition.Number', 'Cutoff', 'Accuracy'))
  expect_equal(names(fortified2),
               c('Repetition.Number', 'False.positive.rate',
                 'True.positive.rate', 'Cutoff'))
  expect_equal(names(fortified3),
               c('Repetition.Number', 'None', 'Expected.cost'))
  expect_equal(names(fortified4),
               c('Repetition.Number', 'Area.under.the.ROC.curve'))

  # Check if nrow is good
  expect_equal(nrow(fortified1),
               sum(vapply(perf1@y.values, length, integer(1))))
  expect_equal(nrow(fortified2),
               sum(vapply(perf2@y.values, length, integer(1))))
  expect_equal(nrow(fortified3),
               sum(vapply(perf3@y.values, length, integer(1))))
  expect_equal(nrow(fortified4),
               sum(vapply(perf4@y.values, length, integer(1))))




})

