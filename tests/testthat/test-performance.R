context('test ROCR performance')

# Load demo objects for two scenarios
library(ROCR)
data("ROCR.xval")     # multiple CV runs
data("ROCR.simple")   # one run

# Create prediction objects for both scenarios
pred1 <- ROCR::prediction(ROCR.xval$predictions,
                          ROCR.xval$labels)
pred2 <- ROCR::prediction(ROCR.simple$predictions,
                          ROCR.simple$labels)

# Four possible types of ROCR performance objects for each prediction object
# See ?ROCR::`performance-class` for definitions of the four types
perf1.1 <- ROCR::performance(pred1, 'acc')
perf1.2 <- ROCR::performance(pred1, 'tpr', 'fpr')
perf1.3 <- ROCR::performance(pred1, 'ecost')
perf1.4 <- ROCR::performance(pred1, 'auc')
perf2.1 <- ROCR::performance(pred2, 'acc')
perf2.2 <- ROCR::performance(pred2, 'tpr', 'fpr')
perf2.3 <- ROCR::performance(pred2, 'ecost')
perf2.4 <- ROCR::performance(pred2, 'auc')



test_that('fortify.performance works for ROCR demo objects', {

  # Fortify our 8 test objects
  fortified1.1 <- ggplot2::fortify(perf1.1)
  fortified1.2 <- ggplot2::fortify(perf1.2)
  fortified1.3 <- ggplot2::fortify(perf1.3)
  fortified1.4 <- ggplot2::fortify(perf1.4)
  fortified2.1 <- ggplot2::fortify(perf2.1)
  fortified2.2 <- ggplot2::fortify(perf2.2)
  fortified2.3 <- ggplot2::fortify(perf2.3)
  fortified2.4 <- ggplot2::fortify(perf2.4)


  # Check if data.frame
  expect_true(is.data.frame(fortified1.1))
  expect_true(is.data.frame(fortified1.2))
  expect_true(is.data.frame(fortified1.3))
  expect_true(is.data.frame(fortified1.4))
  expect_true(is.data.frame(fortified2.1))
  expect_true(is.data.frame(fortified2.2))
  expect_true(is.data.frame(fortified2.3))
  expect_true(is.data.frame(fortified2.4))

  # Check if names are good
  expect_equal(names(fortified1.1), c('Repetition.Number', 'Cutoff', 'Accuracy'))
  expect_equal(names(fortified1.2), c('Repetition.Number', 'False.positive.rate',
                                    'True.positive.rate', 'Cutoff'))
  expect_equal(names(fortified1.3), c('Repetition.Number', 'None', 'Expected.cost'))
  expect_equal(names(fortified1.4), c('Repetition.Number', 'Area.under.the.ROC.curve'))
  expect_equal(names(fortified2.1), c('Repetition.Number', 'Cutoff', 'Accuracy'))
  expect_equal(names(fortified2.2), c('Repetition.Number', 'False.positive.rate',
                                    'True.positive.rate', 'Cutoff'))
  expect_equal(names(fortified2.3), c('Repetition.Number', 'None', 'Expected.cost'))
  expect_equal(names(fortified2.4), c('Repetition.Number', 'Area.under.the.ROC.curve'))

  # Check if nrow is good
  expect_equal(nrow(fortified1.1),
               sum(vapply(perf1.1@y.values, length, integer(1))))
  expect_equal(nrow(fortified1.2),
               sum(vapply(perf1.2@y.values, length, integer(1))))
  expect_equal(nrow(fortified1.3),
               sum(vapply(perf1.3@y.values, length, integer(1))))
  expect_equal(nrow(fortified1.4),
               sum(vapply(perf1.4@y.values, length, integer(1))))
  expect_equal(nrow(fortified2.1),
               sum(vapply(perf2.1@y.values, length, integer(1))))
  expect_equal(nrow(fortified2.2),
               sum(vapply(perf2.2@y.values, length, integer(1))))
  expect_equal(nrow(fortified2.3),
               sum(vapply(perf2.3@y.values, length, integer(1))))
  expect_equal(nrow(fortified2.4),
               sum(vapply(perf2.4@y.values, length, integer(1))))

})


test_that('autoplot.performance works for ROCR demo objects', {

  # Autoplot our 8 test objects
  p1.1 <- ggplot2::autoplot(perf1.1)
  p1.2 <- ggplot2::autoplot(perf1.2)
  p1.3 <- ggplot2::autoplot(perf1.3)
  p1.4 <- ggplot2::autoplot(perf1.4)
  p2.1 <- ggplot2::autoplot(perf2.1)
  p2.2 <- ggplot2::autoplot(perf2.2)
  p2.3 <- ggplot2::autoplot(perf2.3)
  expect_warning({
    p2.4 <- ggplot2::autoplot(perf2.4)
  }, regexp = paste('This histogram is more useful with multiple runs.',
                    'See ?ROCR::prediction'),
  fixed = TRUE)

  # Ensure ggplot
  expect_true(is(p1.1, 'ggplot'))
  expect_true(is(p1.2, 'ggplot'))
  expect_true(is(p1.3, 'ggplot'))
  expect_true(is(p1.4, 'ggplot'))
  expect_true(is(p2.1, 'ggplot'))
  expect_true(is(p2.2, 'ggplot'))
  expect_true(is(p2.3, 'ggplot'))
  expect_true(is(p2.4, 'ggplot'))

  # Check titles
  expect_equal(p1.1$labels$title, 'Accuracy vs Cutoff')
  expect_equal(p1.2$labels$title, 'True.positive.rate vs False.positive.rate')
  expect_equal(p1.3$labels$title, 'Expected.cost vs None')
  expect_equal(p1.4$labels$title, 'Histogram of Area.under.the.ROC.curve')
  expect_equal(p2.1$labels$title, 'Accuracy vs Cutoff')
  expect_equal(p2.2$labels$title, 'True.positive.rate vs False.positive.rate')
  expect_equal(p2.3$labels$title, 'Expected.cost vs None')
  expect_equal(p2.4$labels$title, 'Histogram of Area.under.the.ROC.curve')




  # Custom plots
  p1.1 <- ggplot2::autoplot(perf1.1, main = "test title", xlim = c(-1, 1))
  p1.2 <- ggplot2::autoplot(perf1.2, main = "test title", xlim = c(-1, 1))
  p1.3 <- ggplot2::autoplot(perf1.3, main = "test title", xlim = c(-1, 1))
  p1.4 <- ggplot2::autoplot(perf1.4, main = "test title", xlim = c(-1, 1))
  p2.1 <- ggplot2::autoplot(perf2.1, main = "test title", xlim = c(-1, 1))
  p2.2 <- ggplot2::autoplot(perf2.2, main = "test title", xlim = c(-1, 1))
  p2.3 <- ggplot2::autoplot(perf2.3, main = "test title", xlim = c(-1, 1))
  expect_warning({
    p2.4 <- ggplot2::autoplot(perf2.4, main = "test title", xlim = c(-1, 1))
  }, regexp = paste('This histogram is more useful with multiple runs.',
                    'See ?ROCR::prediction'),
  fixed = TRUE)

  # Ensure ggplot
  expect_true(is(p1.1, 'ggplot'))
  expect_true(is(p1.2, 'ggplot'))
  expect_true(is(p1.3, 'ggplot'))
  expect_true(is(p1.4, 'ggplot'))
  expect_true(is(p2.1, 'ggplot'))
  expect_true(is(p2.2, 'ggplot'))
  expect_true(is(p2.3, 'ggplot'))
  expect_true(is(p2.4, 'ggplot'))

  # Check titles
  expect_equal(p1.1$labels$title, 'test title')
  expect_equal(p1.2$labels$title, 'test title')
  expect_equal(p1.3$labels$title, 'test title')
  expect_equal(p1.4$labels$title, 'test title')
  expect_equal(p2.1$labels$title, 'test title')
  expect_equal(p2.2$labels$title, 'test title')
  expect_equal(p2.3$labels$title, 'test title')
  expect_equal(p2.4$labels$title, 'test title')

  # Check axis limits
  expect_equal(p1.1$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p1.2$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p1.3$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p1.4$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p2.1$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p2.2$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p2.3$scales$scales[[1]]$limits, c(-1, 1))
  expect_equal(p2.4$scales$scales[[1]]$limits, c(-1, 1))


})
