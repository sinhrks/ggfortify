context('test ROCR performance')

test_that('fortify.performance works for ROCR demo objects', {
  # Load demo objects for two different scenarios
  skip_if_not_installed("ROCR")
  library(ROCR)
  data("ROCR.xval")     # multiple CV runs
  data("ROCR.simple")   # one run

  # Expect error if method is called on non-performance objects
  expect_error({
    bad_obj <- 4
    bad_fortified <- fortify.performance(bad_obj)
  }, regexp = 'Unsupported class for fortify.performance: numeric')


  # There are 4 possible types of ROCR performance objects for each of
  # the 2 prediction scenarios.
  # See ?ROCR::`performance-class` for definitions of the 4 types
  for (pred in list(ROCR::prediction(ROCR.xval$predictions,
                                     ROCR.xval$labels),
                    ROCR::prediction(ROCR.simple$predictions,
                                     ROCR.simple$labels))) {
    for (perf in list(ROCR::performance(pred, 'acc'),
                      ROCR::performance(pred, 'tpr', 'fpr'),
                      ROCR::performance(pred, 'ecost'),
                      ROCR::performance(pred, 'auc'))) {

      fortified <- ggplot2::fortify(perf)

      # Check if data.frame
      expect_true(is.data.frame(fortified))

      # Check if the column names are valid
      correct_names <- c('Repetition.Number', make.names(perf@x.name),
                         make.names(perf@y.name), make.names(perf@alpha.name))
      expect_true(all(names(fortified) %in% correct_names))
      # Check if they're in the correct order
      correct_names <- correct_names[correct_names %in% names(fortified)]
      expect_equal(names(fortified), correct_names)

      # Check if nrow is good
      expect_equal(nrow(fortified),
                   sum(vapply(perf@y.values, length, integer(1))))
    }
  }
})


test_that('autoplot.performance works for ROCR demo objects', {

  # Load demo objects for two different scenarios
  skip_if_not_installed("ROCR")
  library(ROCR)
  data("ROCR.xval")     # multiple CV runs
  data("ROCR.simple")   # one run

  for (pred in list(ROCR::prediction(ROCR.xval$predictions,
                                     ROCR.xval$labels),
                    ROCR::prediction(ROCR.simple$predictions,
                                     ROCR.simple$labels))) {
    for (perf in list(ROCR::performance(pred, 'acc'),
                      ROCR::performance(pred, 'tpr', 'fpr'),
                      ROCR::performance(pred, 'ecost'),
                      ROCR::performance(pred, 'auc'))) {

      # Make the autoplot
      if (length(unlist(perf@y.values)) == 1) {
        expect_warning({
          p <- ggplot2::autoplot(perf)
        }, regexp = paste('This histogram is more useful with multiple runs.',
                          'See ?ROCR::prediction'),
        fixed = TRUE)
      } else {
        p <- ggplot2::autoplot(perf)
      }

      # Ensure ggplot
      expect_true(is(p, 'ggplot'))

      # Check titles
      if (length(perf@x.values) == 0) {
        expect_equal(p$labels$title,
                     paste('Histogram of', make.names(perf@y.name)))
      } else {
        expect_equal(p$labels$title,
                     paste(make.names(perf@y.name), 'vs', make.names(perf@x.name)))
      }



      # Custom autoplot
      if (length(unlist(perf@y.values)) == 1) {
        expect_warning({
          p <- ggplot2::autoplot(perf, main = "test title", xlim = c(-1, 1))
        }, regexp = paste('This histogram is more useful with multiple runs.',
                          'See ?ROCR::prediction'),
        fixed = TRUE)
      } else {
        p <- ggplot2::autoplot(perf, main = "test title", xlim = c(-1, 1))
      }

      # Ensure ggplot
      expect_true(is(p, 'ggplot'))

      # Check titles
      expect_equal(p$labels$title, 'test title')

      # Check axis limits
      expect_equal(p$scales$scales[[1]]$limits, c(-1, 1))

    }
  }
})
