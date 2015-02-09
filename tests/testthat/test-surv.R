library(survival)

context('test survival')

test_that('fortify.survfit works for survival::lung', {
  d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
  fortified <- ggplot2::fortify(d.survfit)

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
})

test_that('fortify.survfit.cox works for survival::lung', {
  d.coxph <- survival::coxph(survival::Surv(time, status) ~ sex, data = survival::lung)
  fortified <- ggplot2::fortify(survival::survfit(d.coxph))

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'cumhaz')
  expect_equal(names(fortified), expected_names)
})
