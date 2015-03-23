library(survival)

context('test survival')

test_that('fortify.survfit works for lung', {
  d.survfit <- survfit(Surv(time, status) ~ sex, data = lung)
  fortified <- ggplot2::fortify(d.survfit)

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(206, 9))

  d.survfit <- survfit(Surv(time, status) ~ 1, data = lung)
  fortified <- ggplot2::fortify(d.survfit)

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(186, 8))
})

test_that('fortify.survfit works for simple data', {
  tdata <- data.frame(time = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
                      status = rep(c(1, 0, 2), 4),
                      n = c(12, 3, 2, 6, 2, 4, 2, 0, 2, 3, 3, 5))
  fit  <- survfit(Surv(time, time, status, type='interval') ~1, data = tdata, weight = n)
  fortified <- fortify(fit)
  expected = data.frame(time = c(1, 2, 3, 4),
                        n.risk = c(44.000000000, 20.652979445, 9.318098786, 6.634779353),
                        n.event = c(20.347020555, 9.334880659, 2.683319433, 3.634779353),
                        n.censor = c(3, 2, 0, 3),
                        surv = c(0.53756771467, 0.29459403255, 0.20976021500, 0.09484575319),
                        std.err = c(0.1398238147, 0.2438966932, 0.3207623423, 0.5343228432),
                        upper = c(0.7070521134, 0.4751483078, 0.3933295002, 0.2702927012),
                        lower = c(0.40870968683, 0.18264959088, 0.11186383878, 0.03328139035))
  expect_equal(fortified, expected)
})

test_that('fortify.survfit.cox works for lung', {
  d.coxph <- coxph(Surv(time, status) ~ sex, data = lung)
  fortified <- ggplot2::fortify(survfit(d.coxph))

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'cumhaz')
  expect_equal(names(fortified), expected_names)
})
