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

  fortified <- ggplot2::fortify(d.survfit, surv.connect = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(208, 9))
  expected <- data.frame(time = c(0, 0), n.risk = c(138, 138), n.event = c(3, 3), n.censor = c(0, 0),
                         surv = c(1, 1), std.err = c(0, 0), upper = c(1, 1), lower = c(1, 1),
                         strata = as.factor(c('sex=1', 'sex=2')))
  expect_equal(fortified[1:2, ], expected)

  d.survfit <- survfit(Surv(time, status) ~ 1, data = lung)
  fortified <- ggplot2::fortify(d.survfit)

  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(186, 8))

  fortified <- ggplot2::fortify(d.survfit, surv.connect = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(187, 8))
  expected <- data.frame(time = 0, n.risk = 228, n.event = 1, n.censor = 0,
                         surv = 1, std.err = 0, upper = 1, lower = 1)
  expect_equal(fortified[1, ], expected)
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

  fortified <- fortify(fit, surv.connect = TRUE)
  expected = data.frame(time = c(0, 1, 2, 3, 4),
                        n.risk = c(44.000000000, 44.000000000, 20.652979445, 9.318098786, 6.634779353),
                        n.event = c(20.347020555, 20.347020555, 9.334880659, 2.683319433, 3.634779353),
                        n.censor = c(0, 3, 2, 0, 3),
                        surv = c(1.0, 0.53756771467, 0.29459403255, 0.20976021500, 0.09484575319),
                        std.err = c(0, 0.1398238147, 0.2438966932, 0.3207623423, 0.5343228432),
                        upper = c(1.0, 0.7070521134, 0.4751483078, 0.3933295002, 0.2702927012),
                        lower = c(1.0, 0.40870968683, 0.18264959088, 0.11186383878, 0.03328139035))
  expect_equal(fortified, expected)

  tdata <- data.frame(time = c(1, 1, 2, 2, 3, 3, 4, 4),
                      status = rep(c(1, 2), 4),
                      n = c(24, 3, 20, 4, 18, 2, 15, 3))

  fit <- survfit(coxph(Surv(time, status) ~ 1, data = tdata))
  fortified <- fortify(fit)
  expected = data.frame(time = c(1, 2, 3, 4),
                        n.risk = c(8, 6, 4, 2),
                        n.event = c(1, 1, 1, 1),
                        n.censor = c(1, 1, 1, 1),
                        surv = c(0.8824969026, 0.7470175003, 0.5817778142, 0.3528660815),
                        std.err = c(0.1250000000, 0.2083333333, 0.3254270698, 0.5965758776),
                        upper = c(1, 1, 1, 1),
                        lower = c(0.6907374403, 0.4965890298, 0.3074348749, 0.1095982468),
                        cumhaz = c(0.1250000000, 0.2916666667, 0.5416666667,1.0416666667))
  expect_equal(fortified, expected)

  fortified <- fortify(fit, surv.connect = TRUE)
  expected = data.frame(time = c(0, 1, 2, 3, 4),
                        n.risk = c(8, 8, 6, 4, 2),
                        n.event = c(1, 1, 1, 1, 1),
                        n.censor = c(0, 1, 1, 1, 1),
                        surv = c(1, 0.8824969026, 0.7470175003, 0.5817778142, 0.3528660815),
                        std.err = c(0, 0.1250000000, 0.2083333333, 0.3254270698, 0.5965758776),
                        upper = c(1, 1, 1, 1, 1),
                        lower = c(1, 0.6907374403, 0.4965890298, 0.3074348749, 0.1095982468),
                        cumhaz = c(0, 0.1250000000, 0.2916666667, 0.5416666667,1.0416666667))
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

test_that('fortify.aareg works for lung', {
  fit <- aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1)
  fortified <- fortify(fit)
  expected <- apply(fit$coefficient, 2, cumsum)
  rownames(expected) <- NULL
  expected <- as.data.frame(expected)

  # compare cumulated last row
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(dim(fortified), c(139, 5))
  expect_equal(colnames(fortified), c("time", "Intercept", "age", "sex", "ph.ecog"))
  expect_equal(as.numeric(fortified[-1][nrow(fortified), ]),
               as.numeric(as.data.frame(expected)[nrow(expected), ]))
  expect_equal(as.numeric(fortified$time), c(0, unique(fit$time)))

  fortified <- fortify(fit, surv.connect = FALSE)
  # compare cumulated last row
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(dim(fortified), c(138, 5))
  expect_equal(colnames(fortified), c("time", "Intercept", "age", "sex", "ph.ecog"))
  expect_equal(as.numeric(fortified[nrow(fortified), ][-1]),
               as.numeric(as.data.frame(expected)[nrow(expected), ]))
  expect_equal(as.numeric(fortified$time), unique(fit$time))

  fortified <- fortify(fit, melt = TRUE)
  # compare cumulated last row
  expect_equal(is.data.frame(fortified), TRUE)
  expect_equal(dim(fortified), c(660, 7))
  expect_equal(colnames(fortified), c("time", "variable", "coef", "se", "value", "upper", "lower"))
  expect_equal(fortified$upper - fortified$value, fortified$se * 1.96)
  expect_equal(fortified$value - fortified$lower, fortified$se * 1.96)
  expect_equal(as.numeric(fortified[nrow(fortified), ][c(-1, -2)]),
               c(-4.50000000000, 4.5453070233, -3.2842219995, 5.624579766, -12.193023765192))
})
