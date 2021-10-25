context('test survival')

test_that('fortify.survfit works for lung', {
  skip_if_not_installed("survival")
  library(survival)
  d.survfit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
  fortified <- ggplot2::fortify(d.survfit)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(206, 9))

  p <- ggplot2::autoplot(d.survfit)
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(d.survfit, surv.geom = 'line')
  expect_true(is(p, 'ggplot'))

  fortified2 <- ggplot2::fortify(d.survfit, fun = 'event')
  expect_true(is.data.frame(fortified))
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(206, 9))
  expect_equal(fortified$surv, 1 - fortified2$surv)
  expect_equal(fortified$upper, 1 - fortified2$upper)
  expect_equal(fortified$lower, 1 - fortified2$lower)

  p <- ggplot2::autoplot(d.survfit, fun = 'event')
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(d.survfit, fun = function(x) { 1-x })
  expect_true(is(p, 'ggplot'))

  fortified <- ggplot2::fortify(d.survfit, surv.connect = TRUE)
  expect_true(is.data.frame(fortified))
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(208, 9))
  expected <- data.frame(time = c(0, 0), n.risk = c(138, 138), n.event = c(0, 0), n.censor = c(0, 0),
                         surv = c(1, 1), std.err = c(0, 0), upper = c(1, 1), lower = c(1, 1),
                         strata = factor(c('1', '2')))
  expect_equal(fortified[1:2, ], expected)

  p <- ggplot2::autoplot(d.survfit, surv.connect = TRUE)
  expect_true(is(p, 'ggplot'))

  p <- ggplot2::autoplot(d.survfit, surv.connect = FALSE)
  expect_true(is(p, 'ggplot'))

  d.survfit <- survfit(Surv(time, status) ~ 1, data = lung)
  fortified <- ggplot2::fortify(d.survfit)

  expect_true(is.data.frame(fortified))
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(186, 8))

  p <- ggplot2::autoplot(d.survfit)
  expect_true(is(p, 'ggplot'))

  fortified <- ggplot2::fortify(d.survfit, surv.connect = TRUE)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)
  expect_equal(dim(fortified), c(187, 8))
  expected <- data.frame(time = 0, n.risk = 228, n.event = 0, n.censor = 0,
                         surv = 1, std.err = 0, upper = 1, lower = 1)
  expect_equal(fortified[1, ], expected)

  p <- ggplot2::autoplot(d.survfit)
  expect_true(is(p, 'ggplot'))
})

test_that('autoplot retains order of alphabetically unordered factor levels', {
  skip_if_not_installed("survival")
  library(survival)
  livingStatus <- sample(0:1, 20, replace = TRUE)
  followupTime <- rpois(20, 300)
  samplesGroups <- factor(sample(c("Low", "High"), 20, replace = TRUE), levels = c("Low", "High"))
  survivalData <- Surv(followupTime, livingStatus)
  survivalFit <- survfit(survivalData ~ samplesGroups)
  plotElements <- ggplot2::autoplot(survivalFit, conf.int = FALSE, ylim = c(0, 1))
  expect_equal(levels(plotElements[["data"]][, "strata"]), c("Low", "High"))
})

test_that('fortify.survfit works for simple data', {
  skip_if_not_installed("survival")
  library(survival)
  tdata <- data.frame(time = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
                      status = rep(c(1, 0, 2), 4),
                      n = c(12, 3, 2, 6, 2, 4, 2, 0, 2, 3, 3, 5))
  fit  <- survfit(Surv(time, time, status, type='interval') ~1, data = tdata, weight = n)
  fortified <- fortify(fit)
  expected <- data.frame(time = c(1, 2, 3, 4),
                        n.risk = c(44.000000000, 20.652979445, 9.318098786, 6.634779353),
                        n.event = c(20.347020555, 9.334880659, 2.683319433, 3.634779353),
                        n.censor = c(3, 2, 0, 3),
                        surv = c(0.53756771467, 0.29459403255, 0.20976021500, 0.09484575319),
                        std.err = c(0.1398238147, 0.2438966932, 0.3207623423, 0.5343228432),
                        upper = c(0.7070521134, 0.4751483078, 0.3933295002, 0.2702927012),
                        lower = c(0.40870968683, 0.18264959088, 0.11186383878, 0.03328139035))
  expect_equal(fortified, expected, tolerance = 0.5, scale = 1)

  fortified <- fortify(fit, surv.connect = TRUE)
  expected <- data.frame(time = c(0, 1, 2, 3, 4),
                        n.risk = c(44.000000000, 44.000000000, 20.652979445, 9.318098786, 6.634779353),
                        n.event = c(0, 20.347020555, 9.334880659, 2.683319433, 3.634779353),
                        n.censor = c(0, 3, 2, 0, 3),
                        surv = c(1.0, 0.53756771467, 0.29459403255, 0.20976021500, 0.09484575319),
                        std.err = c(0, 0.1398238147, 0.2438966932, 0.3207623423, 0.5343228432),
                        upper = c(1.0, 0.7070521134, 0.4751483078, 0.3933295002, 0.2702927012),
                        lower = c(1.0, 0.40870968683, 0.18264959088, 0.11186383878, 0.03328139035))
  expect_equal(fortified, expected, tolerance = 0.5, scale = 1)

  p <- ggplot2::autoplot(fit)
  expect_true(is(p, 'ggplot'))

  tdata <- data.frame(time = c(1, 1, 2, 2, 3, 3, 4, 4),
                      status = rep(c(1, 2), 4),
                      n = c(24, 3, 20, 4, 18, 2, 15, 3))

  fit <- survfit(coxph(Surv(time, status) ~ 1, data = tdata))
  fortified <- fortify(fit)
  expected <- data.frame(time = c(1, 2, 3, 4),
                         n.risk = c(8, 6, 4, 2),
                         n.event = c(1, 1, 1, 1),
                         n.censor = c(1, 1, 1, 1),
                         surv = c(0.8824969026, 0.7470175003, 0.5817778142, 0.3528660815),
                         std.err = c(0.1250000000, 0.2083333333, 0.3254270698, 0.5965758776),
                         upper = c(1, 1, 1, 1),
                         lower = c(0.6907374403, 0.4965890298, 0.3074348749, 0.1095982468))
  expect_equal(fortified, expected)

  fortified <- fortify(fit, surv.connect = TRUE)
  expected <- data.frame(time = c(0, 1, 2, 3, 4),
                         n.risk = c(8, 8, 6, 4, 2),
                         n.event = c(0, 1, 1, 1, 1),
                         n.censor = c(0, 1, 1, 1, 1),
                         surv = c(1, 0.8824969026, 0.7470175003, 0.5817778142, 0.3528660815),
                         std.err = c(0, 0.1250000000, 0.2083333333, 0.3254270698, 0.5965758776),
                         upper = c(1, 1, 1, 1, 1),
                         lower = c(1, 0.6907374403, 0.4965890298, 0.3074348749, 0.1095982468))
  expect_equal(fortified, expected)

  p <- ggplot2::autoplot(fit)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.survfit works for simple multistate data', {
  skip_if_not_installed("survival")
  library(survival)
  tdata <- data.frame(time = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
                      status = c(1, 0, 2, 1, 1, 2, 0, 0, 2, 1, 2, 2))
  fit <- survfit(Surv(time, status, type='mstate') ~1, data = tdata)
  fortified <- fortify(fit)
  expect_equal(names(fortified),
               c("time", "n.risk", "n.event", "n.censor", "pstate",
                 "std.err", "upper", "lower", "event"))
  expect_equal(dim(fortified), c(12, 9))

  fortified <- fortify(fit, surv.connect = T)
  expect_equal(names(fortified),
               c("time", "n.risk", "n.event", "n.censor", "pstate",
                 "std.err", "upper", "lower", "event"))
  expect_equal(dim(fortified), c(15, 9))

  p <- ggplot2::autoplot(fit)
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.survfit.cox works for lung', {
  skip_if_not_installed("survival")
  library(survival)
  d.coxph <- coxph(Surv(time, status) ~ sex, data = lung)
  fortified <- ggplot2::fortify(survfit(d.coxph))

  expect_true(is.data.frame(fortified))
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower')
  expect_equal(names(fortified), expected_names)

  p <- ggplot2::autoplot(survfit(d.coxph))
  expect_true(is(p, 'ggplot'))
})

test_that('fortify.aareg works for lung', {
  skip_if_not_installed("survival")
  library(survival)
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

  p <- ggplot2::autoplot(fit)
  expect_true(is(p, 'ggplot'))

  fortified <- fortify(fit, surv.connect = FALSE)
  # compare cumulated last row
  expect_true(is.data.frame(fortified))
  expect_equal(dim(fortified), c(138, 5))
  expect_equal(colnames(fortified), c("time", "Intercept", "age", "sex", "ph.ecog"))
  expect_equal(as.numeric(fortified[nrow(fortified), ][-1]),
               as.numeric(as.data.frame(expected)[nrow(expected), ]))
  expect_equal(as.numeric(fortified$time), unique(fit$time))

  fortified <- fortify(fit, melt = TRUE)
  # compare cumulated last row
  expect_true(is.data.frame(fortified))
  expect_equal(dim(fortified), c(660, 7))
  expect_equal(colnames(fortified), c("time", "variable", "coef", "se", "value", "upper", "lower"))
  expect_equal(fortified$upper - fortified$value, fortified$se * 1.96)
  expect_equal(fortified$value - fortified$lower, fortified$se * 1.96)
  expect_equal(as.numeric(fortified[nrow(fortified), ][c(-1, -2)]),
               c(-4.50000000000, 4.5453070233, -3.2842219995, 5.624579766, -12.193023765192))
})

test_that('autoplot.aareg works for lung', {
  skip_if_not_installed("survival")
  library(survival)
  fit <- aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1)
  p <- autoplot(fit)

  # fail on travis
  skip_on_cran()
  skip_on_travis()

  expect_equal(length(p$layers), 2)
  expect_true(is(p$layers[[1]]$geom, 'GeomStep'))
  expect_true(is(p$layers[[2]]$geom, 'GeomConfint'))
  ld1 <- head(ggplot2:::layer_data(p, 1))
  expect_equal(ld1$x, c(0, 5, 11, 11, 11, 12))
  expect_equal(length(ld1$colour), 6)
  ld2 <- head(ggplot2:::layer_data(p, 2))
  expect_equal(ld2$x, c(0, 5, 11, 11, 11, 12))
  expect_equal(length(ld2$fill), 6)
  expect_equal(ld2$alpha, rep(0.3, 6))
})

test_that('fortify.survfit regular expression for renaming strata works with multiple stratification variables', {
  skip_if_not_installed("survival")
  library(survival)
  d.survfit <- survival::survfit(Surv(time, status) ~ sex + ph.ecog, data = lung)
  fortified <- ggplot2::fortify(d.survfit)
  expect_equal(is.data.frame(fortified), TRUE)
  expected_names <- c('time', 'n.risk', 'n.event', 'n.censor', 'surv',
                      'std.err', 'upper', 'lower', 'strata')
  expect_equal(names(fortified), expected_names)
})
