#' Convert \code{survival::survfit} to \code{data.frame}
#'
#' @param model \code{survival::survfit} instance
#' @param surv.connect logical frag indicates whether connects survival curve to the origin
#' @param fun an arbitrary function defining a transformation of the survival curve
#' @inheritParams fortify_base
#' @return data.frame
#' @aliases fortify.survfit.cox
#' @examples
#' library(survival)
#' fortify(survfit(Surv(time, status) ~ sex, data = lung))
#' fortify(survfit(Surv(time, status) ~ 1, data = lung))
#' fortify(survfit(coxph(Surv(time, status) ~ sex, data = lung)))
#' fortify(survfit(coxph(Surv(time, status) ~ 1, data = lung)))
#' @export
fortify.survfit <- function(model, data = NULL, surv.connect = FALSE,
                            fun = NULL, ...) {
  d <- data.frame(time = model$time,
                  n.risk = model$n.risk,
                  n.event = model$n.event,
                  n.censor = model$n.censor,
                  surv = model$surv,
                  std.err = model$std.err,
                  upper = model$upper,
                  lower = model$lower)

  if (is(model, 'survfit.cox')) {
    d <- cbind_wraps(d, data.frame(cumhaz = model$cumhaz))
  } else if (is(model, 'survfit')) {
    if ('strata' %in% names(model)) {
      groupIDs <- gsub(".*=", '', names(model$strata))
      groupIDs <- factor(rep(groupIDs, model$strata), levels = groupIDs)
      d <- cbind_wraps(d, data.frame(strata = groupIDs))
    }
  } else {
    stop(paste0('Unsupported class for fortify.survfit: ', class(model)))
  }

  # connect to the origin for plotting
  if (surv.connect) {
    base <- d[1, ]
    # cumhaz is for survfit.cox cases
    base[intersect(c('time', 'n.censor', 'std.err', 'cumhaz'), colnames(base))] <- 0
    base[c('surv', 'upper', 'lower')] <- 1.0
    if ('strata' %in% colnames(d)) {
      strata <- levels(d$strata)
      base <- as.data.frame(sapply(base, rep.int, times = length(strata)))
      base$strata <- strata
      base$strata <- factor(base$strata, levels = base$strata)
    }
    d <- rbind(base, d)
  }

  if (!is.null(fun)) {
    if (is.character(fun)) {
      fun <- switch(fun, log = function(x) log(x),
                     event = function(x) 1 - x,
                     cumhaz = function(x) -log(x),
                     cloglog = function(x) log(-log(x)),
                     pct = function(x) x * 100,
                     logpct = function(x) 100 * x,
                     identity = function(x) x,
                     stop("Unrecognized function argument"))
    }
    else if (!is.function(fun)) {
      stop("Invalid 'fun' argument")
    }
    d$surv <- fun(d$surv)
    d$upper <- fun(d$upper)
    d$lower <- fun(d$lower)
  }
  post_fortify(d)
}

#' Autoplot \code{survival::survfit}
#'
#' @param object \code{survival::survfit} instance
#' @param fun an arbitrary function defining a transformation of the survival curve
#' @param surv.geom geometric string for survival curve. 'step' or line'
#' @param surv.colour line colour for survival curve
#' @param surv.size point size for survival curve
#' @param surv.linetype line type for survival curve
#' @param surv.alpha alpha for survival curve
#' @param surv.fill fill colour survival curve
#' @param surv.shape point shape survival curve
#' @inheritParams fortify.survfit
#' @inheritParams plot_confint
#' @param censor Logical flag indicating whether to plot censors
#' @param censor.colour colour for censors
#' @param censor.size size for censors
#' @param censor.alpha alpha for censors
#' @param censor.shape shape for censors
#' @inheritParams apply_facets
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @aliases autoplot.survfit.cox
#' @examples
#' library(survival)
#' autoplot(survfit(Surv(time, status) ~ sex, data = lung))
#' autoplot(survfit(Surv(time, status) ~ sex, data = lung), facets = TRUE)
#' autoplot(survfit(Surv(time, status) ~ 1, data = lung))
#' autoplot(survfit(Surv(time, status) ~ sex, data=lung), conf.int = FALSE, censor = FALSE)
#' autoplot(survfit(coxph(Surv(time, status) ~ sex, data = lung)))
#' @importFrom scales percent
#' @export
autoplot.survfit <- function(object, fun = NULL,
                             surv.geom = 'step',
                             surv.colour = NULL, surv.size = NULL, surv.linetype = NULL,
                             surv.alpha = NULL, surv.fill = NULL, surv.shape = NULL,
                             surv.connect = TRUE,
                             conf.int = TRUE,
                             conf.int.colour = '#0000FF', conf.int.linetype = 'none',
                             conf.int.fill = '#000000', conf.int.alpha = 0.3,
                             censor = TRUE, censor.colour = NULL, censor.size = 3,
                             censor.alpha = NULL, censor.shape = '+',
                             facets = FALSE, nrow = NULL, ncol = 1, scales = 'free_y',
                             xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                             main = NULL, xlab = NULL, ylab = NULL, asp = NULL,
                             ...) {

  if (is_derived_from(object, 'aareg')) {
    # for autoplot.aareg, object must be a data.frame
    plot.data <- object
    mapping <- aes_string(x = 'time', y = 'value')
    facets_formula <- ~ variable
    if (is.null(surv.colour)) {
      surv.colour <- 'variable'
    }
    # use default
    scale_labels <- ggplot2::waiver()
  } else {
    plot.data <- fortify(object, surv.connect = surv.connect, fun = fun)
    mapping <- aes_string(x = 'time', y = 'surv')
    if ('strata' %in% colnames(plot.data)) {
      facets_formula <- ~ strata
      if (is.null(surv.colour)) {
        surv.colour <- 'strata'
      }
    } else {
      facets_formula <- NULL
    }
    if (is.null(fun) || fun %in% c('identity', 'event')) {
      scale_labels <- scales::percent
    } else {
      scale_labels <- ggplot2::waiver()
    }
  }

  if (missing(conf.int.fill) & !is.null(surv.colour)) {
    conf.int.fill <- surv.colour
  }

  geomfunc <- get_geom_function(surv.geom, allowed = c('step', 'line', 'point'))

  p <- ggplot(data = plot.data, mapping = mapping) +
    scale_y_continuous(labels = scale_labels)
  p <- p + geom_factory(geomfunc, plot.data,
                        colour = surv.colour, size = surv.size, linetype = surv.linetype,
                        alpha = surv.alpha, fill = surv.fill, shape = surv.shape)
  if (surv.geom == 'step') {
    conf.int.geom <- 'step'
  } else {
    conf.int.geom <- 'line'
  }
  p <- plot_confint(p, data = plot.data,
                    conf.int = conf.int, conf.int.geom = conf.int.geom,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  if (censor & 'n.censor' %in% colnames(plot.data)) {
    p <- p + geom_factory(geom_point, plot.data[plot.data$n.censor > 0, ],
                          colour = censor.colour, size = censor.size,
                          alpha = censor.alpha, shape = censor.shape)
  }
  if (facets) {
    p <- apply_facets(p, facets_formula, nrow = nrow, ncol = ncol, scales = scales)
  }
  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}

#' Convert \code{survival::aareg} to \code{data.frame}
#'
#' @param model \code{survival::aareg} instance
#' @param maxtime truncate the input to the model at time "maxtime"
#' @inheritParams fortify.survfit
#' @param melt Logical flag indicating whether to melt each timeseries as variable
#' @return data.frame
#' @examples
#' library(survival)
#' fortify(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1))
#' fortify(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1), melt = TRUE)
#' @export
fortify.aareg <- function(model, data = NULL,
                          maxtime = NULL,
                          surv.connect = TRUE,
                          melt = FALSE, ...) {

  if (is.null(maxtime)) {
    keep <- 1:length(model$time)
  } else {
    keep <- 1:sum(model$time <= maxtime)
  }

  if (is.matrix(model$coefficient) && ncol(model$coefficient) > 1) {
    coefs <- model$coefficient[keep, ]
  } else {
    coefs <- model$coefficient[keep]
  }
  rownames(coefs) <- NULL
  coefs <- as.data.frame(coefs)
  cols <- colnames(coefs)

  if (melt) {
    d <- cbind(data.frame(time = model$time[keep]), coefs)
    if (surv.connect) {
      d <- rbind(0, d)
    }
    d <- tidyr::gather_(d, 'variable', 'coef', cols)
    d <- d %>%
      dplyr::group_by_('variable') %>%
      dplyr::mutate_('se' = 'sqrt(cumsum(coef ^ 2))',
                     'value' = 'cumsum(coef)',
                     'upper' = 'value + se * 1.96',
                     'lower' = 'value - se * 1.96')
  } else {
    d <- cbind_wraps(data.frame(time = model$time[keep]),
                     apply(coefs, 2, cumsum))
    indexer <- 1 + length(d$time) - rev(match(unique(rev(d$time)), rev(d$time)))
    d <- d[indexer, ]
    if (surv.connect) {
      d <- rbind(0, d)
    }
  }
  post_fortify(d, klass = model)
}

#' Autoplot \code{survival::aareg}
#'
#' @param object \code{survival::aareg} instance
#' @param maxtime truncate the input to the model at time "maxtime"
#' @inheritParams autoplot.survfit
#' @param ... other arguments passed to \code{autoplot.survfit}
#' @return ggplot
#' @examples
#' library(survival)
#' autoplot(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung, nmin = 1))
#' @export
autoplot.aareg <- function (object, maxtime = NULL,
                            surv.connect = TRUE,
                            facets = TRUE, ncol = NULL,
                            xlab = '', ylab = '',
                            ...) {

  plot.data <- fortify(object, maxtime = maxtime,
                       surv.connect = surv.connect, melt = TRUE)
  autoplot.survfit(plot.data, facets = facets, ncol = ncol,
                   xlab = '', ylab = '', ...)
}
