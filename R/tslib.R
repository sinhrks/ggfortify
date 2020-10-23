#' Convert \code{ts} index to \code{Date} \code{vector}
#'
#' @param data \code{ts} instance
#' @param is.tsp Logical frag whether data is \code{tsp} itself or not
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @return vector
#' @examples
#' ggfortify:::get.dtindex(AirPassengers)
#' ggfortify:::get.dtindex(UKgas)
#' ggfortify:::get.dtindex(Nile, is.date = FALSE)
#' ggfortify:::get.dtindex(Nile, is.date = TRUE)
get.dtindex <- function(data, is.tsp = FALSE, is.date = NULL) {
  if (is.tsp) {
    tsp <- data
  } else {
    tsp <- attr(data, which='tsp')
  }
  if (is.null(tsp)) {
    stop('Failed to convert ts object index to date')
  }
  dtindex <- seq(from = tsp[1], to = tsp[2], by= 1 / tsp[3])
  if ( (is.null(is.date) && any(tsp[3] == c(4, 12))) ||
       (!is.null(is.date) && is.date)) {
    dtindex <- zoo::as.Date.yearmon(dtindex)
  }
  dtindex
}

#' Get \code{Date} \code{vector} continue to \code{ts} index
#'
#' @param data \code{ts} instance
#' @param length A number to continue
#' @param is.tsp Logical frag whether data is \code{tsp} itself or not
#' @param is.date Logical frag indicates whether the \code{stats::ts} is date or not.
#' If not provided, regard the input as date when the frequency is 4 or 12.
#' @return vector
#' @examples
#' ggfortify:::get.dtindex.continuous(AirPassengers, length = 10)
#' ggfortify:::get.dtindex.continuous(UKgas, length = 10)
get.dtindex.continuous <- function(data, length, is.tsp = FALSE, is.date = NULL) {
  if (is.tsp) {
    tsp <- data
  } else {
    tsp <- attr(data, which='tsp')
  }
  if (is.null(tsp)) {
    stop('Failed to convert ts object index to date')
  }
  dt.by <- 1 / tsp[3]
  dtindex <- seq(from = tsp[2] + dt.by, length = length, by = dt.by)
  if ( (is.null(is.date) && any(tsp[3] == c(4, 12))) ||
       (!is.null(is.date) && is.date)) {
    dtindex <- zoo::as.Date.yearmon(dtindex)
  }
  dtindex
}

#' Check if Validates number of \code{ts} variates
#'
#' @param data \code{ts} instance
#' @param raise Logical flag whether raise an error
#' @return logical
#' @examples
#' ggfortify:::is.univariate(AirPassengers)
is.univariate <- function(data, raise = TRUE) {
  if (ncol(as.matrix(data)) > 1) {
    if (raise) {
      stop("data must be univariate time series")
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Rbind original and predicted time-series-like instances as fortified \code{data.frame}
#'
#' @param data Predicted/forecasted \code{ts} instance
#' @param original Original \code{ts} instance
#' @param ts.connect Logical frag indicates whether connects original time-series and predicted values
#' @param index.name Specify column name for time series index
#' @param data.name Specify column name for univariate time series data. Ignored in multivariate time series.
#' @return data.frame
#' @examples
#' predicted <- predict(stats::HoltWinters(UKgas), n.ahead = 5, prediction.interval = TRUE)
#' rbind_ts(predicted, UKgas, ts.connect = TRUE)
#' @export
rbind_ts <- function(data, original, ts.connect = TRUE,
                     index.name = 'Index', data.name = 'Data') {
  if (!is.data.frame(data)) {
    data <- ggplot2::fortify(data, index.name = index.name,
                             data.name = 'Point Forecast')
  }

  dnames <- names(data)
  dnames <- dnames[dnames != index.name]

  if (!is.data.frame(original)) {
    original <- ggplot2::fortify(original, index.name = index.name,
                                 data.name = data.name)
  }
  n <- nrow(original)
  rownames(data) <- NULL
  rownames(original) <- NULL

  d <- dplyr::bind_rows(original, data)
  if (ts.connect) {
    # Use fnames not to overwrite Index
    d[n, dnames] <- d[n, data.name]
  }
  post_fortify(d)
}


#' Calculate confidence interval for \code{stats::acf}
#'
#' @param x \code{stats::acf} instance
#' @param ci Float value for confidence interval
#' @param ci.type "white" or "ma"
#' @return vector
#' @examples
#' air.acf <- acf(AirPassengers, plot = FALSE)
#' ggfortify:::confint.acf(air.acf)
#' ggfortify:::confint.acf(air.acf, ci.type = 'ma')
confint.acf <- function (x, ci = 0.95, ci.type = "white") {
  if ( (nser <- ncol(x$lag)) < 1L)
    stop("x$lag must have at least 1 column")
  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"

  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }
  clim0 <- if (with.ci)
    stats::qnorm( (1 + ci) / 2) / sqrt(x$n.used)
  else c(0, 0)

  Npgs <- 1L
  nr <- nser

  if (nser > 1L) {
    Npgs <- nser
    nr <- ceiling(nser / Npgs)
  }

  for (I in 1L:Npgs) for (J in 1L:Npgs) {
    iind <- (I - 1) * nr + 1L:nr
    jind <- (J - 1) * nr + 1L:nr

    for (i in iind) {
      for (j in jind) {
        if (!(max(i, j) > nser)) {
          clim <- if (with.ci.ma && i == j)
            clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j] ^ 2)))
          else clim0

          if (with.ci && ci.type == "white") {
            clim
          } else if (with.ci.ma && i == j) {
            clim <- clim[-length(clim)]
          }
        }
      }
    }
  }
  if (with.ci.ma && length(clim) < length(x$lag)) {
    clim <- c(NA, clim)
  }
  return(clim)
}

#' Calculate fitted values for \code{stats::ar}
#'
#' @param object \code{stats::ar} instance
#' @param ... other keywords
#' @return ts An time series of the one-step forecasts
#' @examples
#' fitted(ar(WWWusage))
#' @export
fitted.ar <- function(object, ...) {
  x <- forecast::getResponse(object)
  return(x - stats::residuals(object))
}

#' Calculate residuals for \code{stats::ar}
#'
#' @param object \code{stats::ar} instance
#' @param ... other keywords
#' @return ts Residuals extracted from the object object.
#' @examples
#' residuals(ar(WWWusage))
#' @export
residuals.ar <- function(object, ...) {
  return(object$resid)
}

#' Plots a cumulative periodogram
#'
#' @param ts \code{stats::ts} instance
#' @param taper Proportion tapered in forming the periodogram
#' @param colour Line colour
#' @param linetype Line type
#' @inheritParams plot_confint
#' @return ggplot
#' @examples
#' ggcpgram(AirPassengers)
#' @export
ggcpgram <- function (ts, taper = 0.1,
                      colour = '#000000', linetype = 'solid',
                      conf.int = TRUE,
                      conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                      conf.int.fill = NULL, conf.int.alpha = 0.3) {
  is.univariate(ts)

  x <- as.vector(ts)
  x <- x[!is.na(x)]
  x <- stats::spec.taper(scale(x, TRUE, FALSE), p = taper)
  y <- Mod(stats::fft(x)) ^ 2 / length(x)
  y[1L] <- 0
  n <- length(x)
  x <- (0:(n / 2)) * stats::frequency(ts) / n
  if (length(x) %% 2 == 0) {
    n <- length(x) - 1
    y <- y[1L:n]
    x <- x[1L:n]
  }
  else y <- y[seq_along(x)]
  xm <- stats::frequency(ts) / 2
  mp <- length(x) - 1
  crit <- 1.358 / (sqrt(mp) + 0.12 + 0.11 / sqrt(mp))

  d <- data.frame(x = x,
                  y = cumsum(y) / sum(y),
                  upper = 1 / xm * x + crit,
                  lower = 1 / xm * x - crit)
  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes_string(x = 'x', y = 'y')) +
    geom_line(colour = colour, linetype = linetype) +
    ggplot2::scale_x_continuous(name = '', limits = c(0, xm)) +
    ggplot2::scale_y_continuous(name = '', limits = c(0, 1))

  p <- plot_confint(p = p, data = d, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  p
}

#' Plots time-series diagnostics
#'
#' @param object A fitted time-series model
#' @param gof.lag The maximum number of lags for a Portmanteau goodness-of-fit test
#' @inheritParams plot_confint
#' @param ad.colour Line colour for additional lines
#' @param ad.linetype Line type for additional lines
#' @param ad.size Fill colour for additional lines
#' @param nrow Number of facet/subplot rows
#' @param ncol Number of facet/subplot columns
#' @param ... other keywords
#' @return ggplot
#' @examples
#' ggtsdiag(arima(AirPassengers))
#' @export
ggtsdiag <- function(object, gof.lag = 10,
                     conf.int = TRUE,
                     conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                     conf.int.fill = NULL, conf.int.alpha = 0.3,
                     ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                     nrow = NULL, ncol = 1, ...) {
  rs <- stats::residuals(object)
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }

  stdres <- rs / sqrt(object$sigma2)
  p.std <- ggplot2::autoplot(stdres) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    ggplot2::ggtitle('Standardized Residuals')

  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot.acf(acfobj, conf.int = conf.int,
                        conf.int.colour = conf.int.colour,
                        conf.int.linetype = conf.int.linetype,
                        conf.int.fill = conf.int.fill,
                        conf.int.alpha = conf.int.alpha)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')

  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`')) +
    ggplot2::scale_y_continuous(limits=c(0, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')
  p.lb <- plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                       conf.int.colour = conf.int.colour,
                       conf.int.linetype = conf.int.linetype,
                       conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)

  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}

#' Plot seasonal subseries of time series, generalization of \code{stats::monthplot}
#'
#' @param data \code{stats::ts} instance
#' @param freq Length of frequency. If not provided, use time-series frequency
#' @param nrow Number of plot rows
#' @param ncol Number of plot columns
#' @inheritParams plot_confint
#' @param conf.int.value Coverage probability for confidence interval
#' @param facet.labeller A vector used as facet labels
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @examples
#' ggfreqplot(AirPassengers)
#' ggfreqplot(AirPassengers, freq = 4)
#' ggfreqplot(AirPassengers, conf.int = TRUE)
#' @export
ggfreqplot <- function(data, freq = NULL,
                       nrow = NULL, ncol = NULL,
                       conf.int = FALSE,
                       conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                       conf.int.fill = NULL, conf.int.alpha = 0.3,
                       conf.int.value = 0.95, facet.labeller = NULL,
                       ...) {
  is.univariate(data)

  if (is.null(freq)) {
    freq <- stats::frequency(data)
  }

  if (is.null(nrow) && is.null(ncol)) {
    nrow <- ceiling(sqrt(freq))
  }

  d <- ggplot2::fortify(data)
  if (is.null(facet.labeller)) {
    freqs <- 1:freq
  } else if (length(facet.labeller)  == freq) {
    freqs <- factor(facet.labeller, levels = facet.labeller)
  } else {
    stop('facet.labeller must be a vector which has the same length as freq')
  }
  freqd <- data.frame(Frequency = rep(freqs, length.out = length(data)))
  d <- cbind(d, freqd)

  summarised <- dplyr::group_by(d, Frequency) %>%
    dplyr::summarise(m = mean(Data), s = sd(Data))

  p <- (1 - conf.int.value) / 2
  summarised$lower <- stats::qnorm(p, mean = summarised$m, sd = summarised$s)
  summarised$upper <- stats::qnorm(1 - p, mean = summarised$m, sd = summarised$s)

  d <- dplyr::left_join(d, summarised, by = 'Frequency')

  p <- autoplot.ts(d, columns = 'Data', ...)
  p <- p + ggplot2::geom_line(mapping = ggplot2::aes_string(y = 'm'),
                       colour = conf.int.colour) +
    ggplot2::facet_wrap(~Frequency, nrow = nrow, ncol = ncol)
  p <- plot_confint(p = p, data = d, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  p
}
