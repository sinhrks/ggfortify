#' Convert \code{glmnet::glmnet} to \code{data.frame}
#'
#' @param model \code{glmnet::glmnet} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(glmnet::glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
fortify.glmnet <- function(model, data = NULL, ...) {
  beta <- as.matrix(model$beta)
  ret <- cbind_wraps(as.data.frame(t(beta)),
                     data.frame(Df = model$df, Lambda = model$lambda,
                                dev.ratio = model$dev.ratio))
  post_fortify(ret)
}

#' Autoplot \code{glmnet::glmnet}
#'
#' @param object \code{glmnet::glmnet} instance
#' @param xvar values to be dranw on the X axis. Either "norm" (L1-norm), "lambda" (log-lambda sequence) or "dev" (percent deviance)
#' @param label.n Number of Df labels
#' @inheritParams plot_label
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(glmnet::glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
autoplot.glmnet <- function (object,
                             xvar = c("norm", "lambda", "dev"),
                             label.n = 7,
                             label = TRUE, label.label = 'Df',
                             label.colour = NULL, label.alpha = NULL,
                             label.size = NULL, label.angle = NULL,
                             label.family = NULL, label.fontface = NULL,
                             label.lineheight = NULL,
                             label.hjust = NULL, label.vjust = NULL,
                             xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                             main = NULL, xlab = NULL,
                             ylab = 'Coefficients', asp = NULL,
                             ...) {
  beta <- as.matrix(object$beta)
  xvar <- match.arg(xvar)

  switch(xvar, norm = {
    index <- apply(abs(beta), 2, sum)
    iname <- "L1 Norm"
  }, lambda = {
    index <- log(object$lambda)
    iname <- "Log Lambda"
  }, dev = {
    index <- object$dev.ratio
    iname <- "Fraction Deviance Explained"
  })
  if (is.null(xlab)) {
    xlab <- iname
  }
  plot.data <- ggplot2::fortify(object)
  cols <- rownames(beta)
  plot.data$index <- index
  indexer <- seq(0, max(plot.data$index), length = label.n)
  # take closest values
  indexer <- sapply(indexer, function(x) which.min(abs(plot.data$index - x)))
  label.data <- plot.data[indexer, ]

  # preparing plot data
  plot.data <- tidyr::gather_(plot.data, 'variable', 'value', cols)
  # preparing label data (no need to gather)
  label.data$label_y <- rep(max(plot.data$value), nrow(label.data))

  p <- ggplot2::ggplot(data = plot.data) +
    ggplot2::geom_line(aes_string(x = 'index', y = 'value', colour = 'variable'), ...)

  p <- plot_label(p = p, data = label.data, x = 'index', y = 'label_y',
                  label = label,
                  label.label = label.label, label.colour = label.colour,
                  label.alpha = label.alpha, label.size = label.size,
                  label.angle = label.angle, label.family = label.family,
                  label.fontface = label.fontface, label.lineheight = label.lineheight,
                  label.hjust = label.hjust, label.vjust = label.vjust)

  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}

#' Convert \code{glmnet::cv.glmnet} to \code{data.frame}
#'
#' @param model \code{glmnet::cv.glmnet} instance
#' @inheritParams fortify_base
#' @return data.frame
#' @examples
#' fortify(glmnet::cv.glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
fortify.cv.glmnet <- function(model, data = NULL, ...) {
  d <- data.frame('lambda' = log(model$lambda), 'cvm' = model$cvm,
                  'cvup' = model$cvup, 'cvlo' = model$cvlo,
                  'nz' = model$nz)
  post_fortify(d)
}

#' Autoplot \code{glmnet::cv.glmnet}
#'
#' @param object \code{glmnet::cv.glmnet} instance
#' @param sign.lambda Either plot against log(lambda) (default) or its negative if \code{sign.lambda=-1}.
#' @param label.n Number of Df labels
#' @inheritParams plot_label
#' @inheritParams post_autoplot
#' @param ... other arguments passed to methods
#' @return ggplot
#' @examples
#' autoplot(glmnet::cv.glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
#' @export
autoplot.cv.glmnet <- function (object,
                                sign.lambda = 1,
                                label.n = 12,
                                label = TRUE,
                                label.label = 'nz',
                                label.colour = NULL,
                                label.alpha = NULL,
                                label.size = NULL,
                                label.angle = NULL,
                                label.family = NULL,
                                label.fontface = NULL,
                                label.lineheight = NULL,
                                label.hjust = NULL,
                                label.vjust = NULL,
                                label.repel = FALSE,
                                xlim = c(NA, NA), ylim = c(NA, NA), log = "",
                                main = NULL, xlab = NULL,
                                ylab = NULL, asp = NULL,
                                ...) {

  if (is.null(xlab)) {
    xlab <- 'log(Lambda)'
    if (sign.lambda < 0) {
      xlab <- paste("-", xlab, sep = "")
    }
  }
  if (is.null(ylab)) {
    type_measure <- object$name
    ylab <- type_measure[[names(type_measure)[[1]]]]
  }
  plot.data <- ggplot2::fortify(object)
  plot.data$lambda <- sign.lambda * plot.data$lambda
  plot.data$label <- rep(max(object$cvup), nrow(plot.data))

  p <- ggplot2::ggplot(plot.data) +
    geom_point(aes_string('lambda', 'cvm'), ...)

  p <- p + ggplot2::geom_errorbar(aes_string(x = 'lambda', ymin = 'cvlo', ymax = 'cvup'), ...)

  indexer <- seq(1, nrow(plot.data), length = label.n)
  p <- plot_label(p = p, data = plot.data[indexer, ], x = 'lambda', y = 'label',
                  label = label,
                  label.label = label.label,
                  label.colour = label.colour,
                  label.alpha = label.alpha,
                  label.size = label.size,
                  label.angle = label.angle,
                  label.family = label.family,
                  label.fontface = label.fontface,
                  label.lineheight = label.lineheight,
                  label.hjust = label.hjust,
                  label.vjust = label.vjust,
                  label.repel = label.repel)

  p <- p +
    ggplot2::geom_vline(xintercept = sign.lambda * log(object$lambda.min), linetype = 'dashed') +
    ggplot2::geom_vline(xintercept = sign.lambda * log(object$lambda.1se), linetype = 'dashed')

  p <- post_autoplot(p = p, xlim = xlim, ylim = ylim, log = log,
                     main = main, xlab = xlab, ylab = ylab, asp = asp)
  p
}
