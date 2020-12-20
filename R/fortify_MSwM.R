#' Convert \code{MSwM::MSM.lm} to \code{data.frame}
#'
#' @param model \code{MSwM::MSM.lm} instance
#' @inheritParams fortify_base
#' @param melt Logical flag indicating whether to melt each models
#' @return data.frame
#' @examples
#' library(MSwM)
#' d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
#'                 exog = cos(seq(-pi/2, pi/2, length.out = 100)))
#' d.mswm <- MSwM::msmFit(lm(Data ~.-1, data = d), k=2, sw=rep(TRUE, 2),
#'                        control = list(parallelization = FALSE))
#' fortify(d.mswm)
#' @export
fortify.MSM.lm <- function(model, data = NULL, melt = FALSE, ...) {
  probable <- apply(model@Fit@smoProb[-1, ], 1, which.max)
  idx <- names(model@model$fitted.values)

  if (melt) {
    models <- seq(1, model@k)
    .model <- function(k) {
      md <- cbind(data.frame(Index = idx),
                  model@model$model,
                  data.frame(Fitted = model@model$fitted.values,
                             Residuals = model@model$residuals,
                             FiltProb = model@Fit@filtProb[, k],
                             SmoProb = model@Fit@smoProb[-1, k]))
      md$Index <- factor(md$Index, levels = idx)
      md$Model <- rep(k, nrow(md))
      md$ProbableModel <- probable
      md
    }
    d <- dplyr::bind_rows(lapply(models, .model))
    d$Model <- as.factor(d$Model)
  } else {
    d <- cbind(data.frame(Index = idx),
               model@model$model,
               data.frame(Fitted = model@model$fitted.values,
                          Residuals = model@model$residuals,
                          FiltProb = model@Fit@filtProb,
                          SmoProb = model@Fit@smoProb[-1, ],
                          ProbableModel = probable))
  }
  post_fortify(d)
}

#' Autoplot \code{MSwM::MSM.lm}
#'
#' @param object \code{MSwM::MSM.lm} instance
#' @param prob.colour Line colour for probabilities
#' @param prob.linetype Line type for probabilities
#' @param ... other arguments passed to \code{autoplot.ts}
#' @return ggplot
#' @examples
#' \dontrun{
#' library(MSwM)
#' d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
#'                 exog = cos(seq(-pi/2, pi/2, length.out = 100)))
#' d.mswm <- MSwM::msmFit(lm(Data ~.-1, data = d), k=2, sw=rep(TRUE, 2),
#'                        control = list(parallelization = FALSE))
#' autoplot(d.mswm)
#' }
#' @importFrom scales percent
#' @export
autoplot.MSM.lm <- function(object, prob.colour = '#FF0000',
                            prob.linetype = 'dashed', ...) {
  plot.data <- ggplot2::fortify(object, melt = TRUE)

  y <- 'SmoProb'
  p <- autoplot.ts(plot.data, columns = y, group = 'Model', ...)
  p <- p + ggplot2::geom_linerange(mapping = ggplot2::aes_string(ymax = 'FiltProb'), ymin = 0) +
    ggplot2::facet_wrap(~Model, ncol = 1) +
    ggplot2::scale_y_continuous(name = 'Smoothed Probabilities', labels = scales::percent)

  p
}
