#' Convert \code{MSwM::MSM.lm} to data.frame.
#' 
#' @param data \code{MSwM::MSM.lm} instance
#' @param melt Logical flag indicating whether to melt each models
#' @return data.frame
#' @examples
#' library(MSwM)
#' d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
#'                 exog = cos(seq(-pi/2, pi/2, length.out = 100)))
#' d.mswm <- MSwM::msmFit(lm(Data ~.-1, data = d), k=2, sw=rep(TRUE, 2),
#'                        control = list(parallelization = FALSE))
#' ggplot2::fortify(d.mswm)
#' @export
fortify.MSM.lm <- function(data, melt = FALSE) {
  probable <- apply(data@Fit@smoProb[-1, ], 1, which.max)
  idx <- names(data@model$fitted.values)  
  
  if (melt) {
    models <- seq(1, data@k)
    .model <- function(k) {
      md <- cbind(data.frame(Index = idx),
                  data@model$model,
                  data.frame(Fitted = data@model$fitted.values,
                             Residuals = data@model$residuals,
                             FiltProb = data@Fit@filtProb[, k],
                             SmoProb = data@Fit@smoProb[-1, k]))
      md$Index <- factor(md$Index, levels = idx)
      md$Model <- rep(k, nrow(md))
      md$ProbableModel <- probable
      md
    }
    d <- dplyr::rbind_all(lapply(models, .model))
    d$Model <- as.factor(d$Model)
  } else {
    d <- cbind(data.frame(Index = idx),
               data@model$model,
               data.frame(Fitted = data@model$fitted.values,
                          Residuals = data@model$residuals,
                          FiltProb = data@Fit@filtProb,
                          SmoProb = data@Fit@smoProb[-1, ],
                          ProbableModel = probable))
  }
  dplyr::tbl_df(d)
}

#' Autoplot  \code{MSwM::MSM.lm}.
#' 
#' @param data  \code{MSwM::MSM.lm} instance
#' @param prob.colour Line colour for probabilities
#' @param prob.linetype Line type for probabilities
#' @param ... Keywords passed to autoplot.ts
#' @return ggplot
#' @examples
#' library(MSwM)
#' d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
#'                 exog = cos(seq(-pi/2, pi/2, length.out = 100)))
#' d.mswm <- MSwM::msmFit(lm(Data ~.-1, data = d), k=2, sw=rep(TRUE, 2),
#'                        control = list(parallelization = FALSE))
#' ggplot2::autoplot(d.mswm)
#' @export
autoplot.MSM.lm <- function(data,
                         prob.colour = '#FF0000', prob.linetype = 'dashed',
                         ...) {
  plot.data <- ggplot2::fortify(data, melt = TRUE)
  
  y = 'SmoProb'
  p <- autoplot.ts(plot.data, columns = y, group = 'Model', ...)
  p <- p + ggplot2::geom_linerange(mapping = ggplot2::aes_string(ymax = 'FiltProb'), ymin = 0) + 
    ggplot2::facet_wrap(~Model, ncol = 1) +
    ggplot2::scale_y_continuous(name = 'Smoothed Probabilities', labels = scales::percent) 
    
  p
}