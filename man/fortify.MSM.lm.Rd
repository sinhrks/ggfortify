% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify_MSwM.R
\name{fortify.MSM.lm}
\alias{fortify.MSM.lm}
\title{Convert \code{MSwM::MSM.lm} to \code{data.frame}}
\usage{
\method{fortify}{MSM.lm}(model, data = NULL, melt = FALSE, ...)
}
\arguments{
\item{model}{\code{MSwM::MSM.lm} instance}

\item{data}{original dataset, if needed}

\item{melt}{Logical flag indicating whether to melt each models}

\item{...}{other arguments passed to methods}
}
\value{
data.frame
}
\description{
Convert \code{MSwM::MSM.lm} to \code{data.frame}
}
\examples{
\dontrun{
library(MSwM)
d <- data.frame(Data = c(rnorm(50, mean = -10), rnorm(50, mean = 10)),
                exog = cos(seq(-pi/2, pi/2, length.out = 100)))
d.mswm <- MSwM::msmFit(lm(Data ~.-1, data = d), k=2, sw=rep(TRUE, 2),
                       control = list(parallelization = FALSE))
fortify(d.mswm)
}
}
