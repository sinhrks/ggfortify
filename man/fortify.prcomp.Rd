% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify_stats.R
\name{fortify.prcomp}
\alias{fortify.prcomp}
\alias{fortify.princomp}
\title{Convert \code{stats::prcomp}, \code{stats::princomp} to \code{data.frame}}
\usage{
\method{fortify}{prcomp}(model, data = NULL, ...)
}
\arguments{
\item{model}{\code{stats::prcomp} or \code{stats::princomp} instance}

\item{data}{original dataset, if needed}

\item{...}{other arguments passed to methods}
}
\value{
data.frame
}
\description{
Convert \code{stats::prcomp}, \code{stats::princomp} to \code{data.frame}
}
\examples{
\dontrun{
fortify(stats::prcomp(iris[-5]))
fortify(stats::prcomp(iris[-5]), data = iris)

fortify(stats::princomp(iris[-5]))
fortify(stats::princomp(iris[-5]), data = iris)
}
}
