% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tslib.R
\name{ggfreqplot}
\alias{ggfreqplot}
\title{Plot seasonal subseries of time series, generalization of \code{stats::monthplot}}
\usage{
ggfreqplot(
  data,
  freq = NULL,
  nrow = NULL,
  ncol = NULL,
  conf.int = FALSE,
  conf.int.colour = "#0000FF",
  conf.int.linetype = "dashed",
  conf.int.fill = NULL,
  conf.int.alpha = 0.3,
  conf.int.value = 0.95,
  facet.labeller = NULL,
  ...
)
}
\arguments{
\item{data}{\code{stats::ts} instance}

\item{freq}{Length of frequency. If not provided, use time-series frequency}

\item{nrow}{Number of plot rows}

\item{ncol}{Number of plot columns}

\item{conf.int}{Logical flag indicating whether to plot confidence intervals}

\item{conf.int.colour}{line colour for confidence intervals}

\item{conf.int.linetype}{line type for confidence intervals}

\item{conf.int.fill}{fill colour for confidence intervals}

\item{conf.int.alpha}{alpha for confidence intervals}

\item{conf.int.value}{Coverage probability for confidence interval}

\item{facet.labeller}{A vector used as facet labels}

\item{...}{Keywords passed to autoplot.ts}
}
\value{
ggplot
}
\description{
Plot seasonal subseries of time series, generalization of \code{stats::monthplot}
}
\examples{
\dontrun{
ggfreqplot(AirPassengers)
ggfreqplot(AirPassengers, freq = 4)
ggfreqplot(AirPassengers, conf.int = TRUE)
}
}
