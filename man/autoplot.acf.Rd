% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify_stats.R
\name{autoplot.acf}
\alias{autoplot.acf}
\title{Autoplot \code{stats::acf}. Note to pass `plot = FALSE` to original function to suppress
standard plot output}
\usage{
\method{autoplot}{acf}(
  object,
  colour = "#000000",
  linetype = "solid",
  conf.int = TRUE,
  conf.int.colour = "#0000FF",
  conf.int.linetype = "dashed",
  conf.int.fill = NULL,
  conf.int.alpha = 0.3,
  conf.int.value = 0.95,
  conf.int.type = "white",
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  log = "",
  main = NULL,
  xlab = NULL,
  ylab = "ACF",
  asp = NULL,
  ...
)
}
\arguments{
\item{object}{\code{stats::acf} instance}

\item{colour}{Line colour}

\item{linetype}{Line type}

\item{conf.int}{Logical flag indicating whether to plot confidence intervals}

\item{conf.int.colour}{line colour for confidence intervals}

\item{conf.int.linetype}{line type for confidence intervals}

\item{conf.int.fill}{fill colour for confidence intervals}

\item{conf.int.alpha}{alpha for confidence intervals}

\item{conf.int.value}{Coverage probability for confidence interval}

\item{conf.int.type}{Type of confidence interval, 'white' for white noise or 'ma' MA(k-1) model}

\item{xlim}{limits for x axis}

\item{ylim}{limits for y axis}

\item{log}{which variables to log transform ("x", "y", or "xy")}

\item{main}{character vector or expression for plot title}

\item{xlab}{character vector or expression for x axis label}

\item{ylab}{character vector or expression for y axis label}

\item{asp}{the y/x aspect ratio}

\item{...}{other arguments passed to methods}
}
\value{
ggplot
}
\description{
Autoplot \code{stats::acf}. Note to pass `plot = FALSE` to original function to suppress
standard plot output
}
\examples{
\dontrun{
autoplot(stats::acf(AirPassengers, plot = FALSE))
autoplot(stats::pacf(AirPassengers, plot = FALSE))
autoplot(stats::ccf(AirPassengers, AirPassengers, plot = FALSE))
}
}
