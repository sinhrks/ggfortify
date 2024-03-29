% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tslib.R
\name{rbind_ts}
\alias{rbind_ts}
\title{Rbind original and predicted time-series-like instances as fortified \code{data.frame}}
\usage{
rbind_ts(
  data,
  original,
  ts.connect = TRUE,
  index.name = "Index",
  data.name = "Data"
)
}
\arguments{
\item{data}{Predicted/forecasted \code{ts} instance}

\item{original}{Original \code{ts} instance}

\item{ts.connect}{Logical frag indicates whether connects original time-series and predicted values}

\item{index.name}{Specify column name for time series index}

\item{data.name}{Specify column name for univariate time series data. Ignored in multivariate time series.}
}
\value{
data.frame
}
\description{
Rbind original and predicted time-series-like instances as fortified \code{data.frame}
}
\examples{
\dontrun{
predicted <- predict(stats::HoltWinters(UKgas), n.ahead = 5, prediction.interval = TRUE)
rbind_ts(predicted, UKgas, ts.connect = TRUE)
}
}
