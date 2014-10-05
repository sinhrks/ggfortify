\name{fortify.forecast}
\alias{fortify.forecast}
\title{Convert forecast::forecast to data.frame.}
\description{Convert forecast::forecast to data.frame.}
\examples{
d.arima <- forecast::auto.arima(AirPassengers)
d.forecast <- forecast::forecast(d.arima, level = c(95), h = 50)
fortify(d.forecast)
}
