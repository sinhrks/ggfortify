\name{fortify.survfit}
\alias{fortify.survfit}
\title{Convert survival::survfit to data.frame.}
\description{Convert survival::survfit to data.frame.}
\examples{
d.survfit <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
fortify(d.survfit)
}
