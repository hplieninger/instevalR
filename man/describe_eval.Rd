% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_eval.R
\name{describe_eval}
\alias{describe_eval}
\title{Descriptives for all courses}
\usage{
describe_eval(data, CI = 0.95)
}
\arguments{
\item{data}{Data frame as returned from \code{\link{join_eval}}}

\item{CI}{Numeric. Confidence level of error bars, often \code{.95} for a 95\% CI.}
}
\value{
A data frame (more precisely, a tibble).
}
\description{
This function takes a data frame from \code{\link{join_eval}} containing
data from multiple courses and calculates descriptive statistics such as the
mean.
}
\examples{
\dontrun{
tmp1 <- read_eval("./data")
dat1 <- join_eval(tmp1)
describe_eval(dat1)
}
}
