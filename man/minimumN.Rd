% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minimumN.R
\name{minimumN}
\alias{minimumN}
\title{Calculate Minimum Sample Size for t-test}
\usage{
minimumN(x1, x2 = NULL)
}
\arguments{
\item{x1}{A numeric vector representing sample data. If x2 is NULL, a one-sample t-test is performed against a mean of 0.}

\item{x2}{An optional numeric vector representing a second sample for a two-sample t-test.}
}
\value{
The minimum sample size required to achieve the desired power level.
}
\description{
This function calculates the minimum sample size required to achieve 80\% power at a 5\% significance level
for a one-sample or two-sample t-test. It wraps the \code{pwr::pwr.t2n.test} function.
}
\examples{
x1 = rnorm(10, mean = 3)
x2 = rnorm(10, mean = 3.5)
minimumN(x1, x2) # Two-sample t-test
minimumN(x1) # One-sample t-test against a mean of 0

}
