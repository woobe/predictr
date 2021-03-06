% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/normalise.R
\name{normalise}
\alias{normalise}
\title{Normalise a data frame or matrix}
\usage{
normalise(x, method = c("center", "scale"))
}
\arguments{
\item{x}{a data frame or matrix.}

\item{method}{'range': normalise to values between 0 and 1.}
}
\description{
This function normalise a data frame or matrix.
}
\examples{
## Normalise a matrix
x <- matrix(rnorm(100), nrow = 10)
x_norm <- normalise(x)
print(summary(x))
print(summary(x_norm))
}

