% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/batch_write.R
\name{batch_write}
\alias{batch_write}
\title{Writing data to CSV file in batch mode}
\usage{
batch_write(data, file, apd = FALSE, batch_size = 10000, pause = 0)
}
\arguments{
\item{data}{the tabular object (matrix or data frame)}

\item{file}{CSV filename}

\item{apd}{append existing file (TRUE or FALSE)}

\item{batch_size}{number of rows to be written for each batch}

\item{pause}{seconds timeout between batch}
}
\description{
This function is especially useful for writing large files.
}
\examples{
## T.B.A.
}

