% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rep.R
\name{rep_row}
\alias{rep_row}
\alias{rep_row.matrix}
\alias{rep_row.data.frame}
\alias{rep_row.data.table}
\title{Data replication function}
\usage{
rep_row(x, ...)

\method{rep_row}{matrix}(x, ...)

\method{rep_row}{data.frame}(x, ...)

\method{rep_row}{data.table}(x, ...)
}
\arguments{
\item{x}{matrix, data.frame, data.table}

\item{...}{times, each}
}
\value{
matrix, data.frame, data.table.
}
\description{
This function is for replicating data rows
}
\examples{
rep_row(iris, times = 3)
rep_row(iris, each  = 3)

}
