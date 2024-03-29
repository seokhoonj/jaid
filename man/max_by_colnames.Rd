% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{max_by_colnames}
\alias{max_by_colnames}
\alias{min_by_colnames}
\alias{sum_by_colnames}
\title{max, min, sum by column names}
\usage{
max_by_colnames(x, na.rm = TRUE)

min_by_colnames(x, na.rm = TRUE)

sum_by_colnames(x, na.rm = TRUE)
}
\arguments{
\item{x}{A numeric matrix}

\item{na.rm}{a boolean value removing NA or not}
}
\value{
A matrix of minimum values shrinked by column names
}
\description{
max, min, sum of values on each column name of a matrix.
}
\examples{
# max by col nm
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
max_by_colnames(x)}

# min by col nm
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
min_by_colnames(x)}

# sum by col nm
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
sum_by_colnames(x)}

}
