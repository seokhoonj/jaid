% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{max_by_dimnames}
\alias{max_by_dimnames}
\alias{min_by_dimnames}
\alias{sum_by_dimnames}
\title{max, min, sum by dim (row and column) names}
\usage{
max_by_dimnames(x, na.rm = TRUE)

min_by_dimnames(x, na.rm = TRUE)

sum_by_dimnames(x, na.rm = TRUE)
}
\arguments{
\item{x}{A numeric matrix}

\item{na.rm}{a boolean value removing NA or not}
}
\value{
A matrix of minimum values shrinked by dim (row and column) names
}
\description{
max, min, sum of values on each dim (row and column) name of a matrix.
}
\examples{
# max by dim names
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
max_by_dimnames(x)}

# min by dim names
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
min_by_dimnames(x)}

# sum by dim names
\donttest{x <- matrix(c(1:9), ncol = 3)
set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
sum_by_dimnames(x)}

}
