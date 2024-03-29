% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{equal}
\alias{equal}
\title{Equal columns of two data frames.}
\usage{
equal(x, y)
}
\arguments{
\item{x, y}{two data frames}
}
\value{
a logical vector
}
\description{
Whether the columns of two data frames are equal.
}
\examples{
# Are the columns of two data frames equal?
\donttest{equal(mtcars, mtcars)}

}
