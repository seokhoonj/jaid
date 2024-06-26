% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str.R
\name{split_str}
\alias{split_str}
\title{Split strings}
\usage{
split_str(x, split = "|")
}
\arguments{
\item{x}{a string vector}

\item{split}{a string to use for splitting}
}
\value{
a string vector
}
\description{
Split strings
}
\examples{
# split strings
\donttest{split_str(c("a|b|a", "c|d|c"), split = "|")}

}
