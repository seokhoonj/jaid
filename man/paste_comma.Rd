% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{paste_comma}
\alias{paste_comma}
\title{Paste comma}
\usage{
paste_comma(x, newline = FALSE)
}
\arguments{
\item{x}{a vector}

\item{newline}{a logical whether to add newlines by each element}
}
\description{
Paste vector elements with commas.
}
\examples{
# paste comma
\donttest{paste_comma(names(mtcars))}

}
