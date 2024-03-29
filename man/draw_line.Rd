% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{draw_line}
\alias{draw_line}
\title{Draw a line}
\usage{
draw_line(width, mark = "=")
}
\arguments{
\item{width}{a numeric vector specifying width (default: \code{getOption("width")})}

\item{mark}{a string specifying mark (default: "=")}
}
\value{
a string vector with repeated mark
}
\description{
Draw a line in console.
}
\examples{
\donttest{cat(draw_line())}

}
