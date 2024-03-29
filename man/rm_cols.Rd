% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm.R
\name{rm_cols}
\alias{rm_cols}
\title{Remove columns}
\usage{
rm_cols(df, cols)
}
\arguments{
\item{df}{a data.frame}

\item{cols}{name of columns to remove (like .(x, y), list(x, y))}
}
\value{
no return value
}
\description{
Remove columns in a memory efficient way.
}
\examples{
# remove columns
\donttest{data <- mtcars
pryr::address(data)
rm_cols(data, .(mpg, cyl, disp))
pryr::address(data)}

}
