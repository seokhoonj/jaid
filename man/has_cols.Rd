% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{has_cols}
\alias{has_cols}
\title{Has columns}
\usage{
has_cols(df, cols, error_raise = FALSE)
}
\arguments{
\item{df}{a data.frame}

\item{cols}{column names}

\item{error_raise}{a logical whether to raise an error or not}
}
\value{
a logical value
}
\description{
Whether the data has specific columns
}
\examples{
# has columns
\donttest{has_cols(mtcars, c("cyl", "disp"))}

# raise an error
\dontrun{
has_cols(mtcars, c("cyl", "iris"), error_raise = TRUE)}

}
