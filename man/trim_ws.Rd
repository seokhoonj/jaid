% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace.R
\name{trim_ws}
\alias{trim_ws}
\title{Trim white space}
\usage{
trim_ws(df, cols, ws = "[ \\t\\r\\n]")
}
\arguments{
\item{df}{a data.frame}

\item{cols}{a string vector specifying columns}

\item{ws}{a white space \link{regular expression}}
}
\value{
no return values
}
\description{
Trim white space
}
\examples{
\dontrun{df <- data.frame(x = c(" A", "B ", " C "), y = c(1, 2, 3))
set_ptr(df)
data.table::address(df)
trim_ws(df)
data.table::address(df)
df}

}
