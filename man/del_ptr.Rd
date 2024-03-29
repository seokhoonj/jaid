% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ptr.R
\name{del_ptr}
\alias{del_ptr}
\title{Delete external pointer}
\usage{
del_ptr(df)
}
\arguments{
\item{df}{a data.frame}
}
\value{
No return value.
}
\description{
Delete external pointer
}
\examples{
# delete pointer
\donttest{df <- data.frame(x = c(1:3), y = c("a", "b", "c"))
data.table::setalloccol(df)
get_ptr(df)
del_ptr(df)
get_ptr(df) # NULL}

}
