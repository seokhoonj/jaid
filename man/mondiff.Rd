% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date.R
\name{mondiff}
\alias{mondiff}
\title{Month difference}
\usage{
mondiff(sdate, edate, day_limit = c(0:31))
}
\arguments{
\item{sdate}{a start date vector}

\item{edate}{a end date vector}

\item{day_limit}{if the day of sdate is less than \code{day_limit}, count it as a
full month, if the day of edate is greater than or equal to \code{day_limit},
count it as a full month.}
}
\value{
a numeric vector
}
\description{
Month difference between two dates
}
\examples{
# mondiff
sdate <- as.Date("1999-12-31")
edate <- as.Date("2000-01-01")
mondiff(sdate, edate)

}
