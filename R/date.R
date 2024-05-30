#' Add months
#'
#' Add months to the date.
#'
#' @param date A date
#' @param mon A number of months to be added
#'
#' @return A date
#'
#' @examples
#' # add months
#' \donttest{
#' date <- Sys.Date()
#' add_mon(date, 3)}
#'
#' @export
add_mon <- function (date, mon) {
  date <- as.POSIXlt(date)
  date$mon <- date$mon + mon
  return(as.Date(date))
}

#' Add years
#'
#' Add years to the date.
#'
#' @param date A date
#' @param year A number of years to be added
#'
#' @return A date
#'
#' @examples
#' # add years
#' \donttest{
#' date <- Sys.Date()
#' add_year(date, 3)}
#'
#' @export
add_year <- function(date, year) {
  date <- as.POSIXlt(date)
  date$year <- date$year + year
  as.Date(date)
}

#' Beginning of the month, End of the month
#'
#' Get the beginning of the month.
#'
#' @param date A date
#'
#' @return A date
#'
#' @examples
#' # the beginning of the month
#' \donttest{
#' bmonth(Sys.Date())}
#'
#' # the end of the month
#' \donttest{
#' emonth(Sys.Date())}
#'
#' @export
bmonth <- function(date) {
  as.Date(format(as.Date(date), format = "%Y-%m-01"))
}

#' @rdname bmonth
#' @export
emonth <- function(date) {
  add_mon(date, 1L) - 1L
}

#' Is a date format?
#'
#' Is a date or date format vector?
#'
#' @param x value to check
#' @return a logical whether it's a date format or not.
#'
#' @export
is_date_format <- function(x) {
  if (inherits(x, c("Date", "POSIXt")))
    return(TRUE)
  if (inherits(x, c("character", "factor"))) {
    return(all(grepl(local(.DATE_FORMAT, envir = .JAID_ENV), x)))
  }
  return(FALSE)
}

#' Year month
#'
#' Get an year month from the date.
#'
#' @param x a date vector
#' @return a character vector
#'
#' @examples
#' # get year month
#' x <- as.Date(c("1999-12-31", "2000-01-01"))
#' yearmon(x)
#'
#' @export
yearmon <- function(x) {
  # sprintf("%4d%02d", lubridate::year(x), lubridate::month(x))
  substr(format(x, format = "%Y%m%d"), 1L, 6L)
}
