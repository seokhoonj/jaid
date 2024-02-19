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
