#' Add months to a date
#'
#' Add a specified number of months to a date-like object. Negative values
#' subtract months. Uses base rollover via POSIXlt.
#'
#' @param date A date-like object (coercible via [as.POSIXlt()]).
#' @param mon Integer number of months to add (can be negative).
#'
#' @return A Date with the months added.
#'
#' @examples
#' \donttest{
#' # Add months
#' date <- Sys.Date()
#' add_mon(date, 3)
#' }
#'
#' @export
add_mon <- function(date, mon) {
  date <- as.POSIXlt(date)
  date$mon <- date$mon + mon
  as.Date(date)
}

#' Add years to a date
#'
#' Add a specified number of years to a date-like object. Negative values
#' subtract years. Uses base rollover via POSIXlt.
#'
#' @param date A date-like object (coercible via [as.POSIXlt()]).
#' @param year Integer number of years to add (can be negative).
#'
#' @return A Date with the years added.
#'
#' @examples
#' \donttest{
#' # Add years
#' date <- Sys.Date()
#' add_year(date, 3)
#' }
#'
#' @export
add_year <- function(date, year) {
  date <- as.POSIXlt(date)
  date$year <- date$year + year
  as.Date(date)
}

#' Beginning and end of the month
#'
#' Get the beginning date (first day) or end date (last day) of the month.
#'
#' @param date A Date.
#'
#' @return A Date object representing the beginning (bmonth) or end (emonth)
#'   of the month
#'
#' @examples
#' \donttest{
#' # Beginning of month
#' bmonth(Sys.Date())
#'
#' # End of month
#' emonth(Sys.Date())
#' }
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

#' Check if input is in a recognizable date format
#'
#' Test whether an object is a date or can be interpreted as a date according to
#' package-configured formats.
#'
#' @param x Object to test (Date, POSIXt, character, or factor).
#'
#' @return Logical scalar: `TRUE` if `x` is Date/POSIXt or all elements of `x`
#'   match the configured date format; `FALSE` otherwise.
#'
#' @export
is_date_format <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(TRUE)
  }
  if (inherits(x, c("character", "factor"))) {
    return(all(grepl(local(.DATE_FORMAT, envir = .JAID_ENV), x)))
  }
  FALSE
}

#' Format dates as YYYYMM
#'
#' Extract year and month from dates as a compact `YYYYMM` character string.
#'
#' @param x A Date vector.
#'
#' @return A character vector in `YYYYMM` format (e.g., "202312").
#'
#' @examples
#' \donttest{
#' # Get year-month
#' x <- as.Date(c("1999-12-31", "2000-01-01"))
#' yearmon(x)
#' }
#'
#' @export
yearmon <- function(x) {
  substr(format(x, format = "%Y%m%d"), 1L, 6L)
}

#' Difference in months between two dates
#'
#' Compute the number of months between two dates with an optional day
#' threshold. If the first element of `day_limit` is non-zero, the start date
#' counts as a full month when its day is `>= day_limit[1]`, and the end date
#' counts as a full month when its day is `< day_limit[1]`.
#'
#' @param sdate A start date vector
#' @param edate An end date vector
#' @param day_limit Day threshold for counting full months. If the start date's
#'   day is less than this value, it counts as a full month. If the end date's
#'   day is greater than or equal to this value, it counts as a full month.
#'
#' @return A numeric vector representing the number of months between the dates
#'
#' @examples
#' \donttest{
#' # Month difference
#' sdate <- as.Date("1999-12-31")
#' edate <- as.Date("2000-01-01")
#' mondiff(sdate, edate)
#' }
#'
#' @export
mondiff <- function(sdate, edate, day_limit = c(0:31)) {
  # if the sdate month day <  day_limit: count
  # if the edate month day >= day_limit: count
  assert_class(sdate, "Date")
  assert_class(edate, "Date")
  ys <- data.table::year(sdate)
  ye <- data.table::year(edate)
  ms <- data.table::month(sdate)
  me <- data.table::month(edate)
  ds <- de <- 0
  if (day_limit[1L]) {
    ds <- ifelse(data.table::mday(sdate) >= day_limit[1L], 1, 0)
    de <- ifelse(data.table::mday(edate) <  day_limit[1L], 1, 0)
  }
  z <- (ye - ys) * 12 + (me - ms) + 1 - ds - de
  as.numeric(z)
}

#' Collapse overlapping (or near-adjacent) date ranges by ID
#'
#' Merges multiple date ranges that either overlap or are within a user-defined
#' gap (`interval` days) **within each ID group**. Optionally collapses one or
#' more columns (`merge_var`) by concatenating unique values.
#'
#' @param df A `data.table` containing date ranges.
#' @param id_var One or more ID columns (bare names). Ranges are merged within each unique ID.
#' @param merge_var Optional columns (bare names) whose values will be collapsed
#'   over the merged range (see `collapse`).
#' @param from_var Start-date column (bare name). Must be `Date` (days).
#' @param to_var End-date column (bare name). Must be `Date` (days, inclusive).
#' @param interval Non-negative number of **days** allowed between the previous
#'   end date and the next start date to still be merged. For example,
#'   `interval = 0` merges touching ranges (end + 1 day == next start).
#' @param collapse Character delimiter used to combine `merge_var` values.
#'   Defaults to `"|"`. Duplicates and `NA` are removed before collapsing.
#'
#' @return A `data.table` with merged, non-overlapping date ranges. The output
#'   contains the ID columns, any `merge_var` columns (collapsed), the original
#'   date columns (`from_var`, `to_var`), and a `stay` column giving the number
#'   of inclusive days in each merged range.
#'
#' @details
#' - Input is internally sorted by `id_var`, `from_var`, `to_var`.
#' - Dates are treated as **inclusive**; adjacency is tested against `to + 1`.
#' - `interval` must be `>= 0` and is interpreted in whole days.
#' - Requires `data.table`. The merging index is computed in C for speed.
#'
#' @examples
#' \donttest{
#' id <- c("A","A","B")
#' work <- c("cleansing","analysis","cleansing")
#' sdate <- as.Date(c("2022-03-01","2022-03-05","2022-03-08"))
#' edate <- as.Date(c("2022-03-06","2022-03-09","2022-03-10"))
#' dt <- data.table::data.table(id = id, work = work, sdate = sdate, edate = edate)
#'
#' # Merge overlapping or touching ranges (interval = 0)
#' collapse_date_ranges(dt, id, work, sdate, edate, interval = 0)
#'
#' # Allow gaps up to 2 days to merge, and use comma to collapse labels
#' collapse_date_ranges(dt, id, work, sdate, edate, interval = 2, collapse = ", ")
#' }
#'
#' @export
collapse_date_ranges <- function(df, id_var, merge_var, from_var, to_var,
                                 interval = 0, collapse = "|") {
  id_var    <- match_cols(df, sapply(rlang::enexpr(id_var), rlang::as_name))
  merge_var <- match_cols(df, sapply(rlang::enexpr(merge_var), rlang::as_name))
  from_var  <- rlang::as_name(rlang::enquo(from_var))
  to_var    <- rlang::as_name(rlang::enquo(to_var))
  all_var   <- c(id_var, merge_var, from_var, to_var)
  dt <- df[, .SD, .SDcols = all_var]
  data.table::setnames(dt, c(id_var, merge_var, "from", "to"))
  data.table::setorderv(dt, c(id_var, "from", "to"))
  data.table::set(dt, j = "sub_stay", value = 0)
  index <- .Call(IndexOverlappingDateRange, dt[, .SD, .SDcols = id_var],
                 dt$from, dt$to, interval = interval)
  data.table::set(dt, j = "loc", value = index$loc) # group index to combine
  data.table::set(dt, j = "sub", value = index$sub) # days to subtract, if the interval is longer than 0
  group_var <- c(id_var, "loc")
  m <- dt[, lapply(.SD, function(x) paste(unique(x[!is.na(x)]), collapse = collapse)),
          keyby = group_var, .SDcols = merge_var]
  from <- to <- sub_stay <- sub <- NULL
  s <- dt[, list(from = min(from), to = max(to), sub_stay = sum(sub_stay) + sum(sub)),
          keyby = group_var]
  z <- m[s, on = group_var]
  data.table::set(z, j = "loc", value = NULL)
  data.table::set(z, j = "stay", value = as.numeric(z$to - z$from + 1 - z$sub_stay))
  data.table::set(z, j = "sub_stay", value = NULL)
  data.table::setnames(z, c(all_var, "stay"))
  return(z)
}

#' Deprecated: combine_overlapping_date_range
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Use [collapse_date_ranges()] instead.
#'
#' @param ...	Additional arguments passed to combine_overlapping_date_range().
#'
#' @return A data.table with merged, non-overlapping date ranges. The output
#'   contains the ID columns, any `merge_var` columns (collapsed), the original
#'   date columns (`from_var`, `to_var`), and a `stay` column giving the number
#'   of inclusive days in each merged range.
#'
#' @seealso [write_data_xlsx()]
#'
#' @export
combine_overlapping_date_range <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9000", "combine_overlapping_date_ranges()", "collapse_date_ranges()")
  collapse_date_ranges(...)
}
