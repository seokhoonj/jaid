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

  z
}

#' Generate sequences of Dates for each (from, to) range
#'
#' Expands paired start (`from`) and end (`to`) dates into full
#' daily sequences. Each element of the output corresponds to
#' one row of input, and contains a sequence of `Date` values
#' from `from[i]` to `to[i]` (inclusive).
#'
#' Optionally, a `label` vector can be provided to tag each
#' sequence in the output.
#'
#' @param from A Date vector of start dates.
#' @param to A Date vector of end dates. Must be the same
#'   length as `from`.
#' @param label Optional vector of labels (same length as `from`).
#'
#' @return A list of `Date` vectors, each element representing
#'   a full daily sequence between corresponding `from` and `to`.
#'
#' @details
#' - Both `from` and `to` must be of class `Date`.
#' - Sequences are inclusive of both endpoints.
#' - If `label` is supplied, it is attached to the resulting list
#'   as element names.
#'
#' @examples
#' from <- as.Date(c("2024-01-01", "2024-02-01"))
#' to   <- as.Date(c("2024-01-03", "2024-02-02"))
#'
#' # Expand to daily sequences
#' seq_date_list(from, to)
#'
#' # With labels
#' seq_date_list(from, to, label = c("A", "B"))
#'
#' @export
seq_date_list <- function(from, to, label = NULL) {
  stopifnot(inherits(from, "Date"), inherits(to, "Date"))
  .Call(SeqDateList, from, to, label)
}

#' Collapse overlapping (or near-adjacent) date ranges by (id, group)
#'
#' Merges multiple date ranges that either overlap or are within a user-defined
#' gap (`interval` days) *within each (id, group) block*. Optionally collapses
#' one or more columns (`merge_var`) by concatenating unique values.
#'
#' @param df A data.frame or data.table containing date ranges.
#' @param id_var One or more ID columns (bare names). Ranges are merged within each unique ID.
#' @param group_var Optional additional grouping columns (bare names).
#' @param merge_var Optional columns (bare names) whose values will be collapsed
#'   over the merged range; duplicates and `NA` are removed before collapsing.
#' @param from_var Start-date column (bare name). Must be `Date` (days).
#' @param to_var End-date column (bare name). Must be `Date` (days, inclusive).
#' @param interval Integer gap (in days) allowed between consecutive ranges to still merge.
#'   Use `0` to merge touching ranges, positive values to allow gaps, and **`-1` to require
#'   actual overlap** (touching ranges are *not* merged).
#' @param collapse Character delimiter used to combine `merge_var` values (default `"|"`).
#'
#' @return A data.table with merged, non-overlapping date ranges. The output contains:
#' * the grouping keys (`id_var`, `group_var`),
#' * the merged date columns (`from_var`, `to_var`),
#' * the `stay` column = number of inclusive days in each merged range,
#' * and the collapsed `merge_var` columns if provided.
#'
#' @details
#' * Input is internally sorted by `id_var`, `group_var`, `from_var`, `to_var`.
#' * Dates are treated as **inclusive**; adjacency is tested against `to + 1`.
#' * `interval` is a single integer and must be `>= -1`.
#' * Using `data.table` is recommended for performance; base data.frame also works.
#' * The merging index is computed in C (via `.Call(IndexOverlappingDateRanges, ...)`) for speed.
#'
#' @examples
#' \donttest{
#' id <- c("A","A","B","B")
#' group <- c("x","x","y","y")
#' work  <- c("cleansing", "analysis", "cleansing", "QA")
#' from <- as.Date(c("2022-03-01","2022-03-05","2022-03-08","2022-03-12"))
#' to <- as.Date(c("2022-03-06","2022-03-09","2022-03-10","2022-03-15"))
#' dt <- data.table::data.table(id = id, group = group, work = work,
#'                              from = from, to = to)
#'
#' # Merge touching ranges (interval = 0)
#' collapse_date_ranges(
#'   dt, id_var = id, group_var = group, merge_var = work,
#'   from_var = from, to_var = to,
#'   interval = 0
#' )
#'
#' # Require actual overlap (touching ranges NOT merged): interval = -1
#' collapse_date_ranges(
#'   dt, id_var = id, group_var = group, merge_var = work,
#'   from_var = from, to_var = to,
#'   interval = -1
#' )
#'
#' # Allow up to 2-day gaps
#' collapse_date_ranges(
#'   dt, id_var = id, group_var = group, merge_var = work,
#'   from_var = from, to_var = to,
#'   interval = 2, collapse = ", "
#' )
#' }
#'
#' @export
collapse_date_ranges <- function(df, id_var, group_var, merge_var,
                                 from_var, to_var, interval = 0L,
                                 collapse = "|") {
  assert_class(df, "data.frame")
  env <- ensure_dt_env(df)
  dt  <- env$dt

  id_var    <- capture_names(dt, !!rlang::enquo(id_var))
  group_var <- capture_names(dt, !!rlang::enquo(group_var))
  merge_var <- capture_names(dt, !!rlang::enquo(merge_var))
  from_var  <- capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- capture_names(dt, !!rlang::enquo(to_var))

  # ensure Date type
  stopifnot(inherits(dt[[from_var]], "Date"), inherits(dt[[to_var]], "Date"))

  # basic sanity: from <= to
  if (any(dt[[to_var]] - dt[[from_var]] < 0, na.rm = TRUE))
    stop("Some ranges have `from_var` > `to_var`.", call. = FALSE)

  # interval: allow -1 (require overlap), 0 (touching ok), >0 (allow gaps)
  interval <- as.integer(interval)
  if (length(interval) != 1L || is.na(interval) || interval < -1L)
    stop("`interval` must be a single integer >= -1.", call. = FALSE)

  # materialize working table (only needed columns), rename date cols
  id_group_var <- c(id_var, group_var)
  all_var <- c(id_var, group_var, merge_var, from_var, to_var)
  dt <- dt[, .SD, .SDcols = all_var]
  data.table::setnames(dt, c(id_var, group_var, merge_var, "from", "to"))
  data.table::setorderv(dt, c(id_var, group_var, "from", "to"))

  # bookkeeping column for later adjustments (kept numeric)
  data.table::set(dt, j = "sub_stay", value = 0)

  # compute merging index in C
  index <- .Call(IndexOverlappingDateRanges,
                 dt[, .SD, .SDcols = id_group_var],
                 dt$from, dt$to, interval)

  data.table::set(dt, j = "loc", value = index$loc)  # group index (within id/group)
  data.table::set(dt, j = "sub", value = index$sub)  # subtract days when gaps allowed

  id_group_loc_var <- c(id_group_var, "loc")

  # collapse merge_var (if present) or just keep keys
  if (length(merge_var)) {
    m <- dt[
      ,
      lapply(.SD, function(x) paste(unique(x[!is.na(x)]), collapse = collapse)),
      keyby = id_group_loc_var, .SDcols = merge_var
    ]
  } else {
    m <- unique(dt[, .SD, .SDcols = id_group_loc_var])
    data.table::setkeyv(m, id_group_loc_var)
  }

  # compute merged bounds and stay
  from <- to <- sub_stay <- sub <- NULL
  s <- dt[
    ,
    list(from = min(from),
         to   = max(to),
         sub_stay = sum(sub_stay) + sum(sub)),
    keyby = id_group_loc_var
  ]

  z <- m[s, on = id_group_loc_var]
  data.table::set(z, j = "loc", value = NULL)
  data.table::set(z, j = "stay", value = as.numeric(z$to - z$from + 1 - z$sub_stay))
  data.table::set(z, j = "sub_stay", value = NULL)

  # restore original column names order
  data.table::setnames(z, c(id_var, group_var, merge_var, from_var, to_var, "stay"))

  env$restore(z)
}

#' Deprecated: combine_overlapping_date_range
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Use [collapse_date_ranges()] instead.
#'
#' @param ...	Additional arguments passed to combine_overlapping_date_range().
#'
#' @return Same return value as [collapse_date_ranges()]
#'
#' @seealso [collapse_date_ranges()]
#'
#' @export
combine_overlapping_date_range <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9000", "combine_overlapping_date_ranges()", "collapse_date_ranges()")
  collapse_date_ranges(...)
}
