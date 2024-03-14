#' Combine overlapping date ranges
#'
#' Combine multiple overlapping date ranges.
#'
#' @param df a data.frame with data ranges
#' @param id_var id column names
#' @param merge_var column names to be collapsed
#' @param from_var a column name of start date
#' @param to_var a column name of end date
#' @param interval an interval of previous end date to next start date to be combined
#' @param collapse an optional character string to seperate the result of `merge_var`
#' @return a `data.frame` with no overlapping date ranges.
#'
#' @examples
#' # combine overlapping date ranges
#' id <- c("A", "A", "B")
#' work <- c("cleansing", "analysis", "cleansing")
#' sdate <- as.Date(c("2022-03-01", "2022-03-05", "2022-03-08"))
#' edate <- as.Date(c("2022-03-06", "2022-03-09", "2022-03-10"))
#' df <- data.frame(id = id, work = work, sdate = sdate, edate = edate)
#' combine_overlapping_date_range(df, id, work, sdate, edate, interval = 0)
#'
#' @export
combine_overlapping_date_range <- function(df, id_var, merge_var, from_var, to_var,
                                           interval = 0, collapse = "|") {
  old_class <- class(df)
  set_dt(df)
  id_var    <- match_cols(df, sapply(rlang::enexpr(id_var), rlang::as_name))
  merge_var <- match_cols(df, sapply(rlang::enexpr(merge_var), rlang::as_name))
  from_var  <- rlang::as_name(rlang::enquo(from_var))
  to_var    <- rlang::as_name(rlang::enquo(to_var))
  all_var   <- c(id_var, merge_var, from_var, to_var)
  dt <- df[, .SD, .SDcols = all_var]
  setnames(dt, c(id_var, merge_var, "from", "to"))
  setorderv(dt, c(id_var, "from", "to"))
  set(dt, j = "sub_stay", value = 0)
  index <- .Call(IndexOverlappingDateRange, dt[, .SD, .SDcols = id_var],
                 dt$from, dt$to, interval = interval)
  set(dt, j = "loc", value = index$loc) # group index to combine
  set(dt, j = "sub", value = index$sub) # days to subtract, if the interval is longer than 0
  group_var <- c(id_var, "loc")
  m <- dt[, lapply(.SD, function(x) paste0(unique(x[!is.na(x)]), collapse = collapse)),
          keyby = group_var, .SDcols = merge_var]
  from <- to <- sub_stay <- sub <- NULL
  s <- dt[, list(from = min(from), to = max(to), sub_stay = sum(sub_stay) + sum(sub)),
          keyby = group_var]
  z <- m[s, on = group_var]
  set(z, j = "loc", value = NULL)
  set(z, j = "stay", value = as.numeric(z$to - z$from + 1 - z$sub_stay))
  set(z, j = "sub_stay", value = NULL)
  setnames(z, c(all_var, "stay"))
  data.table::setattr(z, "class", old_class)
  data.table::setattr(df, "class", old_class)
  return(z)
}
