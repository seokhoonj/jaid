#' Count Unique Stay Days Within Groups
#'
#' Computes the total number of **unique days** covered by overlapping or
#' adjacent date ranges, grouped by one or more ID and group variables.
#' Unlike a simple sum of durations, overlapping ranges are counted only once.
#'
#' Internally, this function calls a C backend (`CountStay`) for efficiency.
#'
#' @param df A data.frame containing the date ranges.
#' @param id_var One or more columns identifying the main ID (bare names).
#' @param group_var Additional grouping column(s) nested within each ID (bare names).
#' @param from_var Start date column (bare name). Must be of class `Date`.
#' @param to_var End date column (bare name). Must be of class `Date`.
#'
#' @return A `data.table` with one row per `(id_var, group_var)` combination,
#'   containing the number of **unique days** spanned by the given date ranges.
#'
#' @details
#' - Input is internally sorted by `id_var`, `group_var`, `from_var`, `to_var`.
#' - Dates are treated as **inclusive**; each `from_var` and `to_var` is counted.
#' - Overlapping or adjacent ranges do not double-count days.
#' - The implementation is optimized for **data.table** usage and its use is
#'   recommended for best performance, though any data.frame will work.
#'
#' @examples
#' \donttest{
#' dt <- data.frame(
#'   id    = c(1, 1, 1, 2, 2, 2),
#'   group = c("a", "a", "a", "b", "b", "c"),
#'   from  = as.Date(c("2024-01-01", "2023-12-27", "2024-01-03",
#'                     "2024-02-01", "2024-02-05", "2025-02-05")),
#'   to    = as.Date(c("2024-01-07", "2025-01-05", "2025-01-07",
#'                     "2024-02-03", "2024-02-06", "2025-02-06"))
#' )
#'
#' count_stay(dt, id, group, from, to)
#' #   id group stay
#' # 1  1     a  378
#' # 2  2     b    5
#' # 3  2     c    2
#' }
#'
#' @export
count_stay <- function(df, id_var, group_var, from_var, to_var) {
  assert_class(df, "data.frame")
  env <- ensure_dt_env(df)
  dt  <- env$dt

  id_var    <- capture_names(df, !!rlang::enquo(id_var))
  group_var <- capture_names(df, !!rlang::enquo(group_var))
  from_var  <- capture_names(df, !!rlang::enquo(from_var))
  to_var    <- capture_names(df, !!rlang::enquo(to_var))
  id_group_var <- c(id_var, group_var)

  data.table::setorderv(dt, c(id_var, group_var, from_var, to_var))
  id   <- dt[, .SD, .SDcols = id_group_var]
  from <- dt[[from_var]]
  to   <- dt[[to_var]]
  if (any(to - from < 0))
    stop("Some `from_var` are greater than `to_var`.")

  stay <- .Call(CountStay, id, from, to)
  z <- cbind(unique(id), stay = stay)

  env$restore(z)
}

limit_stay <- function(df, id_var, group_var, from_var, to_var,
                       limit, waiting, deduction = 0) {
  assert_class(df, "data.frame")
  env <- ensure_dt_env(df)
  dt  <- env$dt

  id_var    <- capture_names(dt, !!rlang::enquo(id_var))
  group_var <- capture_names(dt, !!rlang::enquo(group_var))
  from_var  <- capture_names(dt, !!rlang::enquo(from_var))
  to_var    <- capture_names(dt, !!rlang::enquo(to_var))
  id_group_var <- c(id_var, group_var)

  data.table::setorderv(dt, c(id_var, group_var, from_var, to_var))

  stay <- .build_binary_stay_vector(dt, id_var, group_var, from_var, to_var)

  dm <- dt[, .(from = min(.SD[[1L]]), to = max(.SD[[2L]])),
           by = id_group_var, .SDcols = c(from_var, to_var)]
  data.table::set(dm, j = "size", value = as.numeric(dm$to - dm$from + 1))

  stay_mod <- .Call(LimitStay, stay, dm$size, limit, waiting)
  from <- as.Date(unlist(seq_date_list(dm$from, dm$to), recursive = FALSE),
                  origin = as.Date("1970-01-01"))

  dm_id <- rep_row(dm[, .SD, .SDcols = id_group_var], times = dm$size)
  z <- data.table::data.table(dm_id, from = from, stay = stay, stay_mod = stay_mod)
  z <- z[!(stay == 0 & stay_mod == 0)]
  data.table::set(z, j = "month", value = bmonth(z$from))

  if (deduction > 0) {
    z[, rank := rank(from, ties.method = "first"), id_group_var]
    z[rank <= deduction, stay_mod := 0]
    rm_cols(z, rank)
  }
  id_group_period_var <- c(id_group_var, "month")

  zsum <- z[, .(
    from     = min(from),
    stay     = sum(stay),
    stay_mod = as.numeric(sum(stay_mod))
  ), by = id_group_period_var]

  data.table::set(zsum, j = "to", value = as.Date(
    ifelse(zsum$stay_mod > 0, zsum$from + zsum$stay_mod - 1L, zsum$from)
  , origin = as.Date("1970-01-01")))
  data.table::setnames(zsum, c("from", "to"), c(from_var, to_var))
  data.table::setcolorder(zsum, to_var, after = from_var)

  env$restore(zsum)
}

#' Build **binary** stay vector from date intervals
#'
#' Internal helper that converts `(from, to)` date intervals
#' (optionally grouped by one or more ID variables)
#' into a binary stay vector (0/1) at the daily level.
#'
#' This expands intervals into a day-by-day representation where:
#' - `1` means "stay present" (inside an interval)
#' - `0` means "stay absent" (outside intervals)
#'
#' Overlaps are not allowed; use [collapse_date_ranges()] first
#' if intervals may overlap.
#'
#' @param dt A data.table. Must already be ordered by grouping
#'   variables and date columns.
#' @param id_var Character vector naming the ID variable(s).
#' @param group_var Character vector naming optional grouping variables.
#' @param from_var Name of the column with start dates (`Date`).
#' @param to_var Name of the column with end dates (`Date`).
#'
#' @return Integer vector of 0/1 values, length equal to the
#'   total span of the expanded date intervals.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(
#'   id   = c(1, 1, 2),
#'   from = as.Date(c("2024-01-01", "2024-01-05", "2024-01-03")),
#'   to   = as.Date(c("2024-01-03", "2024-01-06", "2024-01-04"))
#' )
#' stay <- .build_binary_stay_vector(
#'   dt, id_var = "id", group_var = NULL, from_var = "from", to_var = "to"
#' )
#' head(stay)
#' }
.build_binary_stay_vector <- function(dt, id_var, group_var, from_var, to_var) {
  # combine ID + group vars
  id_group_var <- c(id_var, group_var)

  # safety checks
  if (!inherits(dt[[from_var]], "Date") || !inherits(dt[[to_var]], "Date"))
    stop("`from_var` and `to_var` must be Date columns.", call. = FALSE)
  if (any(dt[[to_var]] < dt[[from_var]]))
    stop("Some `from_var` > `to_var` detected.", call. = FALSE)

  # interleave start/end into a single sequence
  bounds <- as.Date(interleave(dt[[from_var]], dt[[to_var]]))

  # differences between consecutive dates (+1 for trailing edge)
  diff <- as.integer(c(diff(bounds), 1L))

  # build parallel ID vector (duplicated for start/end)
  dt_id <- rep_row(dt[, .SD, .SDcols = id_group_var], each = 2L)

  # find group breakpoints (drop the last element, which is total length)
  pt <- find_group_breaks(dt_id)
  if (length(pt)) diff[pt] <- 1L

  # alternate +1/-1 adjustments across start/end pairs
  adjs <- rep(c(1L, -1L), times = length(diff) / 2L)
  diff <- diff + adjs

  # validate overlap assumption
  if (any(diff < 0L))
    stop("Overlapping ranges detected; use collapse_date_ranges().", call. = FALSE)

  # build binary 0/1 stay vector
  bins <- rep(c(1L, 0L), times = length(diff) / 2L)
  stay <- rep(bins, diff)

  storage.mode(stay) <- "integer"

  stay
}
