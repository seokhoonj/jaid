#' Create binned "bands" from a numeric column (class-preserving)
#'
#' Convert a numeric variable into ordered bands (factor levels) using
#' pretty, human-readable labels. Works with data.frame`, tibble, or
#' data.table. Internally converts to `data.table (via
#' [ensure_dt_env()]) to compute efficiently, then restores the original
#' class on return.
#'
#' @param df A data frame, tibble, or data.table.
#' @param var Unquoted column name to bin (e.g., `age`).
#' @param breaks Optional numeric vector of cut points **in ascending order**.
#'   If missing, breaks are generated from the observed range using
#'   `interval`. (Note: unlike [base::cut()], a single integer here is
#'   **not** interpreted as “number of bins”.)
#' @param interval Integer width for bins when `breaks` is missing. Default `5`.
#' @param right Logical; passed to [base::cut()]. If `TRUE`, intervals are
#'   right-closed `(a, b]`; if `FALSE` (default), left-closed `[a, b)`.
#' @param col_nm Optional name for the new band column. Default is
#'   `"<var>_band"`.
#' @param cutoff Logical; if `TRUE`, the upper break is forced to the
#'   observed maximum + 1, so the top band ends right above the maximum.
#'   Default `FALSE`.
#' @param label_type Label style for the band levels; one of:
#'   - `"close"` (default): `"a-b"` for all bands
#'   - `"open"`: first band `"-b"`, last band `"a-"`
#'   - `"open.start"`: first band `"-b"` only
#'   - `"open.end"`: last band `"a-"` only
#'
#' @return An object of the **same class as `df`**, with an ordered factor
#'   column appended (at `col_nm`). The new column is placed immediately
#'   after `var`.
#'
#' @details
#' - When `breaks` is missing, the function computes a lower bound at
#'   `floor(min(var)/interval) * interval` and an upper bound at
#'   `ceiling(max(var)/interval) * interval` (or `max(var)+1` if
#'   `cutoff = TRUE`), then sequences by `interval`.
#' - Labels are derived from the numeric breaks and formatted as
#'   `"a-b"` (with `b` shown as `b-1` when `right = FALSE`, to reflect
#'   `[a, b)` semantics). `"open"` variants replace the first/last label
#'   with `"-b"` / `"a-"` respectively.
#' - The resulting band column is an **ordered factor**.
#'
#' @examples
#' \donttest{
#' # Basic: 5-year bands, left-closed intervals
#' df <- mtcars
#' set_band(df, hp)
#'
#' # Custom width and right-closed intervals
#' set_band(df, mpg, interval = 10, right = TRUE, col_nm = "mpg_band")
#'
#' # Provide explicit cut points
#' set_band(df, disp, breaks = c(0, 150, 250, 400, 600))
#'
#' # Open-ended labels at both ends
#' set_band(df, qsec, interval = 2, label_type = "open")
#' }
#'
#' @export
set_band <- function(df, var, breaks, interval = 5, right = FALSE,
                     col_nm, cutoff = FALSE,
                     label_type = c("close", "open.start", "open", "open.end")) {
  jaid::assert_class(df, "data.frame")
  label_type <- match.arg(label_type)

  env <- ensure_dt_env(df)
  dt  <- env$dt

  var <- jaid::capture_names(dt, !!rlang::enquo(var))
  col <- dt[[var]]

  mn <- floor(min(col)/interval) * interval
  if (missing(breaks)) {
    if (!cutoff) {
      mx <- ceiling(max(col)/interval) * interval
      if (max(col) == mx)
        mx <- ceiling(max(col)/interval + 1) * interval
      breaks <- seq(mn, mx, interval)
    } else {
      mx <- max(col) + 1
      breaks <- seq(mn, mx, interval)
      breaks <- sort(unique(c(breaks, mx)))
      breaks <- breaks[breaks <= mx]
    }
  }
  col_band <- cut(col, breaks = breaks, right = right, dig.lab = 5)

  # build compact labels like "a-b"
  l <- levels(col_band)
  r <- gregexpr("[0-9]+", l, perl = TRUE)
  m <- regmatches(l, r)
  s <- as.integer(sapply(m, function(x) x[1L]))
  e <- as.integer(sapply(m, function(x) x[2L])) - 1
  labels <- sprintf("%d-%d", s, e)

  # apply open/closed label style
  if (label_type == "open") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  } else if (label_type == "open.start") {
    labels[1L] <- jaid::get_pattern("-[0-9]+", labels[1L])
  } else if (label_type == "open.end") {
    labels[length(labels)] <- jaid::get_pattern("[0-9]+-", labels[length(labels)])
  }
  levels(col_band) <- labels
  if (missing(col_nm)) col_nm <- sprintf("%s_band", var)
  data.table::set(dt, j = col_nm, value = ordered(col_band))
  data.table::setcolorder(dt, col_nm, after = var)

  env$restore(dt[])
}
