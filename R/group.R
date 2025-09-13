#' Add group-wise cumulative statistics
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Adds statistic columns computed **within groups**, typically using
#' cumulative functions such as [`cumsum()`], [`cumprod()`], [`cummax()`],
#' or [`cummin()`].
#'
#' @param dt A data.table (or convert beforehand with `data.table::setDT()`).
#' @param group_var Columns to group by.
#' @param value_var Columns to apply the function to.
#' @param fun A function to apply. Default is `cumsum`
#' @param prefix A new column's perfix string (if the value_var is "loss",
#'   the new column is "closs")
#'
#' @return No return values.
#'
#' @examples
#' \donttest{
#' # Cumulative sum within groups
#' dt <- data.table::as.data.table(mtcars)
#' add_group_stats(dt, group_var = .(cyl), value_var = c(hp, mpg), fun = cumsum)
#' }
#'
#' @export
add_group_stats <- function(dt, group_var, value_var, fun = cumsum,
                            prefix = "c") {
  lifecycle::signal_stage("experimental", "add_group_stats()")
  assert_class(dt, "data.table")
  # grps <- match_cols(dt, sapply(rlang::enexpr(group_var), rlang::as_name))
  # vals <- match_cols(dt, sapply(rlang::enexpr(value_var), rlang::as_name))
  grps <- capture_names(dt, !!rlang::enquo(group_var))
  vals <- capture_names(dt, !!rlang::enquo(value_var))
  cols <- sprintf("%s%s", prefix, vals)
  dt[, `:=`((cols), lapply(.SD, fun)), keyby = grps, .SDcols = vals]
  dt
}

#' Summarise group-wise statistics
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes summary statistics for selected columns, grouped by one or more
#' variables. The function `fun` is applied to each value column within each group.
#'
#' @details
#' - Unlike [add_group_stats()], this function expects aggregation functions
#'   that return a scalar (e.g., `sum`, `mean`, `max`).
#' - The result is one row per group, with summary values for each variable.
#'
#' @param dt A data.table. (Convert beforehand with `data.table::setDT()` if needed.)
#' @param group_var Unquoted column(s) used for grouping (e.g., `.(grp1, grp2)`).
#' @param value_var Unquoted column(s) to summarise.
#' @param fun A summary function such as `sum`, `mean`, `max`. Must return a scalar.
#'
#' @return A grouped summary data.table, with one row per group and one
#'   column per statistic.
#'
#' @examples
#' \donttest{
#' dt <- data.table::as.data.table(mtcars)
#'
#' # Group-wise sums
#' summarise_group_stats(dt, .(cyl, vs), value_var = .(hp, drat), fun = sum)
#'
#' # Group-wise means
#' summarise_group_stats(dt, .(cyl), value_var = .(mpg, wt), fun = mean)
#' }
#'
#' @export
summarise_group_stats <- function(dt, group_var, value_var, fun = sum) {
  lifecycle::signal_stage("experimental", "summarise_group_stats()")
  # grps <- match_cols(dt, sapply(rlang::enexpr(group_var), rlang::as_name))
  # vals <- match_cols(dt, sapply(rlang::enexpr(value_var), rlang::as_name))
  grps <- capture_names(dt, !!rlang::enquo(group_var))
  vals <- capture_names(dt, !!rlang::enquo(value_var))
  dt[, lapply(.SD, fun), keyby = grps, .SDcols = vals]
}

#' Compute frequency proportions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Given a vector, compute the relative frequency of each unique value
#' and return the result as a data.frame.
#'
#' @param x A vector (numeric, character, or factor).
#'
#' @return A data.table with unique values and their proportions.
#'
#' @examples
#' \donttest{
#' # Frequency proportions
#' x <- sample(1:10, 1000, replace = TRUE)
#' freq_prop(x)
#' }
#'
#' @export
freq_prop <- function(x) {
  lifecycle::signal_stage("experimental", "freq_prop()")
  op <- options(scipen = 14L)
  on.exit(op)
  freq <- table(x)
  prop <- prop.table(freq)
  df <- as.data.table(freq)
  dp <- as.data.table(prop)
  data.table::setnames(df, c("x", "freq"))
  data.table::setnames(dp, c("x", "prop"))
  prop <- NULL
  df[dp, prop := prop, on = .(x)]
  df[]
}
