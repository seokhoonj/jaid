#' Set statistic by groups
#'
#' Set statistic column by groups (mainly cumulative function like [`cumsum()`], [`cumprod()`], [`cummax()`], [`cummin()`])
#'
#' @param df a data.frame
#' @param group_var names of columns for grouping
#' @param value_var names of columns to be applied for statistics.
#' @param fun a function applying to columns
#' @param prefix a new column's perfix string (if the value_var is "loss", the new column is "closs")
#' @return no return values.
#'
#' @examples
#' # set statistics by groups
#' \dontrun{
#' set_ptr(mtcars)
#' set_stat_by(mtcars, .(cyl, vs), value_var = hp, fun = cumsum)
#' mtcars[, c("cyl", "vs", "chp")]}
#'
#' @export
set_stat_by <- function(df, group_var, value_var, fun = cumsum, prefix = "c") {
  # has_ptr(df, error_raise = TRUE)
  # old_class <- class(df)
  # set_dt(df)
  grps <- match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  vals <- match_cols(df, sapply(rlang::enexpr(value_var), rlang::as_name))
  cols <- sprintf("%s%s", prefix, vals)
  df[, `:=`((cols), lapply(.SD, fun)), keyby = grps, .SDcols = vals]
  # data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Get statistic by groups
#'
#' Get statistic column by groups
#'
#' @param df a data.frame
#' @param group_var names of columns for grouping
#' @param value_var names of columns to be applied for statistics.
#' @param fun a function applying to columns
#' @return a grouped data.frame
#'
#' @examples
#' # get statistics by groups
#' \dontrun{
#' set_ptr(mtcars)
#' get_stat_by(mtcars, .(cyl, vs), value_var = .(hp, drat), fun = sum)}
#'
#' @export
get_stat_by <- function(df, group_var, value_var, fun = sum) {
  # has_ptr(df, error_raise = TRUE)
  # old_class <- class(df)
  # set_dt(df)
  grps <- match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  vals <- match_cols(df, sapply(rlang::enexpr(value_var), rlang::as_name))
  dt <- df[, lapply(.SD, fun), keyby = grps, .SDcols = vals]
  # data.table::setattr(dt, "class", old_class)
  # data.table::setattr(df, "class", old_class)
  return(dt)
}
