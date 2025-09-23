#' Add group-wise statistics (class-preserving)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Add one or more statistic columns computed **within groups**. The input can be
#' a base data.frame, a tibble, or a data.table. Internally this converts to
#' data.table (via [ensure_dt_env()]) to compute **by reference**, then restores
#' the original class on return.
#'
#' @param x A data.frame, tibble, or data.table.
#' @param group_var Columns to group by (NSE supported: `.(g1, g2)`, `c(g1, g2)`,
#'   or a character vector).
#' @param value_var Column(s) that `fun` is applied to (same NSE rules).
#' @param fun A function (e.g., `sum`, `mean`, `cumsum`) applied to each value
#'   column, **or** a vector/list of functions the same length as `value_var`
#'   (elementwise mapping).
#' @param col_names Optional character vector of output column names. Must have
#'   the same length as `value_var`. If omitted, names are auto-generated from
#'   `prefix`, original name, and `suffix`.
#' @param prefix,suffix Strings to prepend/append when auto-naming. For example,
#'   if `value_var = "loss"` and `prefix = "c"`, the new column becomes `"closs"`.
#' @param overwrite Logical; if `FALSE` (default), error when any `col_names`
#'   already exist in `x`. Set `TRUE` to overwrite.
#'
#' @return An object of the **same class as `x`**, augmented with the new columns.
#'
#' @examples
#' \donttest{
#' # Works with data.table
#' dt <- data.table::as.data.table(mtcars)
#' dt2 <- add_group_stats(dt, group_var = cyl, value_var = c(hp, mpg),
#'                        fun = cumsum, prefix = "cumsum_")
#'
#' # Works with data.frame
#' df <- mtcars
#' df2 <- add_group_stats(df, group_var = cyl, value_var = c(hp, mpg),
#'                        fun = sum, suffix = "_sum")
#'
#' # Different function per column (elementwise mapping)
#' dt3 <- add_group_stats(dt,
#'                        group_var = cyl,
#'                        value_var = c(hp, mpg, wt),
#'                        fun = list(mean, sum, max),
#'                        col_names = c("hp_mean", "mpg_sum", "wt_max"),
#'                        overwrite = TRUE)
#' }
#'
#' @export
add_group_stats <- function(x,
                            group_var,
                            value_var,
                            fun = sum,
                            col_names = NULL,
                            prefix = "",
                            suffix = "",
                            overwrite = FALSE) {
  lifecycle::signal_stage("experimental", "add_group_stats()")

  # Ensure a data.table backend but preserve original class on return
  env <- ensure_dt_env(x)
  dt  <- env$dt

  # Resolve columns (supports NSE or character)
  grps <- capture_names(dt, !!rlang::enquo(group_var))
  vals <- capture_names(dt, !!rlang::enquo(value_var))

  # Validate function
  funs <- if (rlang::is_function(fun)) {
    rep(list(fun), length(vals))
  } else if (is.character(fun)) {
    lapply(fun, match.fun)
  } else {
    fun
  }
  if (length(funs) == 1L)
    funs <- rep(funs, length(vals))
  if (length(funs) != length(vals))
    stop("Length of `fun` must be 1 or equal to length of `value_var`.",
         call. = FALSE)

  # Determine output names
  out_names <- if (!is.null(col_names)) {
    if (length(col_names) != length(vals)) {
      stop("`col_names` must have the same length as `value_var`.",
           call. = FALSE)
    }
    col_names
  } else {
    paste0(prefix, vals, suffix)
  }

  # Overwrite guard
  dup <- intersect(out_names, names(dt))
  if (length(dup) > 0 && !overwrite) {
    stop(
      "Output column(s) already exist in `x`: ",
      paste(dup, collapse = ", "),
      "\nUse `overwrite = TRUE` to replace them.",
      call. = FALSE
    )
  }

  # Compute and assign by reference
  dt[, `:=`((out_names), Map(function(j, f) f(.SD[[j]]), seq_along(vals), funs)),
     keyby = grps, .SDcols = vals]

  # Restore original class (if needed) and return
  env$restore(dt[])
}

#' Summarise group-wise statistics
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Compute scalar summaries for one or more value columns, grouped by
#' one or more variables. The function `fun` is applied to each value
#' column **within each group** and must return a length-1 result.
#'
#' @details
#' - Unlike [add_group_stats()], this performs **aggregation**
#'   (one row per group). `fun` should return a scalar (e.g. `sum`, `mean`,
#'   `max`, or a custom length-1 function).
#' - Accepts a single function for all columns, or a vector/list of functions
#'   mapped elementwise to `value_var`.
#' - Works with data.table, data.frame, or tibble. Non-data.table inputs are
#'   converted internally and restored to the original class on return.
#'
#' @param x A data.table, data.frame, or tibble.
#' @param group_var Unquoted grouping columns (e.g., `.(grp1, grp2)` or `c(grp1, grp2)`),
#'   or a character vector. If missing, a single overall summary is returned.
#' @param value_var Unquoted value column(s) to summarise (same NSE rules as `group_var`),
#'   or a character vector.
#' @param fun A summary function (e.g., `sum`, `mean`, `max`) that returns a scalar,
#'   **or** a vector/list of functions with length equal to `value_var`.
#' @param col_names Optional character vector of output names. Must have the same length
#'   as `value_var`. If omitted, names are auto-generated from `prefix`, original name,
#'   and `suffix`.
#' @param prefix,suffix Strings to prepend/append when auto-naming output columns.
#'
#' @return A summarised table (same high-level class as the input: data.table /
#'   data.frame / tibble) with one row per group and one column per summary.
#'
#' @examples
#' \donttest{
#' dt <- data.table::as.data.table(mtcars)
#'
#' # Group-wise sums
#' summarise_group_stats(dt, .(cyl, vs), value_var = .(hp, drat), fun = sum)
#'
#' # Group-wise means with custom names
#' summarise_group_stats(dt, cyl, value_var = .(mpg, wt), fun = mean,
#'                       col_names = c("mpg_mean", "wt_mean"))
#'
#' # Overall summary (no groups)
#' summarise_group_stats(dt, value_var = .(mpg, hp), fun = max)
#'
#' # Different summary per column (elementwise mapping)
#' summarise_group_stats(dt,
#'                       group_var = cyl,
#'                       value_var = .(mpg, hp, wt),
#'                       fun = list(mean, sum, max),
#'                       col_names = c("mpg_mean", "hp_sum", "wt_max"))
#' }
#'
#' @export
summarise_group_stats <- function(x,
                                  group_var,
                                  value_var,
                                  fun = sum,
                                  col_names = NULL,
                                  prefix = "",
                                  suffix = "") {
  lifecycle::signal_stage("experimental", "summarise_group_stats()")

  # Ensure data.table backend but do NOT mutate the user's object
  env <- ensure_dt_env(x)  # returns list(dt, restore, inplace)
  dt  <- env$dt

  # Resolve columns (supports NSE or character vectors)
  vals <- capture_names(dt, !!rlang::enquo(value_var))
  grps <- if (!missing(group_var)) {
    capture_names(dt, !!rlang::enquo(group_var))
  } else {
    character(0)
  }

  # Validate fun
  funs <- if (rlang::is_function(fun)) {
    rep(list(fun), length(vals))
  } else if (is.character(fun)) {
    lapply(fun, match.fun)
  } else {
    fun
  }
  if (length(funs) == 1L)
    funs <- rep(funs, length(vals))
  if (length(funs) != length(vals))
    stop("Length of `fun` must be 1 or equal to length of `value_var`.",
         call. = FALSE)

  # Determine output names
  out_names <- if (!is.null(col_names)) {
    if (length(col_names) != length(vals)) {
      stop("`col_names` must have the same length as `value_var`.", call. = FALSE)
    }
    col_names
  } else {
    paste0(prefix, vals, suffix)
  }

  # Compute summary (by = if groups present, else overall)
  j_expr <- quote({
    res <- Map(function(j, f) f(.SD[[j]]), seq_along(vals), funs)
    as.list(res)
  })

  if (length(grps)) {
    res <- dt[, eval(j_expr), by = grps, .SDcols = vals]
  } else {
    res <- dt[, eval(j_expr), .SDcols = vals] # no group
  }

  # Rename summary columns
  idx_names <- seq(from = length(grps) + 1L, length.out = length(vals))
  data.table::setnames(res, old = names(res)[idx_names], new = out_names)

  # Restore to original class of `x`
  env$restore(res)
}

#' Most frequent value (mode, modal value)
#'
#' Returns the most frequently occurring value (the statistical mode) in a vector.
#'
#' @param x A vector.
#' @param na.rm Logical. Should missing values (`NA`) be removed? Defaults to FALSE.
#'
#' @return The most frequent value vector and its frequency
#'
#' @examples
#' \donttest{
#' # Get the most frequent values
#' x <- c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
#' mostfreq(x)
#' }
#'
#' @export
mostfreq <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  if (inherits(x, "character"))
    x <- x[x != ""]
  if (inherits(x, "Date"))
    x <- as.character(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#' Frequency and proportion table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' For a given vector, compute the absolute frequency (`n`) and relative
#' proportion (`prop`) of each unique value. Returns the result as a
#' data.table.
#'
#' @param x A vector (numeric, character, factor, or logical).
#'
#' @return A data.table with columns:
#' \describe{
#'   \item{value}{Unique values of `x`}
#'   \item{n}{Absolute frequency (count)}
#'   \item{prop}{Relative frequency (proportion, sum to 1)}
#' }
#'
#' @examples
#' \donttest{
#' # Numeric input
#' x <- sample(1:5, 20, replace = TRUE)
#' freq_table(x)
#'
#' # Character input
#' y <- sample(letters[1:3], 10, replace = TRUE)
#' freq_table(y)
#'
#' # Factor input
#' z <- factor(c("low","medium","high","low"))
#' freq_table(z)
#' }
#'
#' @export
freq_table <- function(x) {
  lifecycle::signal_stage("experimental", "freq_table()")
  op <- options(scipen = 14L)
  on.exit(options(op), add = TRUE)

  freq <- table(x)
  prop <- prop.table(freq)

  df <- as.data.table(freq)
  dp <- as.data.table(prop)

  data.table::setnames(df, c("value", "n"))
  data.table::setnames(dp, c("value", "prop"))

  value <- NULL
  df[dp, on = .(value)]
}


# Deprecated functions ----------------------------------------------------

#' Deprecated: set_stat_by()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [add_group_stats()] instead.
#'
#' @param ... Additional arguments passed to [add_group_stats()].
#'
#' @return Same return value as [add_group_stats()].
#'
#' @seealso [add_group_stats()]
#'
#' @export
set_stat_by <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9001", "set_stat_by()", "add_group_stats()")
  add_group_stats(...)
}

#' Deprecated: get_stat_by()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [summarise_group_stats()] instead.
#'
#' @param ... Additional arguments passed to [summarise_group_stats()].
#'
#' @return Same return value as [summarise_group_stats()].
#'
#' @seealso [summarise_group_stats()]
#'
#' @export
get_stat_by <- function(...) {
  lifecycle::deprecate_warn("0.0.0.9001", "get_stat_by()", "summarise_group_stats()")
  summarise_group_stats(...)
}
