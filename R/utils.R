#' @title devars
#'
#' @description
#' This function operates like `deparse(substitute(x))` inside the functions.
#'
#' @param x A string, vector or list expression that can be a string vector
#' @return A string vector
#'
#' @examples
#' # deparse(substitute(x))
#' \dontrun{devars(expression)
#' devars(c(expression, string))
#' devars(list(expression, string))
#' devars(.(expression, string))}
devars <- function(x) {
  if (identical(parent.frame(), globalenv()))
    n <- sys.nframe()
  else n <- 1L
  x <- eval(substitute(substitute(x)), envir = parent.frame(n = max(n, 1L)))
  if (length(x) == 1L)
    return(deparse(x))
  return(vapply(x, deparse, "character")[-1L])
}

match_cols <- function(df, cols) colnames(df)[match(cols, colnames(df), 0L)]

has_rows <- function(df) {
  df_name <- deparse(substitute(df))
  if (!nrow(df)) {
    stop("'", df_name, "' doesn't have row(s): ",
         call. = FALSE)
  }
}

#' Has columns
#'
#' Whether the data has specific columns
#'
#' @param df a data frame
#' @param cols column names
#' @param error_raise a boolean whether to raise an error or not
#' @return a boolean value
#'
#' @examples
#' # has columns
#' \donttest{has_cols(mtcars, c("cyl", "disp"))}
#'
#' # raise an error
#' \dontrun{
#' has_cols(mtcars, c("cyl", "iris"), error_raise = TRUE)}
#'
#' @export
has_cols <- function(df, cols, error_raise = FALSE) {
  df_name <- deparse(substitute(df))
  df_cols <- colnames(df)
  diff_cols <- setdiff(cols, df_cols)
  rt <- length(diff_cols) == 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have column(s): ",
         paste0(diff_cols, collapse = ", "), ".",
         call. = FALSE)
  }
}

has_missing <- function(x) {
  column_name <- deparse(substitute(x))
  if (any(is.na(x))) {
    stop("'", column_name, "' has missing value(s): ",
         call. = FALSE)
  }
}

sort_group_by <- function(x) {
  .Call(SortGroupBy, x)
}

#' Paste vectors of a list
#'
#' Paste vectors of equal length in a list or data.frame
#'
#' @param x a list with same length vectors or data frame column vectors you want to paste.
#' @param sep a character string to separate the terms.
#' @return a vector pasted
#'
#' @examples
#' # paste length and width of iris
#' iris$size <- paste_list(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
#' head(iris)
#'
#' @export
paste_list <- function(x, sep = "|") {
  n <- length(x)
  if (n == 1L) {
    return(x[[1L]])
  } else {
    return(do.call(function(...) paste(..., sep = sep), x))
  }
}

#' Set data.frame to data.table
#'
#' Set data.frame to data.table class.
#'
#' @param x data.frame
#' @return No return value.
#'
#' @examples
#' # set data.frame to data.table
#' \donttest{set_dt(iris)}
#'
#' @export
set_dt <- function(x) {
  assert_class(x, "data.frame")
  if (!inherits(x, "data.table"))
    data.table::setattr(x, "class", c("data.table", "data.frame"))
  invisible(x)
}

#' Set data frame to tibble
#'
#' Set data frame to tibble class.
#'
#' @param x data.frame
#' @return No return value.
#'
#' @examples
#' # set data.frame to tibble
#' \donttest{set_tibble(iris)}
#'
#' @export
set_tibble <- function(x) {
  assert_class(x, "data.frame")
  if (!inherits(x, "tbl_df"))
    data.table::setattr(x, "class", c("tbl_df", "tbl", "data.frame"))
  invisible(x)
}
