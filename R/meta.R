#' Extract meta information about an object
#'
#' `r lifecycle::badge("experimental")`
#'
#' Generic function to display meta information about an object.
#' Currently supports data.frame objects with a specific method.
#'
#' @param x An R object (e.g., data.frame).
#'
#' @return A meta object summarizing information (see [meta.data.frame()]).
#'
#' @examples
#' \donttest{meta(mtcars)}
#'
#' @export
meta <- function(x) {
  lifecycle::signal_stage("experimental", "meta()")
  UseMethod("meta")
}

#' Meta information for a data.frame
#'
#' Summarize the structure and contents of a data.frame at the column level.
#'
#' The returned table includes:
#' * `column`: column name
#' * `class`: column class
#' * `type`: underlying storage mode
#' * `n`: number of non-missing values
#' * `missing`: number of missing values
#' * `zero`: number of zero values
#' * `distinct`: number of distinct values
#' * `prop`: proportion of non-missing values (`1 - missing/nrows`)
#' * `nzprop`: proportion of non-zero values (`1 - zero/nrows`)
#' * `mode`: most frequent value
#'
#' @param x A data.frame.
#'
#' @return A meta object, which is a `data.table` containing
#'   column-level summary information.
#'
#' @method meta data.frame
#' @export
meta.data.frame <- function(x) {
  column <- names(x)
  class <- sapply(x, class)
  if (is.list(class))
    class <- sapply(class, function(x) paste(x, collapse = ","))
  type <- sapply(x, typeof)
  nrows <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  zero <- sapply(x, function(x) sum(x == 0, na.rm = TRUE))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, function(s) mostfreq(s, na.rm = TRUE))
  df <- data.table(column, class, type, n, missing, zero, distinct,
                   prop = 1 - missing/nrows, nzprop = 1 - zero/nrows, mode)
  data.table::setattr(df, "nrow", nrows)
  data.table::setattr(df, "ncol", ncol(x))
  data.table::setattr(df, "nunique", nrow(unique(x)))
  data.table::setattr(df, "class", c("meta", class(df)))
  df
}

#' Inspect class and storage type
#'
#' Generic function to show class and underlying storage type of objects.
#' Currently provides a method for data.frame.
#'
#' @param x An R object (e.g., data.frame).
#'
#' @return An R object summarizing class and storage type.
#'
#' @examples
#' \donttest{type(cars)}
#'
#' @export
type <- function(x) UseMethod("type")

#' Class and type information for data frames
#'
#' Summarize class and storage type of each column in a data frame.
#'
#' @param x A data.frame.
#'
#' @return A data.table with columns:
#' * `column`: column name
#' * `class`: column class
#' * `type`: storage type
#'
#' @method type data.frame
#' @export
type.data.frame <- function(x) {
  column <- names(x)
  class <- sapply(x, class)
  type <- sapply(x, typeof)
  data.table::data.table(column, class, type)
}
