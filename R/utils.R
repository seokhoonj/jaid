#' @title Desub
#'
#' @description
#' This function operates like `deparse(substitute(x))` inside the functions.
#'
#' @param x an expression that can be a string vector
#' @return a string vector
#'
#' @examples
#' # desub
#' \donttest{f1 <- function(a) desub(a)
#' f2 <- function(b) f1(b)
#' f3 <- function(c) f2(c)
#' f4 <- function(d) f3(d)
#' f5 <- function(e) f4(e)
#' desub(iris) # iris
#' f1(iris) # iris
#' f2(iris) # iris
#' f3(iris) # iris
#' f4(iris) # iris
#' f5(iris) # iris}
#'
#' @export
desub <- function(x) {
  substitute(x) |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    substitute() |>
    eval(envir = parent.frame(n =  1)) |>
    eval(envir = parent.frame(n =  2)) |>
    eval(envir = parent.frame(n =  3)) |>
    eval(envir = parent.frame(n =  4)) |>
    eval(envir = parent.frame(n =  5)) |>
    eval(envir = parent.frame(n =  6)) |>
    eval(envir = parent.frame(n =  7)) |>
    eval(envir = parent.frame(n =  8)) |>
    eval(envir = parent.frame(n =  9)) |>
    eval(envir = parent.frame(n = 10)) |>
    eval(envir = parent.frame(n = 11)) |>
    eval(envir = parent.frame(n = 12)) |>
    eval(envir = parent.frame(n = 13)) |>
    vapply(FUN = deparse, FUN.VALUE = "character")
}

#' Match columns
#'
#' Get matched columns from a data frame.
#'
#' @param df a data frame
#' @param cols a string vector specifying columns
#' @return a string vector
#'
#' @examples
#' # match columns
#' \donttest{df <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"), z = c(4, 5, 6))
#' match_cols(df, c("x", "z"))}
#'
#' @export
match_cols <- function(df, cols) {
  assert_class(df, "data.frame")
  colnames(df)[match(cols, colnames(df), 0L)]
}

#' Find columns using regular expression pattern
#'
#' Find columns using regular expression pattern
#'
#' @param df a data frame
#' @param pattern a string vector specifying columns
#' @return a string vector
#'
#' @examples
#' # find columns using regular expression pattern
#' \donttest{df <- data.frame(col_a = c(1, 2, 3), col_b = c("A", "B", "C"), col_c = c(4, 5, 6))
#' regex_cols(df, pattern = c("a|c"))}
#'
#' @export
regex_cols <- function(df, pattern) {
  assert_class(df, "data.frame")
  colnames(df)[grepl(pattern, names(df), perl = TRUE)]
}

#' Has rows
#'
#' Whether the data has rows
#'
#' @param df a data frame
#' @param error_raise a logcial whether to raise an error or not
#' @return a boolean value
#'
#' @examples
#' # has rows
#' \dontrun{
#' df <- data.frame()
#' has_rows(df)}
#'
#' # raise an error
#' \dontrun{
#' df <- data.frame()
#' has_rows(df, error_raise = TRUE)}
#'
#' @export
has_rows <- function(df, error_raise = TRUE) {
  assert_class(df, "data.frame")
  df_name <- desub(df)
  nrows <- nrow(df)
  rt <- nrows != 0
  if (!error_raise)
    return(rt)
  if (!rt) {
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
has_cols <- function(df, cols, error_raise = TRUE) {
  assert_class(df, "data.frame")
  df_name <- desub(df)
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

#' Has attributes
#'
#' Whether the data has specific attributes
#'
#' @param df a data frame
#' @param attr attribute names
#' @param error_raise a boolean whether to raise an error or not
#' @return a boolean value
#'
#' @examples
#' # has attributes
#' \donttest{has_attr(mtcars, c("names", "class"))}
#'
#' # raise an error
#' \dontrun{
#' has_attr(mtcars, c("names", "types"), error_raise = TRUE)}
#'
#' @export
has_attr <- function(df, attr, error_raise = TRUE) {
  df_name <- desub(df)
  df_attr <- names(attributes(df))
  diff_attr <- setdiff(attr, df_attr)
  rt <- length(diff_attr) == 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have attributes(s): ",
         paste0(diff_attr, collapse = ", "), ".",
         call. = FALSE)
  }
}

has_missing <- function(x) {
  col_nm <- desub(x)
  if (any(is.na(x))) {
    stop("'", col_nm, "' has missing value(s): ",
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

#' Set attributes
#'
#' setattr functions re-exported from `data.table`.
#'
#' @param x any objects; e.g, list, columns of a data.frame or data.table
#' @param name the character attribute name.
#' @param value the value to assign to the attribute or `NULL` removes teh attribute, if present.
#' @return the changed object (invisibly) for use in compound statements.
#'
#' @examples
#' # set attributes
#' \dontrun{df <- data.frame(a = 1:3, b = 4:6)
#' set_attr(df, "flag", TRUE)
#' attr(df, "flag")}
#'
#' @export
set_attr <- function(x, name, value)
  data.table::setattr(x, name, value)

#' Change columns from uppercase to lowercase or from lowercase to uppercase
#'
#' Change columns from uppercase to lowercase or from lowercase to uppercase
#'
#' @param x a data.frame
#' @return no return values
#'
#' @examples
#' # Change columns case
#' \donttest{df <- mtcars
#' set_col_upper(df)
#' set_col_lower(df)}
#'
#' @export
set_col_lower <- function(x)
  data.table::setnames(x, colnames(x), tolower(colnames(x)))

#' @rdname set_col_lower
#' @export
set_col_upper <- function(x)
  data.table::setnames(x, colnames(x), toupper(colnames(x)))

#' Set labels
#'
#' Set column labels for a data frame.
#'
#' @param df a data.frame
#' @param labels a string vector specifying labels to describe columns
#' @param cols a string vector specifying columns
#'
#' @examples
#' # set labels
#' \dontrun{df <- data.frame(Q1 = c(0, 1, 1), Q2 = c(1, 0, 1))
#' set_labels(df, labels = c("Rainy?", "Umbrella?"))
#' View(df)}
#'
#' @export
set_labels <- function(df, labels, cols) {
  if (missing(cols))
    cols <- names(df)
  if (length(cols) != length(labels))
    stop("the length of columns and the length of labels are different.")
  lapply(seq_along(cols),
         function(x) data.table::setattr(df[[cols[[x]]]], "label", labels[[x]]))
  invisible(df)
}

#' Set external pointer
#'
#' Set external pointer
#'
#' @param x a data.frame
#' @return No return value.
#'
#' @examples
#' # set pointer
#' \donttest{set_ptr(iris)}
#'
#' @export
set_ptr <- function(x) {
  if (!has_ptr(x, error_raise = FALSE)) {
    n <- sys.nframe()
    x_name <- desub(x)
    old_class <- class(x)
    data.table::setalloccol(x)
    set_attr(x, "class", old_class)
    assign(x_name, x, envir = parent.frame(n))
  }
}

#' Set data.table function
#'
#' setDT function re-exported from `data.table`.
#'
#' @param x a data.frame
#' @return no return values.
#'
#' @seealso [setDT()]
#'
#' @export
set_dt <- function(x) {
  assert_class(x, "data.frame")
  if (!inherits(x, "data.table")) {
    if (!has_ptr(x, error_raise = FALSE)) {
      n <- sys.nframe()
      x_name <- desub(x)
      old_class <- class(x)
      data.table::setDT(x)
      assign(x_name, x, envir = parent.frame(n))
      if (n > 1)
        assign(x_name, x, envir = parent.frame(n-1))
    }
    else {
      set_attr(x, "class", c("data.table", "data.frame"))
    }
  }
}

#' Set tibble function
#'
#' as_tibble function re-exported from `tibble`.
#'
#' @param x a data.frame
#' @return no return values.
#'
#' @seealso [as_tibble()]
#'
#' @export
set_tibble <- function(x) {
  assert_class(x, "data.frame")
  if (!inherits(x, "tbl_df")) {
    if (!has_ptr(x, error_raise = FALSE)) {
      n <- sys.nframe()
      x_name <- desub(x)
      old_class <- class(x)
      x <- tibble::as_tibble(x)
      assign(x_name, x, envir = parent.frame(n))
      if (n > 1)
        assign(x_name, x, envir = parent.frame(n-1))
    }
    else {
      set_attr(x, "class", c("tbl_df", "tbl", "data.frame"))
    }
  }
}

#' Get external pointer
#'
#' Get external pointer
#'
#' @param x a data.frame
#' @return No return value.
#'
#' @examples
#' # get pointer
#' \dontrun{
#' df <- data.frame(x = c(1:3), y = c("a", "b", "c"))
#' get_ptr(df)
#' df <- setalloccol(df)
#' get_ptr(df)}
#'
#' @export
get_ptr <- function(x)
  attr(x, ".internal.selfref")

#' Is a null external pointer?
#'
#' Is a null external pointer?
#'
#' @param pointer an externalptr object
#' @return a logical value whether it is null external pointer or not.
#'
#' @examples
#' # is null external pointer?
#' \donttest{p <- new("externalptr")
#' is.null.externalptr(p)}
#'
#' @export
is.null.externalptr <- function(pointer) {
  stopifnot(methods::is(pointer, "externalptr"))
  .Call(IsNullExternalPtr, pointer)
}

#' Has not a null pointer?
#'
#' Has not a null external pointer?
#'
#' @param x a data.frame
#' @param error_raise a logcial whether to raise an error or not
#' @return a logical value whether to have not a null external pointer or not
#'
#' @examples
#' # Has not null external pointer?
#' \donttest{has_ptr(iris, error_raise = FALSE)}
#'
#' @export
has_ptr <- function(x, error_raise = TRUE) {
  assert_class(x, "data.frame")
  x_name <- desub(x)
  p <- get_ptr(x)
  rt <- TRUE
  if (is.null(p)) {
    rt <- !rt
  }
  else {
    if (is.null.externalptr(p))
      rt <- !rt
  }
  if (!error_raise)
    return(rt)
  if (!rt)
    stop("'", x_name, "'", " doesn't have a pointer.", call. = FALSE)
}

#' Equal columns of two data frames.
#'
#' Whether the columns of two data frames are equal.
#'
#' @param x,y two data frames
#' @return a boolean vector
#'
#' @examples
#' # Are the columns of two data frames equal?
#' \donttest{equal(mtcars, mtcars)}
#'
#' @export
equal <- function(x, y) {
  assert_class(x, "data.frame")
  assert_class(y, "data.frame")
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  x_cols <- colnames(x); x_nrow <- nrow(x); x_ncol <- ncol(x)
  y_cols <- colnames(y); y_nrow <- nrow(y); y_ncol <- ncol(y)
  if (length(x_cols) != length(y_cols)) {
    stop(sprintf("different number of cols. (%s: %s, %s: %s)",
                 x_name, x_ncol, y_name, y_ncol))
  } else {
    if (any(sort(x_cols) != sort(y_cols))) {
      stop(sprintf("different column names.\n%s: %s\n%s: %s",
                   x_name, paste(x_cols, collapse = ", "),
                   y_name, paste(y_cols, collapse = ", ")))
    }
  }
  if (x_nrow != y_nrow)
    stop(sprintf("different number of rows. (%s: %s, %s: %s).",
                 x_name, x_nrow, y_name, y_nrow))
  return(sapply(x_cols, function(s) all(x[[s]] == y[[s]])))
}

