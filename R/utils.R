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
    deparse()
}

desubs <- function(x) {
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

#' Assert class
#'
#' Assert object class.
#'
#' @param obj an object
#' @param class an object class
#' @return no return
#'
#' @examples
#' # assert object class
#' \donttest{assert_class(cars, "data.frame")}
#'
#' @export
assert_class <- function(obj, class) {
  obj_name <- desub(obj)
  if (!inherits(obj, class)) {
    stop(obj_name, " is not an object of class: '",
         paste(class, collapse = ", "), "'",
         call. = FALSE)
  }
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

#' Change columns from uppercase to lowercase or from lowercase to uppercase
#'
#' Change columns from uppercase to lowercase or from lowercase to uppercase
#'
#' @param df a data.frame
#' @return no return values
#'
#' @examples
#' # Change columns case
#' \donttest{df <- mtcars
#' set_col_upper(df)
#' set_col_lower(df)}
#'
#' @export
set_col_lower <- function(df)
  data.table::setnames(df, colnames(df), tolower(colnames(df)))

#' @rdname set_col_lower
#' @export
set_col_upper <- function(df)
  data.table::setnames(df, colnames(df), toupper(colnames(df)))

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

#' Get a copied data.table
#'
#' Get a copied data.table.
#'
#' @param df a data.frame
#' @return a copied data.table
#'
#' @examples
#' # get copied data.table
#' \donttest{df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' get_copied_dt(df)}
#'
#' @export
get_copied_dt <- function(df)
  return(data.table::setDT(data.table::copy(df))[])

#' Set data.table function
#'
#' setDT function re-exported from `data.table`.
#'
#' @param df a data.frame
#' @return no return values.
#'
#' @seealso [setDT()]
#'
#' @export
set_dt <- function(df) {
  assert_class(df, "data.frame")
  if (!has_ptr(df, error_raise = FALSE)) {
    n <- sys.nframe()
    df_name <- desub(df)
    old_class <- class(df)
    data.table::setDT(df)
    assign(df_name, df, envir = parent.frame(n))
    invisible()
  }
  if (!inherits(df, "data.table")) {
    data.table::setattr(df, "class", c("data.table", "data.frame"))
  }
}

#' Set tibble function
#'
#' as_tibble function re-exported from `tibble`.
#'
#' @param df a data.frame
#' @return no return values.
#'
#' @seealso [as_tibble()]
#'
#' @export
set_tibble <- function(df) {
  assert_class(df, "data.frame")
  if (!has_ptr(df, error_raise = FALSE)) {
    n <- sys.nframe()
    df_name <- desub(df)
    old_class <- class(df)
    data.table::setDT(df)
    data.table::setattr(df, "class", c("tbl_df", "tbl", "data.frame"))
    assign(df_name, df, envir = parent.frame(n))
    invisible()
  }
  if (!inherits(df, "tbl_df")) {
    data.table::setattr(df, "class", c("tbl_df", "tbl", "data.frame"))
  }
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

sort_group_by <- function(x) {
  .Call(SortGroupBy, x)
}
