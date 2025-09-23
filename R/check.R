#' Assert object class
#'
#' Verifies that an object inherits from at least one of the expected classes.
#' On failure, throws an error with details about the actual class.
#'
#' @param x An R object to check.
#' @param class A non-empty character vector of acceptable class names.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' assert_class(mtcars, "data.frame")
#' assert_class(mtcars, "character") # errors
#' }
#'
#' @export
assert_class <- function(x, class) {
  if (!is.character(class) || length(class) < 1L || anyNA(class)) {
    rlang::abort(
      message = "`class` must be a non-empty character vector without NA.",
      class   = c("jaid_error_wrong_class", "jaid_error", "error"),
      arg     = "class"
    )
  }

  x_name <- if (is.character(x) && length(x) == 1L) {
    sprintf('"%s"', x)  # literal string
  } else {trace_arg_expr(x, verbose = FALSE, skip_shiny = TRUE)
  }

  if (inherits(x, class)) return(invisible(TRUE))

  rlang::abort(
    message = sprintf("`%s` must be <%s>, not <%s>.",
                      x_name,
                      paste(class, collapse = " | "),
                      paste(class(x), collapse = " / ")),
    class   = c("jaid_error_wrong_class", "jaid_error", "error"),
    object  = x_name,
    actual  = class(x),
    expect  = class
  )
}

#' Assert nonzero length
#'
#' Verifies that an object has nonzero length. On failure, throws an error.
#'
#' @param x An object of class `logical`, `integer`, `numeric`, `character`,
#'   `complex`, `raw`, `Date`, `POSIXt`, `factor`, or `ordered`.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' assert_length(1:3)
#' assert_length(numeric()) # errors
#' }
#'
#' @seealso [rlang::has_length()]
#' @export
assert_length <- function(x) {
  assert_class(
    x,
    c("logical", "integer", "numeric", "character", "complex", "raw",
      "Date", "POSIXt", "factor", "ordered")
  )

  x_name <- trace_arg_expr(x, verbose = FALSE, skip_shiny = TRUE)

  if (rlang::has_length(x)) return(invisible(TRUE))

  rlang::abort(
    message = sprintf("`%s` must have length > 0.", x_name),
    class   = c("jaid_error_zero_length", "jaid_error", "error"),
    object  = x_name,
    length  = length(x),
    classof = class(x)
  )
}

#' Assert that a data frame has specific columns
#'
#' Verifies that a data frame contains **all** of the specified columns.
#' On failure, throws an error listing the missing columns.
#'
#' @param df A data.frame.
#' @param cols A character vector of column names to require.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' assert_cols(mtcars, c("mpg", "cyl"))
#' assert_cols(mtcars, c("mpg2", "cyl2")) # errors
#' }
#'
#' @export
assert_cols <- function(df, cols) {
  assert_class(df, "data.frame")

  if (!is.character(cols) || length(cols) < 1L || anyNA(cols)) {
    rlang::abort(
      message = "`cols` must be a non-empty character vector without NA.",
      class   = c("jaid_error_invalid_argument", "jaid_error", "error"),
      arg     = "cols"
    )
  }

  df_name <- trace_arg_expr(df, verbose = FALSE, skip_shiny = TRUE)
  missing <- setdiff(cols, names(df))
  if (length(missing) == 0L) return(invisible(TRUE))

  rlang::abort(
    message = sprintf("`%s` is missing column(s): %s",
                      df_name, paste(missing, collapse = ", ")),
    class   = c("jaid_error_missing_columns", "jaid_error", "error"),
    df      = df_name,
    missing = missing,
    columns = names(df)
  )
}

#' Assert that a data frame has rows
#'
#' Verifies that a data frame has at least one row. On failure, throws an error.
#'
#' @param df A data.frame.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' assert_rows(data.frame(1))
#' assert_rows(data.frame()) # error
#' }
#'
#' @export
assert_rows <- function(df) {
  assert_class(df, "data.frame")

  if (nrow(df) > 0L) return(invisible(TRUE))

  df_name <- trace_arg_expr(df, verbose = FALSE, skip_shiny = TRUE)
  rlang::abort(
    message = sprintf("`%s` has no rows.", df_name),
    class   = c("jaid_error_no_rows", "jaid_error", "error"),
    df      = df_name,
    nrows   = 0L,
    columns = names(df)
  )
}

#' Assert that an object has specific attributes
#'
#' Verifies that an object has **all** of the specified attributes.
#' On failure, throws an error listing the missing attributes.
#'
#' @param obj An R object.
#' @param attrs A non-empty character vector of attribute names to require.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' assert_attrs(mtcars, c("names", "class"))
#'
#' @export
assert_attrs <- function(obj, attrs) {
  if (!is.character(attrs) || length(attrs) < 1L || anyNA(attrs)) {
    rlang::abort(
      message = "`attrs` must be a non-empty character vector without NA.",
      class   = c("jaid_error_invalid_argument", "jaid_error", "error"),
      arg     = "attrs"
    )
  }

  obj_name <- trace_arg_expr(obj, verbose = FALSE, skip_shiny = TRUE)
  have     <- names(attributes(obj))
  missing  <- setdiff(attrs, have)

  if (length(missing) == 0L) return(invisible(TRUE))

  rlang::abort(
    message = sprintf("`%s` is missing attribute(s): %s",
                      obj_name, paste(missing, collapse = ", ")),
    class      = c("jaid_error_missing_attributes", "jaid_error", "error"),
    object     = obj_name,
    missing    = missing,
    attributes = have
  )
}

#' Check if a data frame has specific columns
#'
#' Returns `TRUE` if the data frame contains **all** of the specified columns,
#' otherwise `FALSE`. Unlike [assert_cols()], this never throws an error.
#'
#' @param df A data.frame.
#' @param cols A character vector of column names to check.
#'
#' @return Logical scalar: `TRUE` if all columns are present, otherwise `FALSE`.
#'
#' @examples
#' has_cols(mtcars, c("mpg", "cyl"))     # TRUE
#' has_cols(mtcars, c("mpg2", "cyl2"))   # FALSE
#'
#' @seealso [assert_cols()]
#' @export
has_cols <- function(df, cols) {
  assert_class(df, "data.frame")

  if (!is.character(cols) || length(cols) < 1L || anyNA(cols)) {
    return(FALSE)
  }

  length(setdiff(cols, names(df))) == 0L
}

#' Check if a data frame has rows
#'
#' Returns `TRUE` if the data frame has at least one row, otherwise `FALSE`.
#' Unlike [assert_rows()], this never throws an error.
#'
#' @param df A data.frame.
#'
#' @return Logical scalar: `TRUE` if the data frame has rows, otherwise `FALSE`.
#'
#' @examples
#' has_rows(data.frame(1))  # TRUE
#' has_rows(data.frame())   # FALSE
#'
#' @seealso [assert_rows()]
#' @export
has_rows <- function(df) {
  assert_class(df, "data.frame")
  nrow(df) > 0L
}

#' Check if an object has specific attributes
#'
#' Returns `TRUE` if an object has **all** of the specified attributes,
#' otherwise `FALSE`. Unlike [assert_attrs()], this never throws an error.
#'
#' @param obj An R object.
#' @param attrs A character vector of attribute names to check.
#'
#' @return Logical scalar: `TRUE` if all attributes are present, otherwise `FALSE`.
#'
#' @examples
#' has_attrs(mtcars, c("names", "class"))    # TRUE
#' has_attrs(mtcars, c("foo", "bar"))        # FALSE
#'
#' @seealso [assert_attrs()]
#' @export
has_attrs <- function(obj, attrs) {
  if (!is.character(attrs) || length(attrs) < 1L || anyNA(attrs)) {
    return(FALSE)
  }
  length(setdiff(attrs, names(attributes(obj)))) == 0L
}

#' Check Column Specification Against a Data Frame
#'
#' Validates whether the columns in a given data frame match an expected
#' column specification. The specification includes expected column names
#' and their corresponding classes. The function returns a data.table
#' with the actual and expected classes, along with a status indicating
#' whether each column matches, is missing, or is extra.
#'
#' @param df A data.frame or data.table containing the data to be checked.
#' @param col_spec A named list defining the expected specification.
#'   Each name corresponds to a column, and each value is the expected class.
#'
#' @return A data.table with the following columns:
#'   \itemize{
#'     \item `column`: Column name
#'     \item `actual`: Actual class of the column (NA if missing)
#'     \item `expected`: Expected class of the column (NA if not specified)
#'     \item `status`: Comparison result: "match", "mismatch", "missing", or "extra"
#'     \item `sample`: Example value from the first row of the column
#'       (NA if column is missing)
#'   }
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie"),
#'   age = c(25, 30, 28),
#'   paid = c(TRUE, FALSE, TRUE)
#' )
#'
#' col_spec <- list(
#'   id = "character",
#'   name = "character",
#'   age = "integer",
#'   premium = "numeric"
#' )
#'
#' check_col_spec(df, col_spec)
#' }
#'
#' @export
check_col_spec <- function(df, col_spec) {

  assert_class(df, "data.frame")

  if (!is.list(col_spec) || is.null(names(col_spec)) || any(names(col_spec) == ""))
    stop("col_spec must be a named list.")
  if (!all(sapply(col_spec, is.character)))
    stop("All elements of col_spec must be character strings (expected classes).")

  cols_act <- names(df)
  cols_exp <- names(col_spec)
  actual <- vapply(df, function(x) class(x)[1L], character(1L))

  # actual classes
  df_act <- data.frame(
    column = names(actual), actual = actual,
    stringsAsFactors = FALSE
  )

  # expected classes
  df_exp <- data.frame(
    column = cols_exp, expected = unlist(col_spec, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  # full join by column
  merged <- merge(df_act, df_exp, by = "column", all = TRUE)
  cols_ord <- c(cols_exp, setdiff(cols_act, cols_exp))
  ord <- match(cols_ord, merged$column)
  ord <- ord[!is.na(ord)]
  merged <- merged[ord,]
  rownames(merged) <- NULL

  # status
  merged$status <- ifelse(
    is.na(merged$actual), "missing",
    ifelse(is.na(merged$expected), "extra",
           ifelse(merged$actual == merged$expected, "match", "mismatch"))
  )

  # compatibility note
  merged$note <- ifelse(
    merged$status == "mismatch" & (
      (merged$actual == "integer" & merged$expected == "numeric") |
        (merged$actual == "numeric" & merged$expected == "integer")
    ),
    "compatible", NA_character_
  )

  # add first-row sample values
  first_row <- df[1, , drop = FALSE]
  merged$sample <- vapply(merged$column, function(col) {
    if (col %in% names(first_row)) {
      as.character(first_row[[col]])  # simpler: take first-row value as string
    } else {
      NA_character_
    }
  }, character(1L))

  # Console summary
  cat(cli::col_cyan(cli::rule("Column check summary", line = 2)), "\n")
  for (stat in c("match", "mismatch", "missing", "extra")) {
    sub <- merged[merged$status == stat, ]
    if (nrow(sub) == 0) next
    msg_str <- switch(
      stat,
      match = paste(sub$column, collapse = ", "),
      mismatch = paste0(
        sub$column, " (", sub$actual, " \u2192 ", sub$expected,
        ifelse(!is.na(sub$note), paste0(": ", sub$note), ""), ")",
        collapse = ", "
      ),
      missing = paste(sub$column, collapse = ", "),
      extra   = paste(sub$column, collapse = ", ")
    )
    color_msg <- switch(
      stat,
      match    = cli::col_green(msg_str),
      mismatch = cli::col_red(msg_str),
      missing  = cli::col_yellow(msg_str),
      extra    = cli::col_cyan(msg_str)
    )
    icon <- switch(stat, match = "o", mismatch = "x", missing = "-", extra = "+")
    cli::cli_alert("{.strong {icon} {stat}:} {color_msg}")
  }
  cli::cli_text("")

  merged
}

#' Check equality of columns between two data frames
#'
#' Validates that two data frames have the same shape (same columns and row count),
#' then compares each column to determine whether all values match. The result is
#' a named logical vector, one element per column.
#'
#' If the number of rows, number of columns, or column names differ, an error is raised.
#' For value comparison, pairs of missing values (`NA` vs `NA`) are treated as equal.
#'
#' @param x,y Two data.frame objects.
#'
#' @return A named logical vector of length `ncol(x)`, where each element indicates
#'   whether all values in the corresponding column are equal across the two data frames.
#'
#' @examples
#' \donttest{
#' # All columns equal
#' check_col_equal(mtcars, mtcars)
#'
#' # One column differs
#' df1 <- head(mtcars)
#' df2 <- df1; df2$cyl[1] <- df2$cyl[1] + 1
#' check_col_equal(df1, df2)
#'
#' # NA handling: NA vs NA is considered equal
#' a <- data.frame(x = c(1, NA, 3))
#' b <- data.frame(x = c(1, NA, 3))
#' check_col_equal(a, b)  # TRUE for column x
#' }
#'
#' @export
check_col_equal <- function(x, y) {
  assert_class(x, "data.frame")
  assert_class(y, "data.frame")

  x_name <- trace_arg_expr(x)
  y_name <- trace_arg_expr(y)

  x_cols <- colnames(x); x_nrow <- nrow(x); x_ncol <- ncol(x)
  y_cols <- colnames(y); y_nrow <- nrow(y); y_ncol <- ncol(y)

  if (length(x_cols) != length(y_cols)) {
    stop(sprintf("Different number of cols. (%s: %s, %s: %s)",
                 x_name, x_ncol, y_name, y_ncol), call. = FALSE)
  } else {
    if (!identical(sort(x_cols), sort(y_cols))) {
      stop(sprintf("Different column names.\n%s: %s\n%s: %s",
                   x_name, paste(x_cols, collapse = ", "),
                   y_name, paste(y_cols, collapse = ", ")), call. = FALSE)
    }
  }

  if (x_nrow != y_nrow)
    stop(sprintf("Different number of rows. (%s: %s, %s: %s).",
                 x_name, x_nrow, y_name, y_nrow))

  # column-wise comparison
  rt <- vapply(x_cols, function(s) .col_equal(x[[s]], y[[s]]), logical(1L))
  names(rt) <- x_cols

  rt
}


# Internal helper functions -----------------------------------------------

.col_equal <- function(a, b) {
  if (length(a) != length(b))
    return(FALSE)
  if (is.list(a) || is.list(b)) {
    all(mapply(function(u, v) {
      (isTRUE(is.na(u)) && isTRUE(is.na(v))) || identical(u, v)
    }, a, b))
  } else {
    eq <- a == b
    same_na <- is.na(a) & is.na(b)
    eq[same_na] <- TRUE
    eq[is.na(eq)] <- FALSE
    all(eq)
  }
}
