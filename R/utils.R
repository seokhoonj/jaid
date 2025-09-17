#' Assert object class
#'
#' Check that an object inherits from the expected class.
#' Throws an error if the assertion fails.
#'
#' @param x An R object.
#' @param class A character vector of class names to check against.
#'
#' @return No return value. Called for side effects.
#'
#' @examples
#' \donttest{assert_class(cars, "data.frame")}
#'
#' @export
assert_class <- function(x, class) {
  x_name <- if (is.character(x) && length(x) == 1L) {
    sprintf('"%s"', x) # if literal
  } else {
    trace_arg_expr(x, verbose = FALSE, skip_shiny = TRUE)
  }
  if (!inherits(x, class)) {
    stop("'", x_name, "' is not an object of class: '",
         paste(class, collapse = "', '"), "'",
         call. = FALSE)
  }
}

#' Prepend a class to an object
#'
#' Ensures that the specified class (or classes) are placed at the
#' beginning of the object's class vector. If the class already exists,
#' it is moved to the front without duplication.
#'
#' @param x An R object.
#' @param new_class A character vector of class names to prepend.
#'
#' @return The input object `x` with its class attribute updated to have
#'   `new_class` at the front, followed by its original classes (excluding
#'   any duplicates).
#'
#' @examples
#' \donttest{
#' df <- data.frame(a = 1)
#'
#' # Prepend a single class
#' prepend_class(df, "new_class")
#'
#' # Prepend multiple classes
#' prepend_class(df, c("new_class1", "new_class2"))
#' }
#'
#' @export
prepend_class <- function(x, new_class) {
  class(x) <- c(new_class, setdiff(class(x), new_class))
  x
}



#' Get column indices
#'
#' Return the column numbers corresponding to given column names.
#'
#' @param x A data.frame.
#' @param cols A character vector of column names.
#'
#' @return An integer vector of column indices.
#'
#' @examples
#' \donttest{
#' # Column numbers for selected names
#' icol(mtcars, c("disp", "drat", "qsec", "am", "carb"))
#' }
#'
#' @export
icol <- function(x, cols) {
  sapply(unique(cols), function(s) which(colnames(x) == s))
}

#' Match columns
#'
#' Return column names from a data frame that match a specified set.
#'
#' @param df A data.frame.
#' @param cols A character vector of column names.
#'
#' @return A character vector of matched column names (non-matching entries are dropped).
#'
#' @examples
#' \donttest{
#' df <- data.frame(x = c(1, 2, 3), y = c("A", "B", "C"), z = c(4, 5, 6))
#' match_cols(df, c("x", "z"))
#' }
#'
#' @export
match_cols <- function(df, cols) {
  assert_class(df, "data.frame")
  colnames(df)[match(cols, colnames(df), 0L)]
}

#' Find columns by regular expression
#'
#' Return column names that match a regular expression.
#'
#' @param df A data.frame.
#' @param pattern A character string containing a regular expression.
#'
#' @return A character vector of matching column names.
#'
#' @examples
#' \donttest{
#' df <- data.frame(col_a = c(1, 2, 3), col_b = c("A", "B", "C"), col_c = c(4, 5, 6))
#' regex_cols(df, pattern = c("a|c"))
#' }
#'
#' @export
regex_cols <- function(df, pattern) {
  assert_class(df, "data.frame")
  colnames(df)[grepl(pattern, names(df), perl = TRUE)]
}

#' Columns not in a set
#'
#' Return the columns of a data frame that are **not** in a specified list.
#'
#' @param df A data.frame.
#' @param cols A character vector of column names.
#'
#' @return A character vector of column names in `df` but not in `cols`.
#'
#' @examples
#' \donttest{diff_cols(mtcars, c("mpg", "cyl", "disp", "hp", "drat"))}
#'
#' @export
diff_cols <- function(df, cols)
  setdiff(colnames(df), cols)

#' Validate that columns exist
#'
#' Check whether a data frame contains all of the specified columns.
#' Throws an error if any are missing.
#'
#' @param df A data.frame.
#' @param cols A character vector of column names to validate.
#'
#' @return No return value. Called for side effects.
#'
#' @examples
#' \dontrun{valid_cols(mtcars, c("mpg", "cyl", "disp", "hp", "drat"))}
#'
#' @export
valid_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing: ",
         paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
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

  # data.table
  # column <- status <- expected <- note <- NULL
  # act_dt <- data.table::as.data.table(actual, keep.rownames = "column")
  # exp_dt <- data.table::data.table(column = names(col_spec), expected = unlist(col_spec))
  # dt <- data.table::rbindlist(
  #   list(
  #     act_dt[ exp_dt, on = .(column)],
  #     act_dt[!exp_dt, on = .(column)]
  #   ),
  #   fill = TRUE
  # )
  # dt[, status := data.table::fifelse(actual == expected, "match", "mismatch")]
  # dt[is.na(actual), status := "missing"]
  # dt[is.na(expected), status := "extra"]
  # dt[, note := data.table::fifelse(
  #   status == "mismatch" & (
  #     (actual == "integer" & expected == "numeric") |
  #     (actual == "numeric" & expected == "integer")
  #   ), "compatible", NA_character_
  # )]
  # ds <- data.table::data.table(t(head(df, 1)), keep.rownames = "column")
  # dt[ds, on = .(column), `:=`(first, V1)]
  # data.table::setindex(dt, NULL)

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

#' Check if a data frame has rows
#'
#' Test whether a data frame has at least one row. Can return a logical value
#' or raise an error if `error_raise = TRUE`.
#'
#' @param df A data.frame.
#' @param error_raise Logical; if `TRUE`, raise an error if the data frame has no rows.
#'
#' @return A logical scalar (`TRUE` if the data frame has rows, otherwise `FALSE`).
#'
#' @examples
#' \dontrun{
#' df <- data.frame()
#'
#' # Has columns
#' has_rows(df) # FALSE
#'
#' # Raises an error
#' has_rows(df, error_raise = TRUE)
#' }
#'
#' @export
has_rows <- function(df, error_raise = FALSE) {
  assert_class(df, "data.frame")
  df_name <- trace_arg_expr(df)
  nrows <- nrow(df)
  rt <- nrows != 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have any rows: ", call. = FALSE)
  }
  rt
}

#' Check if a data frame has specific columns
#'
#' Test whether a data frame contains all of the specified columns.
#' Can return a logical value or raise an error if `error_raise = TRUE`.
#'
#' @param df A `data.frame`.
#' @param cols A character vector of column names to check.
#' @param error_raise Logical; if `TRUE`, raise an error when columns are missing.
#'
#' @return A logical scalar (`TRUE` if all specified columns exist, otherwise `FALSE`).
#'
#' @examples
#' \dontrun{
#' # Has columns
#' has_cols(mtcars, c("cyl", "disp"))
#'
#' # Raise an error
#' has_cols(mtcars, c("cyl", "iris"), error_raise = TRUE)
#' }
#'
#' @export
has_cols <- function(df, cols, error_raise = FALSE) {
  assert_class(df, "data.frame")
  df_name <- trace_arg_expr(df)
  diff_cols <- setdiff(cols, colnames(df))
  rt <- length(diff_cols) == 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have column(s): ",
         paste0(diff_cols, collapse = ", "), ".",
         call. = FALSE)
  }
  rt
}

#' Check if an object has nonzero length
#'
#' Test whether an object has nonzero length. Can return a logical value
#' or raise an error if `error_raise = TRUE`.
#'
#' @param x An object of class `character`, `integer`, `numeric`, `Date`, or `POSIXt`.
#' @param error_raise Logical; if `TRUE`, raise an error if the object has zero length.
#'
#' @return A logical scalar (`TRUE` if `x` has nonzero length, otherwise `FALSE`).
#'
#' @examples
#' \dontrun{
#' # Has a length
#' has_len(c(numeric(), character()))
#'
#' # Raise an error
#' has_len(c(numeric(), character()), error_raise = TRUE)
#' }
#'
#' @seealso [rlang::has_length()]
#'
#' @export
has_len <- function(x, error_raise = FALSE) {
  assert_class(x, c("character", "integer", "numeric", "Date", "POSIXt"))
  x_name <- trace_arg_expr(x)
  rt <- rlang::has_length(x)
  if (!error_raise)
    return(rt)
  if (!rt)
      stop("'", x_name, "' doesn't have a length.", call. = FALSE)
  rt
}

#' Convert column names to lower or upper case
#'
#' Convenience functions to modify the case of all column names in a data frame.
#' These functions update the names **in place** (using `data.table`).
#'
#' @param df A data.frame.
#'
#' @return The input data frame with column names modified in place.
#'   Called for side effects.
#'
#' @examples
#' \donttest{
#' df <- mtcars
#'
#' # Convert to upper case
#' set_col_upper(df)
#'
#' # Convert to lower case
#' set_col_lower(df)
#'
#' }
#'
#' @export
set_col_lower <- function(df)
  data.table::setnames(df, colnames(df), tolower(colnames(df)))

#' @rdname set_col_lower
#' @export
set_col_upper <- function(df)
  data.table::setnames(df, colnames(df), toupper(colnames(df)))

#' Reorder columns of a data.frame or data.table by reference
#'
#' A convenience wrapper around [data.table::setcolorder()] that allows
#' column reordering with tidy-eval expressions. The function modifies
#' the input object **in place**.
#'
#' @param df A data.frame or data.table.
#' @param neworder Columns to move, specified as bare names inside `.(...)`.
#'   For example, `.(gear, carb)`.
#' @param before,after Optionally, a column (name or position) before/after which
#'   `neworder` should be inserted. Only one of `before` or `after` may be used.
#'
#' @return No return value, called for side effects (the column order of `df` is changed).
#'
#' @examples
#' \dontrun{
#' # With data.frame
#' df1 <- mtcars
#' set_col_order(df1, .(gear, carb), after = mpg)
#'
#' # With data.table
#' df2 <- data.table::as.data.table(mtcars)
#' set_col_order(df2, .(gear, carb), before = am)
#' }
#'
#' @export
set_col_order <- function(df, neworder, before = NULL, after = NULL) {
  # neworder <- match_cols(df, sapply(rlang::enexpr(neworder), rlang::as_name))
  # before <- match_cols(df, sapply(rlang::enexpr(before), rlang::as_name))
  # after  <- match_cols(df, sapply(rlang::enexpr(after), rlang::as_name))
  neworder <- capture_names(df, !!rlang::enquo(neworder))
  before   <- capture_names(df, !!rlang::enquo(before))
  after    <- capture_names(df, !!rlang::enquo(after))
  if (!rlang::has_length(before)) before <- NULL
  if (!rlang::has_length(after)) after <- NULL
  data.table::setcolorder(x = df, neworder = neworder, before = before, after = after)
}

#' Set or get column labels for a data frame
#'
#' Functions to attach or retrieve descriptive labels on columns
#' of a data.frame. Labels are stored as the `"label"` attribute
#' of each column.
#'
#' @param df A data.frame.
#' @param labels A character vector of labels to assign. Must be the same
#'   length as `cols`.
#' @param cols A character vector of column names to set or get labels for.
#'   Defaults to all columns in `df`.
#'
#' @return
#' * `set_labels()` returns the modified data frame, invisibly (labels
#'   are added as side effects).
#' * `get_labels()` returns a character vector of labels corresponding
#'   to the requested columns.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Q1 = c(0, 1, 1), Q2 = c(1, 0, 1))
#'
#' # set labels
#' set_labels(df, labels = c("Rainy?", "Umbrella?"))
#'
#' # get labels
#' get_labels(df)
#'
#' View(df)
#' }
#'
#' @export
set_labels <- function(df, labels, cols) {
  if (missing(cols))
    cols <- names(df)
  if (length(cols) != length(labels))
    stop("The number of columns and the number of labels are different.")
  lapply(seq_along(cols),
         function(x) data.table::setattr(df[[cols[[x]]]], "label", labels[[x]]))
  invisible(df)
}

#' @rdname set_labels
#' @export
get_labels <- function(df, cols) {
  if (missing(cols))
    cols <- names(df)
  sapply(cols, function(x) attr(df[[x]], "label"), USE.NAMES = FALSE)
}

#' Convert a data.frame to data.table (experimental)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A wrapper around [data.table::setDT()] that converts a `data.frame` to a
#' `data.table` **by reference**. This version adds extra safety checks and
#' preserves naming in the calling environment.
#'
#' @param df A data.frame.
#'
#' @return The input `df`, converted to a data.table, returned invisibly.
#'   The object is modified in place (by reference).
#'
#' @seealso [data.table::setDT()]
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3, y = 4:6)
#' set_dt(df)
#' class(df)  # now includes "data.table"
#' }
#'
#' @export
set_dt <- function(df) {
  lifecycle::signal_stage("experimental", "set_dt()")
  assert_class(df, "data.frame")
  if (!has_ptr(df)) {
    n <- sys.nframe()
    df_name <- trace_arg_expr(df)
    old_class <- class(df)
    data.table::setDT(df)
    assign(df_name, df, envir = parent.frame(n))
    invisible(df)
  }
  if (!inherits(df, "data.table")) {
    data.table::setattr(df, "class", c("data.table", "data.frame"))
  }
}

#' Convert a data.frame to tibble (experimental)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A wrapper that converts a `data.frame` to have tibble classes
#' (`tbl_df`, `tbl`, `data.frame`). Internally it ensures reference
#' semantics similar to [data.table::setDT()], then adjusts the
#' class attribute.
#'
#' @param df A data.frame.
#'
#' @return The input `df`, with tibble classes added, returned invisibly.
#'   The object is modified in place (by reference).
#'
#' @seealso [tibble::as_tibble()]
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3, y = 4:6)
#' set_tibble(df)
#' class(df)  # "tbl_df" "tbl" "data.frame"
#' }
#'
#' @export
set_tibble <- function(df) {
  lifecycle::signal_stage("experimental", "set_tibble()")
  assert_class(df, "data.frame")
  if (!has_ptr(df)) {
    n <- sys.nframe()
    df_name <- trace_arg_expr(df)
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
  col_equal <- function(a, b) {
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

  rt <- vapply(x_cols, function(s) col_equal(x[[s]], y[[s]]), logical(1L))
  names(rt) <- x_cols
  rt
}


#' Format numbers with commas
#'
#' Convert a numeric or integer vector into a character vector
#' formatted with commas as thousands separators.
#'
#' @param x A numeric or integer vector.
#'
#' @return A character vector of formatted numbers.
#'
#' @examples
#' # format numbers with commas
#' \donttest{as_comma(c(123456, 234567))}
#'
#' @export
as_comma <- function(x) {
  assert_class(x, c("integer", "numeric"))
  format(round(x), big.mark = ",")
}

#' Paste vector elements with commas
#'
#' Combine elements of a vector into a string with commas.
#' Optionally insert newlines between elements for readability.
#'
#' @param x A vector.
#' @param newline Logical; if `TRUE`, each element is placed on a new line.
#'
#' @return No return value, prints to the console.
#'
#' @examples
#' # paste elements with commas
#' \donttest{paste_comma(names(mtcars))}
#'
#' @export
paste_comma <- function(x, newline = FALSE) {
  if (newline) {
    cat(paste0("c(", paste0("\"", paste(x, collapse = "\"\n, \""), "\""), "\n)"))
  }
  else {
    cat(paste0("c(", paste0("\"", paste(x, collapse = "\", \""), "\""), ")"))
  }
}

#' Quote and paste symbols with commas
#'
#' Capture unquoted symbols, wrap them in quotes, and print them
#' as a comma-separated character vector.
#'
#' @param ... Unquoted variable names or expressions.
#' @param newline Logical; if `TRUE`, print each element on a new line.
#'
#' @return No return value, prints to the console.
#'
#' @examples
#' \donttest{quote_comma(mpg, cyl, disp, hp, drat)} # c("mpg", "cyl", "disp", "hp", "drat")
#'
#' @export
quote_comma <- function(..., newline = FALSE) {
  if (newline) {
    cat(paste0("c(", paste0("\"", paste(vapply(substitute(list(...)),
                                  deparse, "character")[-1L], collapse = "\"\n, \""),
               "\""), "\n)"))
  }
  else {
    cat(paste0("c(", paste0("\"", paste(vapply(substitute(list(...)),
                                  deparse, "character")[-1L], collapse = "\", \""),
               "\""), ")"))
  }
  cat("\n")
}

#' Convert integer64 columns in a data.table to numeric (in-place)
#'
#' Modifies a data.table by converting all `integer64` columns to `numeric`.
#' This function operates **by reference**, so the input data.table
#' is changed directly without creating a copy.
#'
#' @param df A data.table.
#'
#' @return No return value. The input data.table is modified in-place.
#'
#' @examples
#' \dontrun{
#' dt <- data.table::data.table(a = bit64::as.integer64(1:3), b = c("x", "y", "z"))
#' str(dt)
#' set_i64_to_num(dt)
#' str(dt) # column 'a' converted to numeric
#' }
#'
#' @export
set_i64_to_num <- function(df) {
  cols <- jaid::type(df)[class == "integer64"]$column
  df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  return(df)
}

# To be updated -----------------------------------------------------------

sort_group_by <- function(x) {
  .Call(SortGroupBy, x)
}

join <- function(..., by, all = FALSE, all.x = all, all.y = all, sort = TRUE) {
  Reduce(function(...) merge(..., by = by, all = all, all.x = all.x,
                             all.y = all.y, sort = sort), list(...))
}
