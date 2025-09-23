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

#' Index columns by name
#'
#' Return column indices for the given column names from a data.frame or matrix.
#'
#' @param x A data.frame or matrix.
#' @param cols A character vector of column names (in desired order).
#' @param all_matches Logical; if `TRUE`, return indices of *all* columns whose
#'   names match each `cols` entry (duplicates allowed). If `FALSE` (default),
#'   return only the first match for each name.
#' @param error_on_missing Logical; if `TRUE` (default), error when any of
#'   `cols` are not present in `colnames(x)`. If `FALSE`, drop missing names.
#'
#' @return
#' Integer vector of column indices. Names of the vector correspond to the
#' requested `cols` that were successfully matched.
#'
#' @examples
#' # data.frame
#' index_cols(mtcars, c("disp", "drat", "qsec"))
#'
#' # drop missing columns silently
#' index_cols(mtcars, c("disp", "drat", "nope"), error_on_missing = FALSE)
#'
#' # matrix
#' mat <- as.matrix(mtcars)
#' index_cols(mat, c("disp", "hp"))
#'
#' # duplicated column names (all matches)
#' df <- data.frame(a = 1, a = 2, b = 3, check.names = FALSE)
#' index_cols(df, "a", all_matches = TRUE)
#'
#' @export
index_cols <- function(x, cols, all_matches = FALSE, error_on_missing = TRUE) {
  jaid::assert_class(x, c("data.frame", "matrix"))

  if (!is.character(cols) || length(cols) < 1L || anyNA(cols)) {
    rlang::abort(
      message = "`cols` must be a non-empty character vector without NA.",
      class   = c("jaid_error_invalid_argument", "jaid_error", "error"),
      arg     = "cols"
    )
  }

  nms <- colnames(x)
  if (is.null(nms)) {
    rlang::abort(
      message = "`x` has no column names.",
      class   = c("jaid_error_no_colnames", "jaid_error", "error")
    )
  }

  if (!all_matches) {
    idx0    <- match(cols, nms)
    na_mask <- is.na(idx0)

    if (any(na_mask) && error_on_missing) {
      rlang::abort(
        message = sprintf("Missing column(s): %s",
                          paste(cols[na_mask], collapse = ", ")),
        class   = c("jaid_error_missing_columns", "jaid_error", "error"),
        missing = cols[na_mask],
        columns = nms
      )
    }

    idx  <- idx0[!na_mask]
    kept <- cols[!na_mask]
    names(idx) <- kept
    return(idx)
  }

  # all_matches = TRUE — collect all hits for each requested name
  out_idx  <- integer(0)
  out_name <- character(0)

  for (nm in cols) {
    hits <- which(nms == nm)
    if (length(hits) == 0L) {
      if (error_on_missing) {
        rlang::abort(
          message = sprintf("Missing column: %s", nm),
          class   = c("jaid_error_missing_columns", "jaid_error", "error"),
          missing = nm,
          columns = nms
        )
      } else {
        next
      }
    }
    out_idx  <- c(out_idx, hits)
    out_name <- c(out_name, rep.int(nm, length(hits)))
  }

  names(out_idx) <- out_name
  out_idx
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
  nms <- names(df)
  nms[match(cols, nms, 0L)]
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
  nms <- names(df)
  nms[grepl(pattern, nms, perl = TRUE)]
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
#' \donttest{anti_cols(mtcars, c("mpg", "cyl", "disp", "hp", "drat"))}
#'
#' @export
anti_cols <- function(df, cols) {
  assert_class(df, "data.frame")
  setdiff(names(df), cols)
}

#' Find matching attributes
#'
#' Returns the names of attributes in `x` that match the specified names.
#'
#' @param x Any R object (e.g., `list`, `data.frame`, `data.table`).
#' @param name Character vector of attribute names to match.
#'
#' @return A character vector of matching attribute names.
#'
#' @examples
#' \donttest{
#' # Find attributes by name
#' match_attr(iris, c("class", "names"))
#' }
#'
#' @export
match_attr <- function(x, name)
  names(attributes(x))[match(name, names(attributes(x)), 0L)]

#' Find attributes by regular expression
#'
#' Return the names of attributes in `x` whose names match a regular expression pattern.
#'
#' @param x Any R object (e.g., `list`, `data.frame`, `data.table`).
#' @param name A character string containing a regular expression pattern
#'   to match against attribute names.
#'
#' @return A character vector of matching attribute names.
#'
#' @examples
#' \donttest{
#' # Find attributes by regex
#' regex_attr(iris, "class|names")
#' }
#'
#' @export
regex_attr <- function(x, name) {
  names(attributes(x))[grepl(name, names(attributes(x)))]
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

join <- function(..., by, all = FALSE, all.x = all, all.y = all, sort = TRUE) {
  Reduce(function(...) merge(..., by = by, all = all, all.x = all.x,
                             all.y = all.y, sort = sort), list(...))
}
