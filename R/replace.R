#' Replace NA with zero (in place)
#'
#' Efficiently replace numeric NA values with 0 in a data.table. If `cols`
#' is missing, all numeric and integer columns are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all numeric/integer columns.
#'
#' @return The modified data.table (invisibly for chaining).
#'
#' @examples
#' \dontrun{
#' # Replace NA with 0
#' df <- data.table(x = c(1, NA, 3), y = c("A", "B", NA), z = c(NA, 5, NA))
#' data.table::address(df)
#' replace_na_with_zero(df)
#' data.table::address(df)
#' }
#'
#' @export
replace_na_with_zero <- function(df, cols) {
  assert_class(df, "data.table")
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(is.na(x), 0, x))),
     .SDcols = cols]
  df
}

#' Replace zero with NA (in place)
#'
#' Efficiently replace `0` with `NA` in numeric/integer columns of a data.table.
#' If `cols` is missing, all numeric and integer columns are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all numeric/integer columns.
#'
#' @return The modified data.table.
#'
#' @examples
#' \dontrun{
#' # Replace 0 with NA
#' df <- data.table(x = c(1, 0, 3), y = c("A", "B", NA), z = c(0, 5, 0))
#' data.table::address(df)
#' replace_zero_with_na(df)
#' data.table::address(df)
#' }
#'
#' @export
replace_zero_with_na <- function(df, cols) {
  assert_class(df, "data.table")
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == 0, NA, x))),
     .SDcols = cols]
  df
}

#' Replace empty strings with NA (in place)
#'
#' Efficiently replace `""` with `NA_character_` in character columns of a
#' data.table. If `cols` is missing, all character columns are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all character columns.
#'
#' @return The modified data.table.
#'
#' @examples
#' \dontrun{
#' # Replace "" with NA
#' df <- data.table(x = c("A", "B", ""), y = c(1, NA, 3), z = c("", "E", ""))
#' data.table::address(df)
#' replace_empty_with_na(df)
#' data.table::address(df)
#' }
#'
#' @export
replace_empty_with_na <- function(df, cols) {
  assert_class(df, "data.table")
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == "", NA, x))),
     .SDcols = cols]
  df
}

#' Replace NA with empty strings (in place)
#'
#' Efficiently replace `NA_character_` with `""` in character columns of a
#' data.table. If `cols` is missing, all character columns are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all character columns.
#'
#' @return The modified data.table (invisibly for chaining).
#'
#' @examples
#' \dontrun{
#' # Replace NA with ""
#' df <- data.table(x = c("A", "B", NA), y = c(1, NA, 3), z = c(NA, "E", NA))
#' data.table::address(df)
#' replace_na_with_empty(df)
#' data.table::address(df)
#' }
#'
#' @export
replace_na_with_empty <- function(df, cols) {
  assert_class(df, "data.table")
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(is.na(x), "", x))),
     .SDcols = cols]
  invisible(df[])
}

#' Replace a specific value with another (in place)
#'
#' Efficiently replace occurrences of string `a` with string `b` across selected
#' character columns in a data.table. If `cols` is missing, all character columns
#' are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all character columns.
#' @param a A string to be replaced.
#' @param b Replacement string.
#'
#' @return The modified data.table.
#'
#' @examples
#' \dontrun{
#' # Replace A with B
#' df <- data.table(x = c("A", "A", "C"), y = c("A", "C", "C"))
#' data.table::address(df)
#' replace_a_with_b(df, a = "A", b = "B")
#' data.table::address(df)
#' }
#'
#' @export
replace_a_with_b <- function(df, cols, a, b) {
  assert_class(df, "data.table")
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == a, b, x))), .SDcols = cols]
  df
}

#' Trim whitespace (in place)
#'
#' Remove leading and trailing whitespace from character columns in a
#' data.table. If `cols` is missing, all character columns are targeted.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all character columns.
#' @param ws A regular expression describing whitespace to trim.
#'
#' @return The modified data.table.
#'
#' @examples
#' \dontrun{
#' # Trim whitespace
#' df <- data.table(x = c(" A", "B ", " C "), y = c(1, 2, 3))
#' data.table::address(df)
#' trim_ws(df)
#' data.table::address(df)
#' }
#'
#' @export
trim_ws <- function(df, cols, ws = "[ \t\r\n]") {
  assert_class(df, "data.table")
  if (missing(cols)) {
    class <- sapply(df, class)
    cols <- names(class)[which(class == "character")]
  } else {
    # cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
    cols <- capture_names(df, !!rlang::enquo(cols))
  }
  re <- sprintf("^%s+|%s+$", ws, ws)
  df[, `:=`((cols), lapply(.SD, function(x)
    gsub(re, "", x, perl = TRUE))), .SDcols = cols]
  df
}

#' Remove punctuation (in place)
#'
#' Remove punctuation characters from character columns in a data.table. If
#' `cols` is missing, all character columns are targeted. The default pattern
#' avoids removing asterisks.
#'
#' @param df A data.table (modified in place).
#' @param cols A character vector of column names. Defaults to all character columns.
#' @param pattern A regular expression describing punctuation to remove.
#'
#' @return The modified data.table.
#'
#' @examples
#' \dontrun{
#' # Remove punctuations
#' df <- data.table(x = c("A3-", "$+_B", "C+_&"), y = c("123", "R&", "4q_++"))
#' data.table::address(df)
#' rm_punct(df)
#' data.table::address(df)
#' }
#'
#' @export
rm_punct <- function(df, cols, pattern = "(?!\\*)[[:punct:]]") {
  assert_class(df, "data.table")
  if (missing(cols)) {
    class <- sapply(df, class)
    cols <- names(class)[which(class == "character")]
  } else {
    # cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
    cols <- capture_names(df, !!rlang::enquo(cols))
  }
  df[, `:=`((cols), lapply(.SD, function(x)
    gsub(pattern, "", x, perl = TRUE))), .SDcols = cols]
  df
}

#' Remove columns from a data.table
#'
#' Remove one or more columns from a data.table by reference,
#' without making a copy. This is more memory-efficient than
#' creating a modified copy of the object.
#'
#' @param df A data.table, modified in place.
#' @param cols Columns to remove, specified as a tidy-style
#'   expression (e.g., `.(x, y)`, `list(x, y)`).
#'
#' @return The modified data.table, returned invisibly.
#'
#' @examples
#' \dontrun{
#' # Remove columns
#' df <- data.table::as.data.table(mtcars)
#' address(df)
#' rm_cols(df, .(mpg, cyl, disp))
#' address(df)
#' }
#'
#' @export
rm_cols <- function(df, cols) {
  assert_class(df, "data.table")
  # cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
  cols <- capture_names(df, !!rlang::enquo(cols))
  df[, `:=`((cols), NULL)]
  df
}
