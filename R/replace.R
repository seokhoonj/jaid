#' Replace NA with zero
#'
#' Replace NA_integer_ or NA_real_ values with zero in a memory-efficient way
#'
#' @param df a data frame
#' @param cols a string vector specifying columns
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c(1, NA, 3), y = c("A", "B", NA), z = c(NA, 5, NA))
#' set_ptr(df)
#' data.table::address(df)
#' replace_na_with_zero(df)
#' data.table::address(df)
#' df}
#'
#' @export
replace_na_with_zero <- function(df, cols) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(is.na(x), 0, x))),
     .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Replace zero with NA
#'
#' Replace zero values with NA_integer_ or NA_real_ in a memory-efficient way
#'
#' @param df a data frame
#' @param cols a string vector specifying columns
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c(1, 0, 3), y = c("A", "B", NA), z = c(0, 5, 0))
#' set_ptr(df)
#' data.table::address(df)
#' replace_zero_with_na(df)
#' data.table::address(df)
#' df}
#'
#' @export
replace_zero_with_na <- function(df, cols) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class %in% c("numeric", "integer"))]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == 0, NA, x))),
     .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Replace empty with NA
#'
#' Replace empty string like "" with NA_character_ in a memory-efficient way
#'
#' @param df a data frame
#' @param cols a string vector specifying columns
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c("A", "B", ""), y = c(1, NA, 3), z = c("", "E", ""))
#' set_ptr(df)
#' data.table::address(df)
#' replace_empty_with_na(df)
#' data.table::address(df)
#' df}
#'
#' @export
replace_empty_with_na <- function(df, cols) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == "", NA, x))),
     .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Replace NA with empty
#'
#' Replace NA_character_ with "" in a memory-efficient way
#'
#' @param df a data frame
#' @param cols a string vector specifying columns
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c("A", "B", NA), y = c(1, NA, 3), z = c(NA, "E", NA))
#' set_ptr(df)
#' data.table::address(df)
#' replace_na_with_empty(df)
#' data.table::address(df)
#' df}
#'
#' @export
replace_na_with_empty <- function(df, cols) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(is.na(x), "", x))),
     .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Trim white space
#'
#' Trim white space
#'
#' @param df a data.frame
#' @param cols a string vector specifying columns
#' @param ws a white space [regular expression]
#' @return no return values
#'
#' @examples
#' \donttest{df <- data.frame(x = c(" A", "B ", " C "), y = c(1, 2, 3))
#' set_ptr(df)
#' data.table::address(df)
#' trim_ws(df)
#' data.table::address(df)
#' df}
#'
#' @export
trim_ws <- function(df, cols, ws = "[ \t\r\n]") {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  if (missing(cols)) {
    class <- sapply(df, class)
    cols <- names(class)[which(class == "character")]
  } else {
    cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
  }
  re <- sprintf("^%s+|%s+$", ws, ws)
  df[, `:=`((cols), lapply(.SD, function(x)
    gsub(re, "", x, perl = TRUE))), .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}

#' Remove punctuations
#'
#' Remove punctuations.
#'
#' @param df a data.frame
#' @param cols a string vector specifying columns
#' @param pattern a string containing a [regular expression]
#' @return no return values
#'
#' @examples
#' # remove punctuations
#' \donttest{df <- data.frame(x = c("A3-", "$+_B", "C+_&"), y = c("123", "R&", "4q_++"))
#' set_ptr(df)
#' rm_punct(df)
#' df}
#'
#' @export
rm_punct <- function(df, cols, pattern = "(?!\\*)[[:punct:]]") {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  if (missing(cols)) {
    class <- sapply(df, class)
    cols <- names(class)[which(class == "character")]
  } else {
    cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
  }
  df[, `:=`((cols), lapply(.SD, function(x)
    gsub(pattern, "", x, perl = TRUE))), .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}
