#' Replace NA with zero
#'
#' Replace NA_integer_ or NA_real_ values with zero in a memory-efficient way
#'
#' @param df a data frame
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c(1, NA, 3), y = c("A", "B", NA), z = c(NA, 5, NA))
#' pryr::address(df)
#' replace_na_with_zero(df)
#' pryr::address(df)
#' df}
#'
#' @export
replace_na_with_zero <- function(df) {
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
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
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c(1, 0, 3), y = c("A", "B", NA), z = c(0, 5, 0))
#' pryr::address(df)
#' replace_zero_with_na(df)
#' pryr::address(df)
#' df}
#'
#' @export
replace_zero_with_na <- function(df) {
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
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
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c("A", "B", ""), y = c(1, NA, 3), z = c("", "E", ""))
#' pryr::address(df)
#' replace_empty_with_na(df)
#' pryr::address(df)
#' df}
#'
#' @export
replace_empty_with_na <- function(df) {
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
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
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c("A", "B", NA), y = c(1, NA, 3), z = c(NA, "E", NA))
#' pryr::address(df)
#' replace_na_with_empty(df)
#' pryr::address(df)
#' df}
#'
#' @export
replace_na_with_empty <- function(df) {
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
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
#' @param df a data frame
#' @param ws a white space [regular expression]
#' @return no return value
#'
#' @examples
#' \donttest{df <- data.frame(x = c(" A", "B ", " C "), y = c(1, 2, 3))
#' pryr::address(df)
#' trim_ws(df)
#' pryr::address(df)
#' df}
#'
#' @export
trim_ws <- function(df, ws = "[ \t\r\n]") {
  old_class <- class(df)
  set_dt(df)
  class <- sapply(df, class)
  cols <- names(class)[which(class == "character")]
  re <- sprintf("^%s+|%s+$", ws, ws)
  df[, `:=`((cols), lapply(.SD, function(x)
    gsub(re, "", x, perl = TRUE))), .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}
