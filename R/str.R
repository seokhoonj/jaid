#' Paste strings
#'
#' Collapse a character vector into a single string using a separator.
#'
#' @param x A character vector.
#' @param collapse A single string used to separate elements (not `NA_character_`).
#'   Default: `"|"`.
#'
#' @return A length-1 character vector (a single collapsed string).
#'
#' @examples
#' \donttest{paste_str(c("a", "b", "c"))}
#'
#' @export
paste_str <- function(x, collapse = "|")
  paste(x, collapse = collapse)

#' Paste unique strings
#'
#' Collapse unique, non-missing elements of a character vector into a single string.
#'
#' @param x A character vector.
#' @param collapse A single string used to separate elements (not `NA_character_`).
#'   Default: `"|"`.
#'
#' @return A length-1 character vector.
#'
#' @examples
#' \donttest{paste_uni_str(c("a", "a", "c", "b", "b", "d", "e"))}
#'
#' @export
paste_uni_str <- function(x, collapse = "|")
  paste(unique(x[!is.na(x)]), collapse = collapse)

#' Paste sorted unique strings
#'
#' Collapse **sorted** unique, non-missing elements into a single string.
#'
#' @param x A character vector.
#' @param collapse A single string used to separate elements (not `NA_character_`).
#'   Default: `"|"`.
#'
#' @return A length-1 character vector.
#'
#' @examples
#' # paste sorted unique string
#' \donttest{paste_sort_uni_str(c("a", "a", "c", "b", "b", "d", "e"))}
#'
#' @export
paste_sort_uni_str <- function(x, collapse = "|")
  paste(sort(unique(x[!is.na(x)])), collapse = collapse)

#' Split strings
#'
#' Split each input string on a delimiter.
#'
#' @param x A character vector.
#' @param split A single string delimiter.
#'
#' @return A list of character vectors (one element per input string).
#'
#' @examples
#' # split strings
#' \donttest{split_str(c("a|b|a", "c|d|c"), split = "|")}
#'
#' @export
split_str <- function(x, split = "|")
  strsplit(x, split = split, fixed = TRUE)

#' Split and paste unique strings
#'
#' Split each string then paste the **unique** tokens back with the same separator.
#'
#' @param x A character vector.
#' @param split A single string delimiter.
#'
#' @return A character vector of the same length as `x`.
#'
#' @examples
#' \donttest{
#' split_and_paste_uni_str(c("b|a|b", "d|c|d"), split = "|")
#' split_and_paste_sort_uni_str(c("b|a|b", "d|c|d"), split = "|")
#' }
#'
#' @export
split_and_paste_uni_str <- function(x, split = "|") {
  z <- split_str(x, split = split)
  sapply(z, function(x) paste_uni_str(x, collapse = split))
}

#' @rdname split_and_paste_uni_str
#' @description
#' Split each string then paste the **sorted unique** tokens with the same separator.
#'
#' @export
split_and_paste_sort_uni_str <- function(x, split = "|") {
  z <- split_str(x, split = split)
  sapply(z, function(x) paste_sort_uni_str(x, collapse = split))
}

#' Count pattern matches
#'
#' Count regex matches in each string.
#'
#' @param pattern A string containing a regular expression.
#' @param x A character vector.
#' @param ignore.case Logical; if `TRUE`, case is ignored. Default: `FALSE`.
#'
#' @return An integer vector of match counts (same length as `x`).
#'
#' @examples
#' \donttest{count_pattern(pattern = "c", c("a|b|c", "a|c|c"))}
#'
#' @export
count_pattern <- function(pattern, x, ignore.case = FALSE) {
  sapply(gregexpr(pattern, x, ignore.case = ignore.case, perl = TRUE),
         function(x) length(x[x != -1]))
}

#' Get first match
#'
#' Extract the first regex match from each string (empty string if no match).
#'
#' @param pattern A string containing a regular expression.
#' @param x A character vector.
#' @param ignore.case Logical; if `TRUE`, case is ignored. Default: `TRUE`.
#'
#' @return A character vector of the same length as `x`.
#'
#' @examples
#' \donttest{get_pattern(pattern = "c", c("a|b|c", "a|c|c"))}
#'
#' @export
get_pattern <- function(pattern, x, ignore.case = TRUE) {
  r <- regexpr(pattern, x, ignore.case = ignore.case, perl = TRUE)
  z <- rep("", length(x))
  z[r != -1] <- regmatches(x, r)
  return(z)
}

#' Get all matches
#'
#' Extract all regex matches from each string and collapse them with a separator.
#'
#' @param pattern A string containing a regular expression.
#' @param x A character vector.
#' @param collapse A single string used to separate matches (not `NA_character_`).
#'   Default: `"|"`.
#' @param ignore.case Logical; if `TRUE`, case is ignored. Default: `TRUE`.
#'
#' @return A character vector of the same length as `x`
#'   (empty string for elements with no matches).
#'
#' @examples
#' \donttest{get_pattern_all(pattern = "c", c("a|b|c", "a|c|c"))}
#'
#' @export
get_pattern_all <- function(pattern, x, collapse = "|", ignore.case = TRUE) {
  r <- gregexpr(pattern, x, ignore.case = ignore.case, perl = TRUE)
  z <- regmatches(x, r)
  sapply(z, function(s) paste(s, collapse = collapse))
}


#' Delete patterns
#'
#' Remove all regex matches from each string.
#'
#' @param pattern A string containing a regular expression.
#' @param x A character vector.
#'
#' @return A character vector with matches removed.
#'
#' @examples
#' \donttest{del_pattern(pattern = "c", c("abc", "acc"))}
#'
#' @export
del_pattern <- function(pattern, x)
  gsub(pattern, "", x)

#' Paste vectors of a list
#'
#' Paste vectors of equal length in a list or data.frame
#'
#' @param x A list with same length vectors or data frame column vectors you want to paste.
#' @param sep A character string to separate the terms.
#' @param na.rm A logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#' @return a vector pasted
#'
#' @examples
#' \donttest{
#' iris$size <- paste_list(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
#' head(iris)
#' }
#'
#' @export
paste_list <- function(x, sep = "|", na.rm = FALSE) {
  if (na.rm)
    x[] <- lapply(x, function(s) ifelse(is.na(s), "", s))
  pattern <- sprintf("^\\%s|\\%s\\%s|\\%s$", sep, sep, sep, sep)
  n <- length(x)
  if (n == 1L)
    return(x[[1L]])
  x <- do.call(function(...) paste(..., sep = sep), x)
  x <- gsub(pattern, "", x)
  if (na.rm)
    x <- ifelse(x == "", NA, x)
  return(x)
}
