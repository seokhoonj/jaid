#' Paste strings
#'
#' Paste strings
#'
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When `collapse` is a string, the result is always a string
#' ([`character`] of length 1). default "|"
#' @return a string vector
#'
#' @examples
#' # paste string
#' \donttest{paste_str(c("a", "b", "c"))}
#'
#' @export
paste_str <- function(x, collapse = "|")
  paste(x, collapse = collapse)

#' Paste unique strings
#'
#' Paste unique strings
#'
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When `collapse` is a string, the result is always a string
#' ([`character`] of length 1). default "|"
#' @return a string vector
#'
#' @examples
#' # paste unique string
#' \donttest{paste_uni_str(c("a", "a", "c", "b", "b", "d", "e"))}
#'
#' @export
paste_uni_str <- function(x, collapse = "|")
  paste(unique(x[!is.na(x)]), collapse = collapse)

#' Paste sorted unique strings
#'
#' Paste sorted unique strings
#'
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When `collapse` is a string, the result is always a string
#' ([`character`] of length 1). default "|"
#' @return a string vector
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
#' Split strings
#'
#' @param x a string vector
#' @param split a string to use for splitting
#' @return a string vector
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
#' Split and paste unique strings
#'
#' @param x a string vector
#' @param split a string to use for splitting
#' @return a string vector
#'
#' @examples
#' # split and paste unique strings
#' \donttest{split_and_paste_uni_str(c("b|a|b", "d|c|d"), split = "|")
#' split_and_paste_sort_uni_str(c("b|a|b", "d|c|d"), split = "|")}
#'
#' @export
split_and_paste_uni_str <- function(x, split = "|") {
  z <- split_str(x, split = split)
  sapply(z, function(x) paste_uni_str(x, collapse = split))
}

#' @rdname split_and_paste_uni_str
#' @export
split_and_paste_sort_uni_str <- function(x, split = "|") {
  z <- split_str(x, split = split)
  sapply(z, function(x) paste_sort_uni_str(x, collapse = split))
}

#' Count pattern matched strings
#'
#' Count pattern matched strings from a string vector.
#'
#' @param pattern a string containing a [regular expression]
#' @param x a string
#' @param ignore.case if `FALSE`, the pattern matching is case sensitive and if `TRUE`, case is ignored during matching.
#' @return a string vector
#'
#' @examples
#' # count pattern matched strings from a string vector
#' \donttest{count_pattern(pattern = "c", c("a|b|c", "a|c|c"))}
#'
#' @export
count_pattern <- function(pattern, x, ignore.case = FALSE) {
  sapply(gregexpr(pattern, x, ignore.case = ignore.case, perl = TRUE), length)
}

#' Get a first pattern
#'
#' Get a first pattern from a string vector.
#'
#' @param pattern a string containing a [regular expression]
#' @param x a string vector
#' @param ignore.case if `FALSE`, the pattern matching is case sensitive and if `TRUE`, case is ignored during matching.
#' @return a string vector
#'
#' @examples
#' # get a first pattern from a string vector
#' \donttest{get_pattern(pattern = "c", c("a|b|c", "a|c|c"))}
#'
#' @export
get_pattern <- function(pattern, x, ignore.case = TRUE) {
  r <- regexpr(pattern, x, ignore.case = ignore.case, perl = TRUE)
  z <- rep("", length(x))
  z[r != -1] <- regmatches(x, r)
  return(z)
}

#' Get all patterns
#'
#' Get all patterns from a string vector.
#'
#' @param pattern a string containing a [regular expression]
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When `collapse` is a string, the result is always a string
#' ([`character`] of length 1). default "|"
#' @param ignore.case if `FALSE`, the pattern matching is case sensitive and if `TRUE`, case is ignored during matching.
#' @return a string vector
#'
#' @examples
#' # get all patterns from a string vector
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
#' Delete patterns from a string vector.
#'
#' @param pattern a string containing a [regular expression]
#' @param x a string vector
#' @return a string vector
#'
#' @examples
#' # delete patterns from a string vector
#' \donttest{del_pattern(pattern = "c", c("abc", "acc"))}
#'
#' @export
del_pattern <- function(pattern, x)
  gsub(pattern, "", x)

#' Paste vectors of a list
#'
#' Paste vectors of equal length in a list or data.frame
#'
#' @param x a list with same length vectors or data frame column vectors you want to paste.
#' @param sep a character string to separate the terms.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#' @return a vector pasted
#'
#' @examples
#' # paste length and width of iris
#' iris$size <- paste_list(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
#' head(iris)
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
