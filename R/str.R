#' Paste strings
#'
#' Paste strings
#'
#' @param x a string vector
#' @param collapse an optional character string to separate the results. Not
#' [`NA_character_`]. When collapse is a string, the result is always a string
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
#' [`NA_character_`]. When collapse is a string, the result is always a string
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
#' [`NA_character_`]. When collapse is a string, the result is always a string
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

split_str <- function(x, split = "|") {
  z <- strsplit(x, split = split, fixed = TRUE)[[1L]]
  z[!z %in% c(NA, "NA", "")]
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
#' [`NA_character_`]. When collapse is a string, the result is always a string
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
