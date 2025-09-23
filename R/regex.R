#' Build an exact-match regex
#'
#' Construct a regex that matches input strings **exactly** (i.e., with
#' start/end anchors). When multiple inputs are provided, they are joined
#' with OR (`|`) and wrapped with `^(?: ... )$`.
#'
#' @param x Character vector of patterns.
#' @param collapse String used to join multiple patterns (default: `"|"`).
#' @param fixed Logical; if `TRUE` (default), escape regex metacharacters in `x`
#'   so they are treated literally. If `FALSE`, `x` is treated as raw regex.
#'
#' @return A length-1 character string containing the regex. Use with
#'   `grepl(..., perl = TRUE)`.
#'
#' @examples
#' \donttest{
#' x <- c("apple", "banana")
#' pattern <- build_exact_pattern(x)
#' grepl(pattern, c("apple", "banana", "pineapple"), perl = TRUE)
#' # [1] TRUE TRUE FALSE
#' }
#'
#' @export
build_exact_pattern <- function(x, collapse = "|", fixed = TRUE) {
  x <- as.character(x)
  if (length(x) == 0L) return("^(?!)")  # match nothing

  if (fixed) {
    esc <- function(s) gsub("([][{}()+*^$|?.\\-\\\\])", "\\\\\\1", s, perl = TRUE)
    x <- vapply(x, esc, FUN.VALUE = character(1))
  }

  alt <- paste(x, collapse = collapse)
  paste0("^(?:", alt, ")$")
}

#' Match only if all patterns are present (AND)
#'
#' Vectorized helper around [base::grepl()] that returns `TRUE` only when
#' every pattern in `pattern` is found in `x`. Internally, it builds a single
#' regular expression using positive lookahead and evaluates it with
#' `perl = TRUE`.
#'
#' @param pattern Character vector of regular expressions that must all match.
#' @param x Character vector to search.
#' @param ignore.case Logical; if `TRUE`, case is ignored during matching.
#'
#' @return Logical vector the same length as `x`.
#'
#' @examples
#' x <- c("abcde12345", "xyz", "a1c")
#' grepl_all(c("a", "c", "[1-5]"), x)
#' grepl_all(c("foo", "bar"), c("foo bar", "foo", "bar"))
#'
#' @seealso [base::grepl()]
#'
#' @export
grepl_all <- function(pattern, x, ignore.case = TRUE) {
  pattern <- paste(sprintf("(?=.*%s)", pattern), collapse = "")
  grepl(pattern, x, ignore.case = ignore.case, perl = TRUE)
}
