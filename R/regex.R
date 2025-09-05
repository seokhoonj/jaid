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
