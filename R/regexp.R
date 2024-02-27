#' grepl with `And` conditions
#'
#' grepl with `And` conditions.
#'
#' @param pattern a character vector containing regular expressions
#' @param x a character vector
#' @param ignore.case if `FALSE`, the pattern matching is case sensitive and
#' if `TRUE`, case is ignored during matching.
#' @return a boolean vector
#'
#' @examples
#' # grepl with "And" conditions
#' \donttest{string <- "abcde12345"
#' grepl_and(c("a", "c", "e"), string)
#' grepl_and(c("a", "c", "[1-5]"), string)
#' grepl_and(c("a", "c", "z"), string)}
#'
#' @export
grepl_and <- function(pattern, x, ignore.case = TRUE) {
  pattern <- paste(sprintf("(?=.*%s)", pattern), collapse = "")
  grepl(pattern, x, ignore.case = ignore.case, perl = TRUE)
}
