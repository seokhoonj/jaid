#' @title devars
#'
#' @description
#' This function operates like `deparse(substitute(x))` inside the functions.
#'
#' @param x A string, vector or list expression that can be a string vector
#' @return A string vector
#'
#' @examples
#' # deparse(substitute(x))
#' \donttest{devars(expression)
#' devars(c(expression, string))
#' devars(list(expression, string))
#' devars(.(expression, string))}
#'
#' @export
devars <- function(x) {
  if (identical(parent.frame(), globalenv()))
    n <- sys.nframe()
  else n <- 1L
  x <- eval(substitute(substitute(x)), envir = parent.frame(n = max(n, 1L)))
  if (length(x) == 1L)
    return(deparse(x))
  return(vapply(x, deparse, "character")[-1L])
}
