#' Set the full `dimnames` of a matrix.
#'
#' This function replaces both row and column names at once
#' by assigning the provided `dimnames` list directly to the matrix.
#'
#' @param x A matrix.
#' @param dimnames A list of length 2: the first element is a character vector
#'   of row names, the second element is a character vector of column names.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \donttest{
#' x <- matrix(1:9, ncol = 3)
#' set_dimnames(x, list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
#' }
#'
#' @export
set_dimnames <- function(x, dimnames)
  invisible(.Call(SetDimNames, x, dimnames))
