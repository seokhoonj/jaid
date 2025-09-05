#' Set dimension, row, or column names for a matrix
#'
#' These functions set names on a matrix:
#' * `set_dimnames()` sets both row and column names.
#' * `set_rownames()` sets only row names.
#' * `set_colnames()` sets only column names.
#'
#' @param x A matrix.
#' @param dimnames A list of length 2: the first element is a character vector
#'   of row names, the second element is a character vector of column names.
#' @param rownames A character vector of row names.
#' @param colnames A character vector of column names.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \donttest{
#' # Set both row and column names
#' x <- matrix(1:9, ncol = 3)
#' set_dimnames(x, list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
#'
#' # Set only row names
#' x <- matrix(1:9, ncol = 3)
#' set_rownames(x, c("r1", "r2", "r3"))
#'
#' # Set only column names
#' x <- matrix(1:9, ncol = 3)
#' set_colnames(x, c("c1", "c2", "c3"))
#' }
#'
#' @export
set_dimnames <- function(x, dimnames) invisible(.Call(SetDimNames, x, dimnames))

#' @rdname set_dimnames
#' @export
set_rownames <- function(x, rownames) invisible(.Call(SetRowNames, x, rownames))

#' @rdname set_dimnames
#' @export
set_colnames <- function(x, colnames) invisible(.Call(SetColNames, x, colnames))
