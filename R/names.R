#' Set dimension, row, or column names for a matrix
#'
#' These functions set names on a matrix:
#' * `set_mat_dimnames()` sets both row and column names.
#' * `set_mat_rownames()` sets only row names.
#' * `set_mat_colnames()` sets only column names.
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
#' set_mat_dimnames(x, list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
#'
#' # Set only row names
#' x <- matrix(1:9, ncol = 3)
#' set_mat_rownames(x, c("r1", "r2", "r3"))
#'
#' # Set only column names
#' x <- matrix(1:9, ncol = 3)
#' set_mat_colnames(x, c("c1", "c2", "c3"))
#' }
#'
#' @export
set_mat_dimnames <- function(x, dimnames) invisible(.Call(SetMatDimNames, x, dimnames))

#' @rdname set_mat_dimnames
#' @export
set_mat_rownames <- function(x, rownames) invisible(.Call(SetMatRowNames, x, rownames))

#' @rdname set_mat_dimnames
#' @export
set_mat_colnames <- function(x, colnames) invisible(.Call(SetMatColNames, x, colnames))
