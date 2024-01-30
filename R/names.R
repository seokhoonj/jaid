#' Set dimension, row, column names
#'
#' Set dimension, row, column names to a matrix.
#'
#' @param x A matrix
#' @param dimnames,rownames,colnames A list of dimension names\cr
#'                                   A vector of row names\cr
#'                                   A vector of column names
#'
#' @return no return value
#'
#' @examples
#' # set dimension names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))}
#'
#' # set row names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_rownames(x, c("a", "a", "b"))}
#'
#' # set column names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_colnames(x, c("a", "b", "b"))}
#'
#' @export
set_dimnames <- function(x, dimnames) invisible(.Call(SetDimNames, x, dimnames))

#' @rdname set_dimnames
#' @export
set_rownames <- function(x, rownames) .Call(SetRowNames, x, rownames)

#' @rdname set_dimnames
#' @export
set_colnames <- function(x, colnames) invisible(.Call(SetColNames, x, colnames))
