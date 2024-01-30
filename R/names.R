#' Set dimension, row, column names
#'
#' Set dimension, row, column names to a matrix.
#'
#' @param x A matrix
#' @param dimnames A list of dimension names\cr A vector of row names\cr A vector of column names
#'
#' @return no return value
#'
#' @examples
#' # set dimension names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))}
#'
#' # set row names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_row_nm(x, c("a", "a", "b"))}
#'
#' # set column names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_col_nm(x, c("a", "b", "b"))}
#'
#' @export
set_dim_nm <- function(x, dimnames) invisible(.Call(SetDimNm, x, dimnames))

#' @rdname set_dim_nm
#' @export
set_row_nm <- function(x, rownames) invisible(.Call(SetRowNm, x, rownames))

#' @rdname set_dim_nm
#' @export
set_col_nm <- function(x, colnames) invisible(.Call(SetColNm, x, colnames))
