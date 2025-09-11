#' Replicate rows of a matrix/data.frame/data.table
#'
#' Convenience S3 generic to replicate rows of tabular objects. For
#' each column, values are replicated according to arguments passed to
#' [base::rep()] (e.g., `times`, `each`).
#'
#' @param x A matrix, data.frame or data.table.
#' @param ... Arguments passed to [base::rep()] such as `times` and `each`.
#'
#' @return An object of the same general type as `x` with rows replicated.
#'
#' @examples
#' \donttest{
#' rep_row(iris, times = 3)
#' rep_row(iris, each  = 3)
#' }
#'
#' @export
rep_row <- function(x, ...) {
  assert_class(x, c("data.frame", "data.table", "matrix"))
  UseMethod("rep_row")
}

#' @rdname rep_row
#' @export
rep_row.data.frame <- function(x, ...)
  as.data.frame(lapply(x, rep, ...)) # times, each

#' @rdname rep_row
#' @export
rep_row.data.table <- function(x, ...)
  data.table::as.data.table(lapply(x, rep, ...)) # times, each

#' @rdname rep_row
#' @export
rep_row.matrix <- function(x, ...)
  do.call(cbind, lapply(seq_len(ncol(x)), function(s) rep(x[, s], ...)))


#' Replicate columns of a matrix
#'
#' Repeat each column of a matrix `each` times, returning a matrix with
#' replicated columns (column-wise Kronecker-style replication).
#'
#' @param x A matrix.
#' @param each Integer; number of times to repeat each column.
#' @return A matrix with `ncol(x) * each` columns.
#'
#' @examples
#' \donttest{
#' m <- matrix(1:6, nrow = 2)
#' rep_col(m, each = 3)
#' }
#'
#' @export
rep_col <- function(x, each) .Call(RepCol, x, each)
