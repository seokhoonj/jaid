#' Data replication function
#'
#' This function is for replicating data rows
#'
#' @param x matrix, data.frame, data.table
#' @param ... times, each
#' @return matrix, data.frame, data.table.
#' @examples
#' rep_row(iris, times = 3)
#' rep_row(iris, each  = 3)
#'
#' @export
rep_row <- function(x, ...) UseMethod("rep_row")

#' @rdname rep_row
#' @export
rep_row.matrix <- function(x, ...) do.call(cbind, lapply(seq_len(ncol(x)), function(s) rep(x[, s], ...)))

#' @rdname rep_row
#' @export
rep_row.data.frame <- function(x, ...) as.data.frame(lapply(x, rep, ...)) # times, each

#' @rdname rep_row
#' @export
rep_row.data.table <- function(x, ...) data.table::as.data.table(lapply(x, rep, ...)) # times, each


# to be updated -----------------------------------------------------------

rep_col <- function(x, each) .Call(RepCol, x, each)
