#' get or set first positive values
#'
#' Get or set first positive values of a numerical matrix by rownames
#'
#' @param x A numerical matrix
#' @param cols A boolean vector. for example, if you want select first and third
#'             columns for 3 X 3 matrix, c(1, 0, 1).
#' @return A matrix with only first positive value for each row names, and the
#'         other values become 0.
#'
#' @examples
#' # get first positive values of a numerical matrix by rownames
#' \donttest{x <- matrix(c(-1, 2, 0, -2, -1, 5, 2, -2, 3), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' get_first_pos(x)}
#'
#' # set first positive values of a numerical matrix by rownames
#' \donttest{x <- matrix(c(-1, 2, 0, -2, -1, 5, 2, -2, 3), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' set_first_pos(x)}
#'
#' @export
get_first_pos <- function(x, cols) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  if (missing(cols)) cols <- rep(1, ncol(x))
  .Call(GetFirstPos, x = x, id = rownames(x), ot = cols)
}

#' @rdname get_first_pos
#' @export
set_first_pos <- function(x, cols) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  if (missing(cols)) cols <- rep(1, ncol(x))
  invisible(.Call(SetFirstPos, x, id = rownames(x), ot = cols))
}
