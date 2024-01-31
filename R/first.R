#' fill with or set to zero except for the first positive values
#'
#' Fill with or set to zero except for the first positive values of a numerical
#' matrix by rownames
#'
#' @param x A numerical matrix
#' @param cols A boolean vector. for example, if you want select first and third
#'             columns for 3 X 3 matrix, c(1, 0, 1).
#' @return A matrix with only first positive value for each row names, and the
#'         other values become 0.
#'
#' @examples
#' # fill with zero except for the first positive values of a numerical matrix
#' # by rownames
#' \donttest{x <- matrix(c(-1, 2, 0, -2, -1, 5, 2, -2, 3), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' fill_zero_not_first_pos(x)}
#'
#' # set to zero except for the first positive values of a numerical matrix by
#' # rownames
#' \donttest{x <- matrix(c(-1, 2, 0, -2, -1, 5, 2, -2, 3), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' set_zero_not_first_pos(x)}
#'
#' @export
fill_zero_not_first_pos <- function(x, cols) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  if (missing(cols)) cols <- rep(1, ncol(x))
  .Call(FillZeroNotFirstPos, x, id = rownames(x), ot = cols)
}

#' @rdname fill_zero_not_first_pos
#' @export
set_zero_not_first_pos <- function(x, cols) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  if (missing(cols)) cols <- rep(1, ncol(x))
  invisible(.Call(SetZeroNotFirstPos, x, id = rownames(x), ot = cols))
}

#' fill with one before the first one
#'
#' Fill with one before the first one appears in each column in a binary matrix
#' by rownames
#'
#' @param x A binary matrix
#'
#' @return A binary matrix
#'
#' @examples
#' # fill with one before the first one appears by rownames
#' \donttest{x <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 1), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' fill_one_before_first_one(x)}
#'
#' # Set to one before the first one appears by rownames
#' \donttest{x <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 1), nrow = 3)
#' set_rownames(x, c(1, 1, 2))
#' fill_one_before_first_one(x)}
#'
#' @export
fill_one_before_first_one <- function(x) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  .Call(FillOneBeforeFirstOne, x = x, id = rownames(x))
}

#' @rdname fill_one_before_first_one
#' @export
set_one_before_first_one <- function(x) {
  if (is.null(rownames(x)))
    stop(devars(x), " doesn't have row names", call. = FALSE)
  invisible(.Call(SetOneBeforeFirstOne, x = x, id = rownames(x)))
}
