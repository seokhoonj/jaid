#' Zero out all but the first positive value per row group
#'
#' For each group of rows (defined by row names), scan from top to bottom
#' and keep only the first positive value in the selected columns. All other
#' values in the group are set to zero.
#'
#' @param x A numeric matrix with row names.
#' @param include_cols A logical vector of length `ncol(x)` indicating which
#'   columns should be considered (default: all `TRUE`). For example, for a
#'   3×3 matrix, `c(TRUE, FALSE, TRUE)` selects the 1st and 3rd columns.
#'
#' @return
#' * `fill_zero_not_first_pos()` returns a modified copy of `x`.
#' * `set_zero_not_first_pos()` modifies `x` in place and returns it invisibly.
#'
#' @examples
#' \donttest{
#' x <- matrix(c(-1,  2, 0,
#'               -2, -1, 5,
#'                2, -2, 3), nrow = 3, byrow = TRUE)
#' rownames(x) <- c(1, 1, 2)
#'
#' # Keep only the first positive per group
#' fill_zero_not_first_pos(x)
#'
#' # Restrict to first and third columns
#' fill_zero_not_first_pos(x, include_cols = c(TRUE, FALSE, TRUE))
#'
#' # In-place modification
#' set_zero_not_first_pos(x, include_cols = c(TRUE, FALSE, TRUE))
#' }
#'
#' @export
fill_zero_not_first_pos <- function(x, include_cols) {
  if (is.null(rownames(x)))
    stop(trace_arg_expr(x), " doesn't have row names", call. = FALSE)

  if (missing(include_cols)) include_cols <- rep(TRUE, ncol(x))

  if (length(include_cols) != ncol(x))
    stop("`include_cols` must have length ncol(x).", call. = FALSE)

  if (!is.logical(include_cols))
    stop("`include_cols` must be a logical vector (TRUE/FALSE).", call. = FALSE)

  .Call(FillZeroNotFirstPos, x, id = rownames(x), ot = include_cols)
}

#' @rdname fill_zero_not_first_pos
#' @export
set_zero_not_first_pos <- function(x, include_cols) {
  if (is.null(rownames(x)))
    stop(trace_arg_expr(x), " doesn't have row names", call. = FALSE)

  if (missing(include_cols)) include_cols <- rep(TRUE, ncol(x))

  if (length(include_cols) != ncol(x))
    stop("`include_cols` must have length ncol(x).", call. = FALSE)

  if (!is.logical(include_cols))
    stop("`include_cols` must be a logical vector (TRUE/FALSE).", call. = FALSE)

  invisible(.Call(SetZeroNotFirstPos, x, id = rownames(x), ot = include_cols))
}

#' Insert ones before the first one per row group
#'
#' For each group of rows (defined by row names), scan each column from
#' top to bottom. Whenever a `1` is encountered, also set the entry in
#' the immediately preceding row of the same group to `1`. All other
#' entries remain unchanged.
#'
#' @param x A binary matrix with row names.
#'
#' @return
#' * `fill_one_before_first_one()` returns a modified copy of `x`.
#' * `set_one_before_first_one()` modifies `x` in place and returns it invisibly.
#'
#' @examples
#' \donttest{
#' # Example binary matrix
#' x <- matrix(c(0, 1, 0,
#'               0, 0, 1,
#'               1, 0, 1), nrow = 3, byrow = TRUE)
#' rownames(x) <- c(1, 1, 2)
#'
#' # Return a modified copy
#' fill_one_before_first_one(x)
#'
#' # Modify the matrix in place
#' set_one_before_first_one(x)
#' }
#'
#' @export
fill_one_before_first_one <- function(x) {
  if (is.null(rownames(x)))
    stop(trace_arg_expr(x), " doesn't have row names", call. = FALSE)
  .Call(FillOneBeforeFirstOne, x = x, id = rownames(x))
}

#' @rdname fill_one_before_first_one
#' @export
set_one_before_first_one <- function(x) {
  if (is.null(rownames(x)))
    stop(trace_arg_expr(x), " doesn't have row names", call. = FALSE)
  invisible(.Call(SetOneBeforeFirstOne, x = x, id = rownames(x)))
}
