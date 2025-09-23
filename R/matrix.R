#' Aggregate by row names (sum, max, min)
#'
#' Compute the maximum, minimum, or sum of values in a numeric matrix,
#' grouped by identical row names. These functions are implemented in C
#' for performance.
#'
#' @param x A numeric matrix with non-`NULL` row names.
#' @param na.rm Logical; if `TRUE`, missing values are removed before
#'   aggregation.
#'
#' @return A numeric matrix with one row per unique row name of `x`:
#' * `max_by_rownames()` returns the maximum of each group.
#' * `min_by_rownames()` returns the minimum of each group.
#' * `sum_by_rownames()` returns the sum of each group.
#'
#' @examples
#' \donttest{
#' # Maximum values grouped by row names
#' x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_rownames(x)
#'
#' # Minimum values grouped by row names
#' x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_rownames(x)
#'
#' # Sum of values grouped by row names
#' x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_rownames(x)
#' }
#'
#' @export
max_by_rownames <- function(x, na.rm = TRUE) {
  .Call(MaxByRowNames, x, na.rm)
}

#' @rdname max_by_rownames
#' @export
min_by_rownames <- function(x, na.rm = TRUE) {
  .Call(MinByRowNames, x, na.rm)
}

#' @rdname max_by_rownames
#' @export
sum_by_rownames <- function(x, na.rm = TRUE) {
  .Call(SumByRowNames, x, na.rm)
}

#' Aggregate by column names (max, min, sum)
#'
#' Compute the maximum, minimum, or sum of values in a numeric matrix,
#' grouped by identical column names. These functions are implemented in C
#' for performance.
#'
#' @param x A numeric matrix with non-`NULL` column names.
#' @param na.rm Logical; if `TRUE`, missing values are removed before
#'   aggregation.
#'
#' @return A numeric matrix with one column per unique column name of `x`:
#' * `max_by_colnames()` returns the maximum of each group.
#' * `min_by_colnames()` returns the minimum of each group.
#' * `sum_by_colnames()` returns the sum of each group.
#'
#' @examples
#' \donttest{
#' # Maximum values grouped by column names
#' x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_colnames(x)
#'
#' # Minimum values grouped by column names
#' x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_colnames(x)
#'
#' # Sum of values grouped by column names
#' x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_colnames(x)
#' }
#'
#' @export
max_by_colnames <- function(x, na.rm = TRUE) {
  .Call(MaxByColNames, x, na.rm)
}

#' @rdname max_by_colnames
#' @export
min_by_colnames <- function(x, na.rm = TRUE) {
  .Call(MinByColNames, x, na.rm)
}

#' @rdname max_by_colnames
#' @export
sum_by_colnames <- function(x, na.rm = TRUE) {
  .Call(SumByColNames, x, na.rm)
}

#' Aggregate by both row and column names (sum, max, min)
#'
#' Compute element-wise aggregates of a numeric matrix after grouping
#' simultaneously by identical **row names** and **column names**. Rows with
#' the same name are collapsed into one row, and columns with the same name are
#' collapsed into one column, using the chosen summary (sum/max/min).
#'
#' @param x A numeric matrix with non-`NULL` row and column names.
#' @param na.rm Logical; if `TRUE`, missing values are removed before
#'   aggregation.
#'
#' @return A numeric matrix with one row per unique row name and one column
#'   per unique column name of `x`:
#' * `sum_by_dimnames()` returns element-wise sums within each (row-name, col-name) group.
#' * `max_by_dimnames()` returns element-wise maxima within each group.
#' * `min_by_dimnames()` returns element-wise minima within each group.
#'
#' @examples
#' \donttest{
#' x <- matrix(1:9, ncol = 3)
#' set_dimnames(x, list(c("a","a","b"), c("a","b","b")))
#'
#' sum_by_dimnames(x)
#' max_by_dimnames(x)
#' min_by_dimnames(x)
#' }
#'
#' @export
sum_by_dimnames <- function(x, na.rm = TRUE) {
  .Call(SumByDimNames, x, na.rm)
}

#' @rdname sum_by_dimnames
#' @export
max_by_dimnames <- function(x, na.rm = TRUE) {
  .Call(MaxByDimNames, x, na.rm)
}

#' @rdname sum_by_dimnames
#' @export
min_by_dimnames <- function(x, na.rm = TRUE) {
  .Call(MinByDimNames, x, na.rm)
}

#' Row-wise sum, maximum, and minimum
#'
#' Compute the sum, maximum, or minimum of values in each row of a numeric
#' matrix. These functions are implemented in C for performance.
#'
#' @param x A numeric matrix.
#' @param na.rm Logical. Should missing values (`NA`) be removed?
#'   If `TRUE` (default), they are ignored; if `FALSE`, any `NA` in a row
#'   makes the result `NA`.
#'
#' @return A numeric vector of length equal to the number of rows in `x`:
#' * `row_sum()` returns the sum of each row.
#' * `row_max()` returns the maximum of each row.
#' * `row_min()` returns the minimum of each row.
#'
#' @examples
#' \donttest{
#' x <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 2)
#'
#' # Row-wise sum
#' row_sum(x)
#'
#' # Row-wise maximum
#' row_max(x)
#'
#' # Row-wise minimum (preserve NA)
#' row_min(x, na.rm = FALSE)
#' }
#'
#' @export
row_sum <- function(x, na.rm = TRUE) .Call(RowSum, x, na.rm)

#' @rdname row_sum
#' @export
row_max <- function(x, na.rm = TRUE) .Call(RowMax, x, na.rm)

#' @rdname row_sum
#' @export
row_min <- function(x, na.rm = TRUE) .Call(RowMin, x, na.rm)


#' Column-wise sum, maximum, and minimum
#'
#' Compute the sum, maximum, or minimum of values in each column of a numeric
#' matrix. These functions are implemented in C for performance.
#'
#' @param x A numeric matrix.
#' @param na.rm Logical. Should missing values (`NA`) be removed?
#'   If `TRUE` (default), they are ignored; if `FALSE`, any `NA` in a column
#'   makes the result `NA`.
#'
#' @return A numeric vector of length equal to the number of columns in `x`:
#' * `col_sum()` returns the sum of each column.
#' * `col_max()` returns the maximum of each column.
#' * `col_min()` returns the minimum of each column.
#'
#' @examples
#' \donttest{
#' x <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 3)
#'
#' # Column-wise sum
#' col_sum(x)
#'
#' # Column-wise maximum
#' col_max(x)
#'
#' # Column-wise minimum (preserve NA)
#' col_min(x, na.rm = FALSE)
#' }
#'
#' @export
col_sum <- function(x, na.rm = TRUE) .Call(ColSum, x, na.rm)

#' @rdname col_sum
#' @export
col_max <- function(x, na.rm = TRUE) .Call(ColMax, x, na.rm)

#' @rdname col_sum
#' @export
col_min <- function(x, na.rm = TRUE) .Call(ColMin, x, na.rm)

#' Row- and column-wise differences
#'
#' Compute successive differences within a numeric matrix, similar to
#' [base::diff()] but applied along rows or columns:
#'
#' * `row_diff()` computes differences between successive rows, column by column.
#' * `col_diff()` computes differences between successive columns, row by row.
#'
#' @param x A numeric matrix.
#' @param na.rm Logical; if `TRUE`, missing values (`NA`/`NaN`) are not removed
#'   in pairwise differences. Both elements of a pair must be non-missing to
#'   yield a non-missing difference.
#'
#' @return A numeric matrix with one fewer row (`row_diff()`) or one fewer column
#'   (`col_diff()`) than `x`, containing the differences between successive
#'   elements along the specified dimension.
#'
#' @examples
#' \donttest{
#' x <- matrix(1:9, nrow = 3)
#'
#' # Differences between successive rows
#' row_diff(x)
#'
#' # Differences between successive columns
#' col_diff(x)
#' }
#'
#' @export
row_diff <- function(x, na.rm = TRUE) .Call(RowDiff, x, na.rm)

#' @rdname row_diff
#' @export
col_diff <- function(x, na.rm = TRUE) .Call(ColDiff, x, na.rm)

#' Rotate a matrix by 90, 180, or 270 degrees
#'
#' Rotate a numeric or character matrix clockwise by a specified angle.
#' Only 90, 180, and 270 degree rotations are supported. Dimension names
#' (row and column names) are preserved and reordered appropriately.
#'
#' @param x A matrix.
#' @param angle Integer. The rotation angle in degrees. Must be one of
#'   `90`, `180`, or `270`. Values are taken modulo 360.
#'
#' @return A matrix of the same mode as `x`, rotated by the specified angle,
#'   with appropriately reordered dimension names.
#'
#' @examples
#' \donttest{
#' # Rotate a matrix by 90 degrees
#' x <- matrix(1:9, nrow = 3)
#' rotate(x, 90)
#'
#' # Rotate a matrix by 180 degrees
#' x <- matrix(1:9, nrow = 3)
#' rotate(x, 180)
#'
#' # Rotate a matrix by 270 degrees
#' x <- matrix(1:9, nrow = 3)
#' rotate(x, 270)
#' }
#'
#' @export
rotate <- function(x, angle = c(90, 180, 270)) {
  .Call(Rotate, x, angle)
}

#' In-place, memory-efficient matrix multiplications
#'
#' Perform matrix multiplications **in place**:
#' the result is written back into the memory of the first matrix argument,
#' avoiding additional allocations. Useful for large matrices and
#' tight memory budgets.
#'
#' @section What “in place” means:
#' * The first matrix argument is **modified** directly.
#' * No new object is returned (called for side effects).
#' * Copy the matrix first if you need to preserve it.
#'
#' @name mat-ops-inplace
#' @param x,y,mat A numeric matrix (modified in place).
#' @param vec A numeric vector (row or column operand).
#' @param num A numeric scalar.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' x <- matrix(1:4, 2, 2)
#' y <- matrix(5:8, 2, 2)
#'
#' matXmat(x, y)       # matrix × matrix
#' matXrow(x, c(1, 2)) # matrix × row vector
#' matXcol(x, c(3, 4)) # matrix × column vector
#' matXnum(x, 10)      # matrix × scalar
#' }
NULL

#' @rdname mat-ops-inplace
#' @export
matXmat <- function(x, y) {
  assert_class(x, "matrix")
  assert_class(y, "matrix")
  if (is.integer(x) & is.numeric(y))
    x <- as.numeric(x)
  if (is.numeric(x) & is.integer(y))
    y <- as.numeric(y)
  invisible(.Call(MatXMat, x, y))
}

#' @rdname mat-ops-inplace
#' @export
matXrow <- function(mat, vec) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(vec))
    mat <- as.numeric(mat)
  if (is.numeric(mat) & is.integer(vec))
    vec <- as.numeric(vec)
  invisible(.Call(MatXRow, mat, vec))
}

#' @rdname mat-ops-inplace
#' @export
matXcol <- function(mat, vec) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(vec))
    mat <- as.numeric(mat)
  if (is.numeric(mat) & is.integer(vec))
    vec <- as.numeric(vec)
  invisible(.Call(MatXCol, mat, vec))
}

#' @rdname mat-ops-inplace
#' @export
matXnum <- function(mat, num) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(num))
    mat <- as.numeric(mat)
  if (is.numeric(mat) & is.integer(num))
    num <- as.numeric(num)
  invisible(.Call(MatXNum, mat, num))
}

#' Replace a column vector in a matrix (in place)
#'
#' Replace one or more columns of a matrix with a new vector, directly
#' modifying the matrix memory. This avoids reallocation and can be more
#' efficient for large matrices.
#'
#' @param mat A numeric matrix (modified in place).
#' @param col Column(s) to replace; either character names or integer indices.
#' @param vec A numeric vector to insert in place of the selected columns.
#'
#' @return No return value, called for side effects (the matrix is updated in place).
#'
#' @examples
#' \dontrun{
#' x <- matrix(as.numeric(1:6), 3, 2)
#' replace_vec_in_mat(x, 2, c(100, 200, 300)) # replace 2nd column
#' }
#'
#' @export
replace_vec_in_mat <- function(mat, col, vec) {
  if (is.character(col)) col <- index_cols(mat, col)
  invisible(.Call(ReplaceVecInMat, mat, col, vec))
}

# Sample matrix -----------------------------------------------------------

#' Create a zero matrix
#'
#' Generate a matrix filled with zeros of the specified dimensions.
#'
#' @param dim Integer vector giving the dimensions of the result.
#'
#' @return A numeric matrix of zeros with dimensions specified by `dim`.
#'
#' @examples
#' \donttest{
#' # 5x5 zero matrix
#' zeros(c(5, 5))
#' }
#'
#' @export
zeros <- function(dim) {
  array(0L, dim = dim)
}

#' Create a random matrix from specified values
#'
#' Generate a matrix by random sampling from a set of values.
#'
#' @param dim Integer vector giving the dimensions of the result.
#' @param x A numeric vector of values to sample from (default is `c(0L, 1L)`).
#' @param replace Logical; should sampling be with replacement? Defaults to `TRUE`.
#' @param prob A numeric vector of probability weights for sampling `x`.
#'
#' @return A numeric matrix sampled from `x` with dimensions specified by `dim`.
#'
#' @examples
#' \donttest{
#' # 5x5 random matrix of 0s and 1s
#' rands(c(5, 5))
#'
#' # 3x3 matrix sampled from 1:10 without replacement
#' rands(c(3, 3), x = 1:10, replace = FALSE)
#' }
#'
#' @export
rands <- function(dim, x = c(0L, 1L), replace = TRUE, prob = NULL) {
  array(sample(x, size = prod(dim), replace = replace, prob = prob), dim = dim)
}
