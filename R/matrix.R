#' max, min, sum on each row of a matrix
#'
#' Get max, min, sum of values on each row of a matrix.
#'
#' @param x A numeric matrix
#' @return A vector of min, max, sum of values on each row of a matrix
#'
#' @examples
#' # row max
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' row_max(x)}
#'
#' # row min
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' row_min(x)}
#'
#' # row sum
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' row_sum(x)}
#'
#' @export
row_max <- function(x) .Call(RowMax, x)

#' @rdname row_max
#' @export
row_min <- function(x) .Call(RowMin, x)

#' @rdname row_max
#' @export
row_sum <- function(x) .Call(RowSum, x)


#' max, min, sum on each column of a matrix
#'
#' Get max, min, sum of values on each column of a matrix.
#'
#' @param x A numeric matrix
#' @return A vector of min, max, sum of values on each column of a matrix
#'
#' @examples
#' # column max
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' col_max(x)}
#'
#' # column min
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' col_min(x)}
#'
#' # column sum
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' col_sum(x)}
#'
#' @export
col_max <- function(x) .Call(ColMax, x)

#' @rdname col_max
#' @export
col_min <- function(x) .Call(ColMin, x)

#' @rdname col_max
#' @export
col_sum <- function(x) .Call(ColSum, x)


#' max, min, sum by row names
#'
#' max, min, sum of values on each row name of a matrix.
#'
#' @param x A numeric matrix
#' @param na.rm a boolean value removing NA or not
#'
#' @return A matrix of minimum values shrinked by row names
#'
#' @examples
#' # max by row nm
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_rownames(x)}
#'
#' # min by row nm
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_rownames(x)}
#'
#' # sum by row nm
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_rownames(x)}
#'
#' @export
max_by_rownames <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g); minval <- min(x)
  .Call(MaxByRowNames, x, g, uniqueg, na.rm, minval)
}

#' @rdname max_by_rownames
#' @export
min_by_rownames <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g); maxval <- max(x)
  .Call(MinByRowNames, x, g, uniqueg, na.rm, maxval)
}

#' @rdname max_by_rownames
#' @export
sum_by_rownames <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g)
  .Call(SumByRowNames, x, g, uniqueg, na.rm)
}


#' max, min, sum by column names
#'
#' max, min, sum of values on each column name of a matrix.
#'
#' @param x A numeric matrix
#' @param na.rm a boolean value removing NA or not
#'
#' @return A matrix of minimum values shrinked by column names
#'
#' @examples
#' # max by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_colnames(x)}
#'
#' # min by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_colnames(x)}
#'
#' # sum by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_colnames(x)}
#'
#' @export
max_by_colnames <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g); minval <- min(x)
  .Call(MaxByColNames, x, g, uniqueg, na.rm, minval)
}

#' @rdname max_by_colnames
#' @export
min_by_colnames <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g); maxval <- max(x)
  .Call(MinByColNames, x, g, uniqueg, na.rm, maxval)
}

#' @rdname max_by_colnames
#' @export
sum_by_colnames <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g)
  .Call(SumByColNames, x, g, uniqueg, na.rm)
}


#' max, min, sum by dim (row and column) names
#'
#' max, min, sum of values on each dim (row and column) name of a matrix.
#'
#' @param x A numeric matrix
#' @param na.rm a boolean value removing NA or not
#'
#' @return A matrix of minimum values shrinked by dim (row and column) names
#'
#' @examples
#' # max by dim names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_dimnames(x)}
#'
#' # min by dim names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_dimnames(x)}
#'
#' # sum by dim names
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dimnames(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_dimnames(x)}
#'
#' @export
max_by_dimnames <- function(x, na.rm = TRUE) {
  max_by_rownames(max_by_colnames(x, na.rm = na.rm), na.rm = na.rm)
}

#' @rdname max_by_dimnames
#' @export
min_by_dimnames <- function(x, na.rm = TRUE) {
  min_by_rownames(min_by_colnames(x, na.rm = na.rm), na.rm = na.rm)
}

#' @rdname max_by_dimnames
#' @export
sum_by_dimnames <- function(x, na.rm = TRUE) {
  sum_by_rownames(sum_by_colnames(x, na.rm = na.rm), na.rm = na.rm)
}


#' rotate a matrix
#'
#' Rotate a matrix.
#'
#' @param x A matrix
#' @param angle A numeric value specifying rotation angle (90, 180, 270)
#'
#' @return A rotation matrix
#'
#' @examples
#' # 90 degree rotation
#' \donttest{x <- matrix(1:9, nrow = 3)
#' rotate(x, 90)}
#'
#' # 180 degree rotation
#' \donttest{x <- matrix(1:9, nrow = 3)
#' rotate(x, 180)}
#'
#' # 270 degree rotation
#' \donttest{x <- matrix(1:9, nrow = 3)
#' rotate(x, 270)}
#'
#' @export
rotate <- function(x, angle = c(90, 180, 270)) {
  z <- .Call(Rotate, x, angle)
  if (angle[1L] %% 360 == 90) {
    dn <- dimnames(x)
    dn <- rev(dn)
    dn[[2L]] <- rev(dn[[2L]])
    set_dimnames(z, dn)
  } else if (angle[1L] %% 360 == 180) {
    dn <- dimnames(x)
    dn <- lapply(dn, rev)
    set_dimnames(z, dn)
  } else if (angle[1L] %% 360 == 270) {
    dn <- dimnames(x)
    dn[[2L]] <- rev(dn[[2L]])
    dn <- rev(dn)
    set_dimnames(z, dn)
  }
  return(z)
}


#' Matrix X Matrix
#'
#' Multiply matrices and allocate the result to a first matrix memory.
#'
#' @param x a numeric matrix
#' @param y a numeric matrix
#' @return no return value
#'
#' @export
matXmat <- function(x, y) {
  assert_class(x, "matrix")
  assert_class(y, "matrix")
  if (is.integer(x) & is.numeric(y))
    x <- as_numeric(x)
  if (is.numeric(x) & is.integer(y))
    y <- as_numeric(y)
  invisible(.Call(MatXMat, x, y))
}

#' Matrix X Row vector
#'
#' Multiply a matrix and a row vector and allocate the result to the matrix memory.
#'
#' @param mat a numeric matrix
#' @param vec a numeric vector
#' @return no return value
#'
#' @export
matXrow <- function(mat, vec) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(vec))
    mat <- as_numeric(mat)
  if (is.numeric(mat) & is.integer(vec))
    vec <- as_numeric(vec)
  invisible(.Call(MatXRow, mat, vec))
}

#' Matrix X Column vector
#'
#' Multiply a matrix and a column vector and allocate the result to the matrix memory.
#'
#' @param mat a numeric matrix
#' @param vec a numeric vector
#' @return no return value
#'
#' @export
matXcol <- function(mat, vec) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(vec))
    mat <- as_numeric(mat)
  if (is.numeric(mat) & is.integer(vec))
    vec <- as_numeric(vec)
  invisible(.Call(MatXCol, mat, vec))
}

#' Matrix X Number
#'
#' Multiply a matrix and a numeric value and allocate the result to the matrix memory.
#'
#' @param mat a numeric matrix
#' @param num a numeric value
#' @return no return value
#'
#' @export
matXnum <- function(mat, num) {
  assert_class(mat, "matrix")
  if (is.integer(mat) & is.numeric(num))
    mat <- as_numeric(mat)
  if (is.numeric(mat) & is.integer(num))
    num <- as_numeric(num)
  invisible(.Call(MatXNum, mat, num))
}

#' Replace vectors in Matrix
#'
#' Replace vectors in Matrix
#'
#' @param mat a matrix
#' @param col string names of columns or numeric order numbers of columns
#' @param vec a vector to be replaced
#' @return no return value
#'
#' @export
replace_vec_in_mat <- function(mat, col, vec) {
  if (is.character(col))
    col <- icol(mat, col)
  invisible(.Call(ReplaceVecInMat, mat, col, vec))
}


# sample matrix -----------------------------------------------------------

#' Zeros
#'
#' Sample matrix containing only zeros
#'
#' @param dim a numeric vector specifying dimensions
#' @return a matrix with zeros
#'
#' @examples
#' # zeros
#' zeros(c(5, 5))
#'
#' @export
zeros <- function(dim) {
  array(0L, dim = dim)
}

#' Rands
#'
#' Generate a sample matrix with specific numbers randomly
#'
#' @param dim a numeric vector specifying dimensions
#' @param x a numeric vector specifying numbers
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @return a matrix with specific numbers
#'
#' @examples
#' # rands
#' rands(c(5, 5))
#'
#' @export
rands <- function(dim, x = c(0L, 1L), replace = TRUE, prob = NULL) {
  array(sample(x, size = prod(dim), replace = replace, prob = prob), dim = dim)
}

