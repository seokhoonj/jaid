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
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_row_nm(x)}
#'
#' # min by row nm
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_row_nm(x)}
#'
#' # sum by row nm
#' \donttest{x <- matrix(c(1:9), nrow = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_row_nm(x)}
#'
#' @export
max_by_row_nm <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g); minval <- min(x)
  .Call(MaxByRowNm, x, g, uniqueg, na.rm, minval)
}

#' @rdname max_by_row_nm
#' @export
min_by_row_nm <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g); maxval <- max(x)
  .Call(MinByRowNm, x, g, uniqueg, na.rm, maxval)
}

#' @rdname max_by_row_nm
#' @export
sum_by_row_nm <- function(x, na.rm = TRUE) {
  g <- rownames(x); uniqueg <- unique(g)
  .Call(SumByRowNm, x, g, uniqueg, na.rm)
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
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_col_nm(x)}
#'
#' # min by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_col_nm(x)}
#'
#' # sum by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_col_nm(x)}
#'
#' @export
max_by_col_nm <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g); minval <- min(x)
  .Call(MaxByColNm, x, g, uniqueg, na.rm, minval)
}

#' @rdname max_by_col_nm
#' @export
min_by_col_nm <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g); maxval <- max(x)
  .Call(MinByColNm, x, g, uniqueg, na.rm, maxval)
}

#' @rdname max_by_col_nm
#' @export
sum_by_col_nm <- function(x, na.rm = TRUE) {
  g <- colnames(x); uniqueg <- unique(g)
  .Call(SumByColNm, x, g, uniqueg, na.rm)
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
#' # max by dim nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' max_by_dim_nm(x)}
#'
#' # min by dim nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' min_by_dim_nm(x)}
#'
#' # sum by col nm
#' \donttest{x <- matrix(c(1:9), ncol = 3)
#' set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#' sum_by_dim_nm(x)}
#'
#' @export
max_by_dim_nm <- function(x, na.rm = TRUE) {
  max_by_row_nm(max_by_col_nm(x, na.rm = na.rm), na.rm = na.rm)
}

#' @rdname max_by_dim_nm
#' @export
min_by_dim_nm <- function(x, na.rm = TRUE) {
  min_by_row_nm(min_by_col_nm(x, na.rm = na.rm), na.rm = na.rm)
}

#' @rdname max_by_dim_nm
#' @export
sum_by_dim_nm <- function(x, na.rm = TRUE) {
  sum_by_row_nm(sum_by_col_nm(x, na.rm = na.rm), na.rm = na.rm)
}
