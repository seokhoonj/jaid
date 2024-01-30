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
  if (angle %% 360 == 90) {
    dn <- dimnames(x)
    dn <- rev(dn)
    dn[[2L]] <- rev(dn[[2L]])
    set_dimnames(z, dn)
  } else if (angle %% 360 == 180) {
    dn <- dimnames(x)
    dn <- lapply(dn, rev)
    set_dimnames(z, dn)
  } else if (angle %% 360 == 270) {
    dn <- dimnames(x)
    dn[[2L]] <- rev(dn[[2L]])
    dn <- rev(dn)
    set_dimnames(z, dn)
  }
  return(z)
}
