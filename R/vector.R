#' Change vector into row vector or column vector
#'
#' Change vector into row vector or column vector.
#'
#' @param x a vector
#' @return a column vector or row vector.
#'
#' @examples
#' # change into column vector
#' rowvec(c(1, 2, 3, 4, 5))
#' colvec(c(1, 2, 3, 4, 5))
#'
#' @export
rowvec <- function(x) array(x, dim = c(1L, length(x)), dimnames = list(NULL, names(x)))

#' @rdname rowvec
#' @export
colvec <- function(x) array(x, dim = c(length(x), 1L), dimnames = list(names(x), NULL))

#' Length of a unique vector
#'
#' Calculate length of a unique vector.
#'
#' @param x A vector
#' @return A length of a unique vector
#'
#' @examples
#' # length of unique vector
#' \donttest{x <- c(1, 1, 2, 3, 4, 5, 5)
#' unilen(x)}
#'
#' @export
unilen <- function(x) .Call(Unilen, x)

#' Reverse a vector
#'
#' Reverse a vector directly.
#'
#' @param x A vector
#' @return A reversed vector
#'
#' @examples
#' # reverse a vector
#' \donttest{x <- c(1:10)
#' reverse(x)}
#'
#' @export
reverse <- function(x) invisible(.Call(Reverse, x))

#' Traverse two vectors
#'
#' Intersect the elements of two vectors.
#'
#' @param x A vector
#' @param y A vector
#' @return A combined vector
#'
#' @examples
#' # traverse two vectors
#' \donttest{x <- c(1, 3, 5, 7)
#' y <- c(2, 4, 6, 8)
#' traverse(x, y)}
#'
#' @export
traverse <- function(x, y) .Call(Traverse, x, y)

#' Most frequent value (mode, modal value)
#'
#' Get the most frequent value.
#'
#' @param x A vector
#' @param na.rm A boolean value removing na or not
#'
#' @return the most frequent value vector and its frequency
#'
#' @examples
#' # get the most frequent values
#' \donttest{x <- c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
#' mostfreq(x)}
#'
#' @export
mostfreq <- function(x, na.rm = FALSE) {
  # fastModeX(x, na.rm) is so slow for 10 million vector.
  # need to find a different way
  if (na.rm)
    x <- x[!is.na(x)]
  if (inherits(x, "character"))
    x <- x[x != ""]
  if (inherits(x, "Date"))
    x <- as.character(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


before_change_index <- function(x) .Call(BeforeChangeIndex, x)
