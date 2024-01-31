#' length of a unique vector
#'
#' Calculate length of a unique vector.
#'
#' @param x A vector
#'
#' @return A length of a unique vector
#'
#' @examples
#' # length of unique vector
#' \donttest{x <- c(1, 1, 2, 3, 4, 5, 5)
#' unilen(x)}
#'
#' @export
unilen <- function(x) .Call(Unilen, x)

#' reverse a vector
#'
#' Reverse a vector directly.
#'
#' @param x A vector
#'
#' @return A reversed vector
#'
#' @examples
#' # reverse a vector
#' \donttest{x <- c(1:10)
#' reverse(x)}
#'
#' @export
reverse <- function(x) invisible(.Call(Reverse, x))

#' traverse two vectors
#'
#' Intersect the elements of two vectors.
#'
#' @param x A vector
#' @param y A vector
#'
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
mostfreq <- function(x, na.rm = FALSE) fastModeX(x, na.rm)
