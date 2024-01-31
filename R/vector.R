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
#' \donttest{x <- y <- c(1:10)
#' traverse(x, y)}
#'
#' @export
traverse <- function(x, y) .Call(Traverse, x, y)
