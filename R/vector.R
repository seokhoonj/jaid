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
