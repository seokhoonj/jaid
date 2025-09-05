#' Convert a vector into a row or column vector
#'
#' These functions reshape a one-dimensional vector into either a row vector
#' (1 × *n*) or a column vector (*n* × 1).
#'
#' @param x A vector.
#'
#' @return
#' A matrix with either one row (`rowvec()`) or one column (`colvec()`),
#' preserving names where available.
#'
#' @examples
#' \donttest{
#' # Convert into a row vector
#' rowvec(c(1, 2, 3, 4, 5))
#'
#' # Convert into a column vector
#' colvec(c(1, 2, 3, 4, 5))
#' }
#'
#' @export
rowvec <- function(x) array(x, dim = c(1L, length(x)), dimnames = list(NULL, names(x)))

#' @rdname rowvec
#' @export
colvec <- function(x) array(x, dim = c(length(x), 1L), dimnames = list(names(x), NULL))

#' Count unique elements
#'
#' Returns the number of distinct values in a vector. This is a lightweight,
#' fast alternative to `length(unique(x))`, implemented in C for performance.
#'
#' @param x A vector.
#'
#' @return An integer scalar: the count of distinct values in `x`.
#'
#' @examples
#' \donttest{
#' # Length of unique vector
#' x <- c(1, 1, 2, 3, 4, 5, 5)
#' unilen(x)
#' }
#'
#' @export
unilen <- function(x) .Call(Unilen, x)

#' Reverse the order of elements in a vector
#'
#' Returns the input vector with its elements in reverse order.
#'
#' @param x A vector.
#' @return A vector of the same type as `x`, with elements in reverse order.
#'
#' @examples
#' \donttest{
#' # Reverse a numeric vector
#' x <- c(1:10)
#' reverse(x)
#' }
#'
#' @export
reverse <- function(x) invisible(.Call(Reverse, x))

#' Interleave two vectors
#'
#' Combine two vectors by interleaving their elements. That is, take one element
#' from `x`, then one from `y`, alternating until both vectors are exhausted.
#'
#' @param x A vector.
#' @param y A vector.
#' @return A vector containing the elements of `x` and `y` interleaved.
#'   The type will follow the usual R coercion rules when combining vectors.
#'
#' @examples
#' # Interleave two numeric vectors
#' \donttest{x <- c(1, 3, 5, 7)
#' y <- c(2, 4, 6, 8)
#' interleave(x, y)}
#'
#' @export
interleave <- function(x, y) .Call(Interleave, x, y)

#' Most frequent value (mode, modal value)
#'
#' Returns the most frequently occurring value (the statistical mode) in a vector.
#'
#' @param x A vector.
#' @param na.rm Logical. Should missing values (`NA`) be removed? Defaults to FALSE.
#'
#' @return The most frequent value vector and its frequency
#'
#' @examples
#' # Get the most frequent values
#' x <- c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
#' mostfreq(x)
#'
#' @export
mostfreq <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  if (inherits(x, "character"))
    x <- x[x != ""]
  if (inherits(x, "Date"))
    x <- as.character(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#' Generate a list of sequential vectors
#'
#' Create a list where each element is a sequence generated from the
#' corresponding elements of `from` and `to`.
#'
#' @param from A numeric vector giving the starting values of the sequences.
#' @param to A numeric vector giving the end values of the sequences. Must be the same length as `from`.
#' @param by A numeric value giving the increment of the sequences. Defaults to 1.
#'
#' @return A list of numeric vectors, each containing a sequence from
#'   the corresponding elements of `from` to `to`.
#'
#' @examples
#' \donttest{
#' # Generate sequences 1:3, 2:5, and 3:7
#' seq_list(from = c(1, 2, 3), to = c(3, 5, 7))
#'
#' # Using a custom step size
#' seq_list(from = c(1, 5), to = c(3, 9), by = 2)
#' }
#'
#' @export
seq_list <- function(from, to, by = 1L) {
  if (length(from) != length(to))
    stop("`from` and `to` must have the same length.")
  # lapply(seq_along(from), function(x) seq(from[x], to[x], by))
  mapply(seq, from, to, MoreArgs = list(by = by), SIMPLIFY = FALSE)
}

# To be updated -----------------------------------------------------------

before_change_index <- function(x) {
  lifecycle::signal_stage("experimental", "before_change_index()")
  .Call(BeforeChangeIndex, x)
}
