#' As functions
#'
#' As logical, integer, numeric, character maintaining the original shape.
#'
#' @param x a vector or matrix
#' @return a converted vector or matrix
#'
#' @export
as_numeric <- function(x) .Call(AsNumeric, x)

#' @rdname as_numeric
#' @export
as_logical <- function(x) .Call(AsLogical, x)

#' @rdname as_numeric
#' @export
as_integer <- function(x) .Call(AsInteger, x)

#' @rdname as_numeric
#' @export
as_double <- function(x) .Call(AsDouble, x)

#' @rdname as_numeric
#' @export
as_character <- function(x) .Call(AsCharacter, x)
