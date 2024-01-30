#' Meta information
#'
#' Show meta information
#'
#' @param x object vector, data.frame, environment and etc
#' @return meta information data frame
#'
#' @examples
#' \donttest{meta(cars)}
#' @export
meta <- function(x) UseMethod("meta")

#' @method meta data.frame
#' @export
meta.data.frame <- function(x) {
  assert_class(x, "data.frame")
  column <- names(x)
  class <- sapply(x, class)
  type <- sapply(x, typeof)
  nrows <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  zero <- sapply(x, function(x) sum(x == 0, na.rm = TRUE))
  distinct <- sapply(x, function(s) length(unique(s)))
  mode <- sapply(x, function(x) fastModeX(x)[1L])
  data.frame(column, class, type, n, missing, zero, distinct,
             prop = 1 - missing/nrows, nzprop = 1 - zero/nrows, mode)
}
