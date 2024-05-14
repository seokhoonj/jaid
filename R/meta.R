#' Meta information
#'
#' Show meta information
#'
#' @param x object vector, data.frame, environment and etc
#' @return meta information data frame
#'
#' @examples
#' \donttest{meta(cars)}
#'
#' @export
meta <- function(x) UseMethod("meta")

#' @method meta data.frame
#' @export
meta.data.frame <- function(x) {
  column <- names(x)
  class <- sapply(x, class)
  if (is.list(class))
    class <- sapply(class, function(x) paste(x, collapse = ","))
  type <- sapply(x, typeof)
  nrows <- nrow(x)
  n <- sapply(x, function(x) sum(!is.na(x)))
  missing <- sapply(x, function(x) sum(is.na(x)))
  zero <- sapply(x, function(x) sum(x == 0, na.rm = TRUE))
  distinct <- sapply(x, unilen)
  mode <- sapply(x, function(s) mostfreq(s, na.rm = TRUE))
  df <- data.table(column, class, type, n, missing, zero, distinct,
                   prop = 1 - missing/nrows, nzprop = 1 - zero/nrows, mode)
  data.table::setattr(df, "class", c("meta", class(df)))
  return(df)
}


#' Class and type information
#'
#' Show class and type information.
#'
#' @param x object vector, data.frame, environment and etc
#' @return class and type information data frame
#'
#' @examples
#' \donttest{type(cars)}
#'
#' @export
type <- function(x) UseMethod("type")

#' @method type data.frame
#' @export
type.data.frame <- function(x) {
  column <- names(x)
  class <- sapply(x, class)
  type <- sapply(x, typeof)
  data.table(column, class, type)
}
