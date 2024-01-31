#' Object size
#'
#' Calculates the object size
#'
#' @param x object vector, data.frame, environment and etc
#' @param unit size unit `B`, `KB`, `MB`, `GB` default `KB`
#' @examples
#' \dontrun{
#' sizeof(x = sys.frame(), unit = "kb")
#' }
#' @export
sizeof <- function(x, unit) UseMethod("sizeof")

#' @method sizeof default
#' @export
sizeof.default <- function(x, unit = "mb") {
  class <- paste(class(x), collapse = ",")
  type  <- typeof(x)
  size  <- object.size(x)[1L]
  power <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.frame(
    class = class,
    type  = type,
    size  = round(size / (2^10)^power, 3),
    unit  = toupper(unit)
  )
}

#' @method sizeof data.frame
#' @export
sizeof.data.frame <- function(x, unit = "mb") {
  col   <- names(x)
  class <- sapply(x, function(s) paste(class(s), collapse = ","))
  type  <- sapply(x, typeof)
  size  <- c(lapply(x, function(s) object.size(s)), total = object.size(x))
  power <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.frame(
    col   = c(col,   "total"),
    class = c(class, total = "total"),
    type  = c(type,  total = "total"),
    size  = round(unlist(size) / (2^10)^power, 3),
    unit  = toupper(unit)
  )
}

#' @method sizeof environment
#' @export
sizeof.environment <- function(x, unit = "mb") {
  x <- sys.frame()
  env <- ls(all.names = TRUE, envir = x)
  if (length(env) == 0L) stop("Object not found.")
  class <- sapply(env, function(s) paste(class(get(s, envir = x)), collapse = ","))
  size  <- sapply(env, function(s) object.size(get(s, envir = x)))
  sizes <- c(size, total = sum(size))
  power <- switch(tolower(unit), b = 0, kb = 1, mb = 2, gb = 3)
  data.frame(
    obj   = c(names(size), total = "total"),
    class = c(class, total = "total"),
    size  = round(sizes / (2^10)^power, 3),
    unit  = toupper(unit)
  )
}

