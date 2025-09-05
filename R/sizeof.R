#' Object size utility
#'
#' Provides a unified way to inspect the memory footprint of R objects.
#' Supports atomic objects, data frames, data.tables, and environments.
#'
#' @param x An R object (e.g., vector, data frame, data.table, environment).
#' @param unit Character string giving the size unit. One of `"B"`, `"KB"`,
#'   `"MB"`, or `"GB"`. Case-insensitive. Defaults to `"MB"`.
#'
#' @details
#' This is an S3 generic with methods for different object types:
#'
#' * `sizeof.default()` — reports the total size of a single object.
#' * `sizeof.data.frame()` — reports the size of each column plus the total.
#' * `sizeof.data.table()` — like the data frame method, but returns a
#'   data.table.
#' * `sizeof.environment()` — reports the size of each object in the given
#'   environment plus the total.
#'
#' Internally uses [utils::object.size()].
#'
#' @return A data frame (or data.table) describing object size(s) in the chosen unit.
#'
#' @examples
#' \dontrun{
#' # Check the size of the current frame
#' sizeof(sys.frame(), unit = "KB")
#'
#' # Size of the iris data
#' sizeof(iris, unit = "MB")
#' }
#'
#' @export
sizeof <- function(x, unit) UseMethod("sizeof")

#' @method sizeof default
#' @export
sizeof.default <- function(x, unit = "MB") {
  class <- paste(class(x), collapse = ",")
  type  <- typeof(x)
  size  <- utils::object.size(x)[1L]
  power <- switch(toupper(unit), B = 0, KB = 1, MB = 2, GB = 3)
  data.frame(
    class = class,
    type  = type,
    size  = round(size / (2^10)^power, 3),
    unit  = toupper(unit)
  )
}

#' @method sizeof data.frame
#' @export
sizeof.data.frame <- function(x, unit = "MB") {
  column <- names(x)
  class  <- sapply(x, function(s) paste(class(s), collapse = ","))
  type   <- sapply(x, typeof)
  size   <- c(lapply(x, function(s) utils::object.size(s)),
              total = utils::object.size(x))
  power  <- switch(toupper(unit), B = 0, KB = 1, MB = 2, GB = 3)
  data.frame(
    column = c(column, total = "total"),
    class  = c(class , total = "total"),
    type   = c(type  , total = "total"),
    size   = round(unlist(size) / (2^10)^power, 3),
    unit   = toupper(unit)
  )
}

#' @method sizeof data.table
#' @export
sizeof.data.table <- function(x, unit = "MB") {
  column <- names(x)
  class  <- sapply(x, function(s) paste(class(s), collapse = ","))
  type   <- sapply(x, typeof)
  size   <- c(lapply(x, function(s) utils::object.size(s)),
              total = utils::object.size(x))
  power  <- switch(toupper(unit), B = 0, KB = 1, MB = 2, GB = 3)
  data.table::data.table(
    column = c(column, total = "total"),
    class  = c(class , total = "total"),
    type   = c(type  , total = "total"),
    size   = round(unlist(size) / (2^10)^power, 3),
    unit   = toupper(unit)
  )
}

#' @method sizeof environment
#' @export
sizeof.environment <- function(x, unit = "MB") {
  x <- sys.frame()
  env <- ls(all.names = TRUE, envir = x)
  if (length(env) == 0L) stop("Object not found.")
  class <- sapply(env, function(s) paste(class(get(s, envir = x)), collapse = ","))
  size  <- sapply(env, function(s) utils::object.size(get(s, envir = x)))
  sizes <- c(size, total = sum(size))
  power <- switch(toupper(unit), B = 0, KB = 1, MB = 2, GB = 3)
  data.frame(
    object = c(names(size), total = "total"),
    class  = c(class, total = "total"),
    size   = round(sizes / (2^10)^power, 3),
    unit   = toupper(unit)
  )
}

