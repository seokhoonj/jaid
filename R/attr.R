#' Set attributes
#'
#' A wrapper for `data.table::setattr()` to set object attributes.
#'
#' @param x Any R object (e.g., `list`, `data.frame`, `data.table`)
#' @param name Character string specifying the attribute name
#' @param value Value to assign to the attribute, or `NULL` to remove the
#'   attribute if it exists
#'
#' @return Returns the modified object invisibly for use in compound statements
#'
#' @examples
#' \donttest{
#' # Set attributes
#' df <- data.frame(a = 1:3, b = 4:6)
#' set_attr(df, "flag", TRUE)
#' attr(df, "flag")
#' }
#'
#' @export
set_attr <- function(x, name, value)
  data.table::setattr(x, name, value)

#' Check if object has specific attributes
#'
#' Determines whether an object has all specified attributes.
#'
#' @param df An R object to check for attributes.
#' @param attr Character vector of attribute names to check for.
#' @param error_raise Logical; if `TRUE`, raises an error when any attributes
#'   are missing, if `FALSE` returns logical result.
#'
#' @return Logical value: `TRUE` if all specified attributes exist,
#'   `FALSE` otherwise
#'
#' @examples
#' \donttest{
#' # Check if mtcars has specific attributes
#' has_attr(mtcars, c("names", "class"))
#' }
#'
#' @export
has_attr <- function(df, attr, error_raise = FALSE) {
  df_name <- trace_arg_expr(df)
  df_attr <- names(attributes(df))
  diff_attr <- setdiff(attr, df_attr)
  rt <- length(diff_attr) == 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have attribute(s): ",
         paste0(diff_attr, collapse = ", "), ".",
         call. = FALSE)
  }
}

#' Find matching attributes
#'
#' Returns the names of attributes in `x` that match the specified names.
#'
#' @param x Any R object (e.g., `list`, `data.frame`, `data.table`).
#' @param name Character vector of attribute names to match.
#'
#' @return A character vector of matching attribute names.
#'
#' @examples
#' \donttest{
#' # Find attributes by name
#' match_attr(iris, c("class", "names"))
#' }
#'
#' @export
match_attr <- function(x, name)
  names(attributes(x))[match(name, names(attributes(x)), 0L)]

#' Find attributes by regular expression
#'
#' Return the names of attributes in `x` whose names match a regular expression pattern.
#'
#' @param x Any R object (e.g., `list`, `data.frame`, `data.table`).
#' @param name A character string containing a regular expression pattern
#'   to match against attribute names.
#'
#' @return A character vector of matching attribute names.
#'
#' @examples
#' \donttest{
#' # Find attributes by regex
#' regex_attr(iris, "class|names")
#' }
#'
#' @export
regex_attr <- function(x, name) {
  names(attributes(x))[grepl(name, names(attributes(x)))]
}

#' Set class for matching attributes
#'
#' Set the class for attributes that match specified names or patterns.
#'
#' @param x Any R object (e.g., list, data.frame, data.table).
#' @param name Attribute names or pattern to match.
#' @param class Character vector specifying the class to assign.
#'   (e.g., c("data.table", "data.frame"))
#' @param regex Logical; if `TRUE` (default), treats `name` as a regular
#'   expression pattern, if `FALSE` treats it as exact attribute names.
#'
#' @return Returns invisibly; called for side effects.
#'
#' @examples
#' \donttest{
#' # Set class of matched attribute
#' df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' attr(df, "summary") <- data.frame(a = 4:6, b = c("d", "e", "f"))
#' attr(df, "summary")
#' set_attr_class(df, "sum", c("tbl_df", "tbl", "data.frame"), regex = TRUE)
#' attr(df, "summary")
#' }
#'
#' @export
set_attr_class <- function(x, name, class, regex = TRUE) {
  nms <- if (regex) regex_attr(x, name) else match_attr(x, name)
  invisible(lapply(nms, function(s)
      data.table::setattr(attr(x, s), "class", class)
  ))
}
