#' Set attributes
#'
#' setattr functions re-exported from `data.table`.
#'
#' @param x any objects; e.g, list, columns of a data.frame or data.table
#' @param name the character attribute name.
#' @param value the value to assign to the attribute or `NULL` removes teh attribute, if present.
#' @return the changed object (invisibly) for use in compound statements.
#'
#' @examples
#' # set attributes
#' \dontrun{df <- data.frame(a = 1:3, b = 4:6)
#' set_attr(df, "flag", TRUE)
#' attr(df, "flag")}
#'
#' @export
set_attr <- function(x, name, value)
  data.table::setattr(x, name, value)

#' Has attributes
#'
#' Whether the data has specific attributes
#'
#' @param df a data frame
#' @param attr attribute names
#' @param error_raise a boolean whether to raise an error or not
#' @return a boolean value
#'
#' @examples
#' # has attributes
#' \donttest{has_attr(mtcars, c("names", "class"))}
#'
#' # raise an error
#' \dontrun{
#' has_attr(mtcars, c("names", "types"), error_raise = TRUE)}
#'
#' @export
has_attr <- function(df, attr, error_raise = TRUE) {
  df_name <- desub(df)
  df_attr <- names(attributes(df))
  diff_attr <- setdiff(attr, df_attr)
  rt <- length(diff_attr) == 0
  if (!error_raise)
    return(rt)
  if (!rt) {
    stop("'", df_name, "' doesn't have attributes(s): ",
         paste0(diff_attr, collapse = ", "), ".",
         call. = FALSE)
  }
}

#' Matched attributes
#'
#' Find matched attributes.
#'
#' @param x any objects; e.g, list, columns of a data.frame or data.table
#' @param name the character attribute names.
#' @return matched attributes' names
#'
#' @examples
#' # matched attributes
#' \donttest{match_attr(iris, c("class", "names"))}
#'
#' @export
match_attr <- function(x, name)
  names(attributes(x))[match(name, names(attributes(x)), 0L)]

#' Matched attributes using regular expressions
#'
#' Find matched attributes using regular expressions.
#'
#' @param x any objects; e.g, list, columns of a data.frame or data.table
#' @param name the regular expression attribute names.
#' @return matched attributes' names
#'
#' @examples
#' # matched attributes using regular expressions
#' \donttest{regex_attr(iris, "class|names")}
#'
#' @export
regex_attr <- function(x, name)
  names(attributes(x))[grepl(name, names(attributes(x)))]

#' Set matched attributes' class
#'
#' Set matched attributes' class.
#'
#' @param x any objects; e.g, list, columns of a data.frame or data.table
#' @param name the attribute names.
#' @param class a string specifying class; e.g, c("data.table", "data.frame")
#' @param regex a logical specifying attributes finding method (a string vector or regular expression),
#' regex = `TRUE` is a default
#' @return matched attributes' names
#'
#' @examples
#' # set matched attributes' class
#' \donttest{df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' attr(df, "summary") <- data.frame(a = 4:6, b = c("d", "e", "f"))
#' attr(df, "summary")
#' set_attr_class(df, "sum", c("tbl_df", "tbl", "data.frame"), regex = TRUE)
#' attr(df, "summary")}
#'
#' @export
set_attr_class <- function(x, name, class, regex = TRUE) {
  nms <- if(regex) regex_attr(x, name) else match_attr(x, name)
  invisible(lapply(nms, function(s)
    data.table::setattr(attr(x, s), "class", class)
  ))
}

