#' Is a null external pointer?
#'
#' Is a null external pointer?
#'
#' @param pointer an externalptr object
#' @return a logical value whether it is null external pointer or not.
#'
#' @examples
#' # is null external pointer?
#' \dontrun{
#' p <- new("externalptr")
#' is.null.externalptr(p)}
#'
#' @export
is.null.externalptr <- function(pointer) {
  if (!inherits(pointer, "externalptr"))
    stop("Not a pointer")
  .Call(IsNullExternalPtr, pointer)
}

#' Set external pointer
#'
#' Set external pointer
#'
#' @param df a data.frame
#' @return No return value.
#'
#' @examples
#' # set pointer
#' \donttest{set_ptr(iris)}
#'
#' @export
set_ptr <- function(df) {
  if (!has_ptr(df)) {
    n <- sys.nframe()
    df_name <- rlang::as_name(rlang::enquo(df))
    old_class <- class(df)
    data.table::setalloccol(df)
    set_attr(df, "class", old_class)
    assign(df_name, df, envir = parent.frame(n))
  }
}

#' Get external pointer
#'
#' Get external pointer
#'
#' @param df a data.frame
#' @return No return value.
#'
#' @examples
#' # get pointer
#' \dontrun{
#' df <- data.frame(x = c(1:3), y = c("a", "b", "c"))
#' get_ptr(df)
#' df <- setalloccol(df)
#' get_ptr(df)}
#'
#' @export
get_ptr <- function(df)
  attr(df, ".internal.selfref")

#' Delete external pointer
#'
#' Delete external pointer
#'
#' @param df a data.frame
#' @return No return value.
#'
#' @examples
#' # delete pointer
#' \donttest{df <- data.frame(x = c(1:3), y = c("a", "b", "c"))
#' data.table::setalloccol(df)
#' get_ptr(df)
#' del_ptr(df)
#' get_ptr(df) # NULL}
#'
#' @export
del_ptr <- function(df)
  data.table::setattr(df, ".internal.selfref", NULL)

#' Has not a null pointer?
#'
#' Has not a null external pointer?
#'
#' @param df a data.frame
#' @param error_raise a logcial whether to raise an error or not
#' @return a logical value whether to have not a null external pointer or not
#'
#' @examples
#' # Has not null external pointer?
#' \donttest{has_ptr(iris, error_raise = FALSE)}
#'
#' @export
has_ptr <- function(df, error_raise = FALSE) {
  assert_class(df, "data.frame")
  df_name <- rlang::as_name(rlang::enquo(df))
  p <- get_ptr(df)
  rt <- TRUE
  if (is.null(p)) {
    rt <- !rt
  }
  else {
    if (is.null.externalptr(p))
      rt <- !rt
  }
  if (!error_raise)
    return(rt)
  if (!rt)
    stop("'", df_name, "'", " doesn't have a pointer.", call. = FALSE)
}
