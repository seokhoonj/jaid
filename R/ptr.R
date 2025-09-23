#' Is a NULL external pointer?
#'
#' Checks whether an object of type `externalptr` is a **NULL** external pointer.
#' This does **not** test for R's `NULL`, but for a C-level null pointer behind
#' an `externalptr`.
#'
#' @param ptr An externalptr object.
#'
#' @return A single logical value: `TRUE` if `ptr` is a NULL external pointer,
#'   otherwise `FALSE`.
#'
#' @examples
#' # is null external pointer?
#' \dontrun{
#' p <- new("externalptr")
#' is_null_externalptr(p)
#' }
#'
#' @seealso [get_ptr()], [drop_ptr()]
#'
#' @export
is_null_externalptr <- function(ptr) {
  if (!inherits(ptr, "externalptr"))
    stop("Not a pointer")
  .Call(IsNullExternalPtr, ptr)
}

#' Deprecated: set_ptr()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Set an external pointer.
#'
#' @param df A data.frame.
#' @param skip_shiny Logical; if `TRUE`, and a Shiny context is detected
#'   (via [.is_shiny_running()]), the function immediately returns
#'   `data.table::setDT(df)`
#' @return No return value. Called for side effects.
#'
#' @examples
#' \donttest{
#' set_ptr(iris)
#' }
#'
#' @export
set_ptr <- function(df, skip_shiny = TRUE) {
  lifecycle::deprecate_warn("0.0.0.9001", "set_ptr()", "data.table::setDT()")
  if (skip_shiny && .is_shiny_running())
    return(data.table::setDT(df))
  if (!has_ptr(df)) {
    # legacy code (problems in Shiny):
    n <- sys.nframe()
    df_name <- trace_arg_expr(df)
    old_class <- class(df)
    data.table::setalloccol(df)
    data.table::setattr(df, "class", old_class)
    assign(df_name, df, envir = parent.frame(n))
  }
}

#' Get data.table internal pointer
#'
#' Retrieves the internal data.table self-reference (`.internal.selfref`)
#' attribute from an object, if present.
#'
#' @param df A data.frame or data.table.
#'
#' @return The value of the `.internal.selfref` attribute, or `NULL`
#'   if it does not exist.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' get_ptr(df)  # usually NULL
#'
#' data.table::setalloccol(df)
#' get_ptr(df)  # now shows the internal pointer
#' }
#'
#' @seealso [drop_ptr()], [data.table::setalloccol()]
#'
#' @export
get_ptr <- function(df)
  attr(df, ".internal.selfref")

#' Drop data.table internal pointer
#'
#' Drops the internal data.table self-reference (`.internal.selfref`)
#' from an object. If the object is a `data.table`, the `"data.table"`
#' class is also dropped so it behaves as a plain `data.frame`.
#'
#' This function mutates `df` in place. It is safe to call even if
#' `.internal.selfref` is absent; in that case nothing happens.
#'
#' @param df A data.frame or data.table.
#'
#' @return Invisibly returns `df` for convenience.
#'
#' @examples
#' \dontrun{
#' # data.table (already has a selfref pointer)
#' dt <- data.table::data.table(x = 1:3, y = c("a","b","c"))
#' drop_ptr(dt) # drops only the pointer
#' class(dt)    # no longer contains "data.table"
#' attr(dt, ".internal.selfref", exact = TRUE)  # NULL
#'
#' # data.frame that has a selfref pointer
#' df <- data.frame(x = 1:3, y = c("a","b","c"))
#' data.table::setalloccol(df)  # attaches .internal.selfref
#' class(df)                    # still data.frame
#' drop_ptr(df)                 # drops only the pointer
#' attr(df, ".internal.selfref", exact = TRUE)  # NULL
#' }
#'
#' @seealso [data.table::setattr()], [data.table::setalloccol()]
#'
#' @export
drop_ptr <- function(df) {
  assert_class(df, "data.frame")
  if (inherits(df, "data.table")) {
    new_class <- setdiff(class(df), "data.table")
    data.table::setattr(df, "class", new_class)
  }
  if (!is.null(attr(df, ".internal.selfref", exact = TRUE))) {
    data.table::setattr(df, ".internal.selfref", NULL)
  }
  invisible(df)
}

#' Has a non-null data.table internal pointer?
#'
#' Checks whether `df` carries a valid (non-NULL) data.table self-reference
#' external pointer stored in the `.internal.selfref` attribute.
#'
#' @param df A data.frame or data.table.
#' @param error_raise Logical; if `TRUE`, raise an error when the pointer is
#'   absent or NULL. Default is `FALSE`.
#' @return `TRUE` if a non-null external pointer exists, otherwise `FALSE`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:3)
#' has_ptr(df)  # typically FALSE
#'
#' dt <- data.table::data.table(x = 1:3)
#' has_ptr(dt)  # TRUE if pointer attached
#'
#' # With error
#' has_ptr(iris, error_raise = TRUE)  # errors if no pointer
#' }
#'
#' @seealso [get_ptr()], [drop_ptr()]
#'
#' @export
has_ptr <- function(df, error_raise = FALSE) {
  assert_class(df, "data.frame")
  p <- get_ptr(df)
  rt <- TRUE
  if (is.null(p)) {
    df_name <- trace_arg_expr(df)
    if (error_raise)
      stop("'", df_name, "' doesn't have a valid pointer (.internal.selfref).",
           call. = FALSE)
    return(!rt)
  } else {
    if (!is_null_externalptr(p))
      return(rt)
  }
}

#' Deprecated: get_copied_dt()
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Convert a data.frame to data.table
#'
#' @param df A data.frame.
#' @return A data.table copied.
#'
#' @examples
#' \donttest{
#' df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' get_copied_dt(df)
#' }
#'
#' @export
get_copied_dt <- function(df) {
  lifecycle::deprecate_warn(
    "0.0.0.9001", "get_copied_dt()", "data.table::copy()"
  )
  return(data.table::setDT(data.table::copy(df))[])
}
