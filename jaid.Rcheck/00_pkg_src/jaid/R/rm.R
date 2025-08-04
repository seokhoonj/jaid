#' Remove columns
#'
#' Remove columns in a memory efficient way.
#'
#' @param df a data.frame
#' @param cols name of columns to remove (like .(x, y), list(x, y))
#' @return no return value
#'
#' @examples
#' # remove columns
#' \donttest{data <- mtcars
#' pryr::address(data)
#' rm_cols(data, .(mpg, cyl, disp))
#' pryr::address(data)}
#'
#' @export
rm_cols <- function(df, cols) {
  old_class <- class(df)
  set_dt(df)
  cols <- match_cols(df, sapply(rlang::enexpr(cols), rlang::as_name))
  df[, `:=`((cols), NULL)]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}
