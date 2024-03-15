#' Draw a line
#'
#' Draw a line in console.
#'
#' @param width a numeric vector specifying width (default: `getOption("width")`)
#' @param mark a string specifying mark (default: "=")
#' @return a string vector with repeated mark
#'
#' @examples
#' \donttest{cat(draw_line())}
#'
#' @export
draw_line <- function(width, mark = "=") {
  if (missing(width))
    width <- getOption("width")
  sapply(width, function(x)
    paste(rep(mark, times = ifelse(!is.na(x), min(x, getOption("width")), 0)),
          collapse = ""))
}
