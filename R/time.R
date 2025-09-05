#' Measure execution time of an expression
#'
#' Evaluate an expression and return the elapsed time in `HH:MM:SS.subsec`
#' format. Intended for quick ad-hoc timing in scripts.
#'
#' @param expr An R expression to evaluate. Use `{ ... }` to time multiple lines.
#'
#' @examples
#' # Time a simple loop
#' \donttest{timeit({ for (i in 1:1e6) i })}
#'
#' @export
timeit <- function(expr) {
  stime <- as.numeric(Sys.time())
  eval(expr)
  etime <- as.numeric(Sys.time())
  sec_to_hms(etime - stime)
}

#' Convert seconds to HH:MM:SS.subsec string
#'
#' Utility to format a numeric number of seconds as a clock-style string with
#' subseconds preserved.
#'
#' @param sec Numeric seconds.
#' @return A character string in the form `HH:MM:SS.subsec`.
#'
#' @keywords internal
sec_to_hms <- function(sec) {
  op <- options(scipen = 14); on.exit(options(op))
  hh  <- sec %/% 3600
  sec <- sec %%  3600
  mm  <- sec %/% 60
  ss  <- sec %%  60
  int <- floor(ss)
  frac_str <- sub("^0\\.", "", sprintf("%.3f", ss - int))
  sprintf("%02d:%02d:%02d.%s", hh, mm, int, frac_str)
}
