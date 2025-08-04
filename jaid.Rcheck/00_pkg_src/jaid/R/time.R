#' Time it
#'
#' Time \R code expression
#'
#' @param expr \R code expression
#'
#' @examples
#' # time it
#' \donttest{timeit(for (i in 1:1000000) i)}
#'
#' @export
timeit <- function(expr) {
  stime <- as.numeric(Sys.time())
  eval(expr)
  etime <- as.numeric(Sys.time())
  sec_to_hms(etime - stime)
}

sec_to_hms <- function(sec) {
  op <- options(scipen = 14)
  hh  <- sec %/% (60^2)
  sec <- sec %%  (60^2)
  mm  <- sec %/% (60)
  ss  <- sec %%  (60)
  ss <- strsplit(as.character(ss), split = "\\.")[[1L]]
  s1 <- as.numeric(ss[1L])
  s2 <- ss[2L]
  on.exit(op)
  return(sprintf("%02d:%02d:%02d.%s", hh, mm, s1, s2))
}
