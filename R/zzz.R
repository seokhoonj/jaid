
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.jaid <- list(
    jaid.eps = 1e-8,
    jaid.scipen = 14,
    jaid.guess_max = 21474836
  )
  toset <- !(names(op.jaid) %in% names(op))
  if (any(toset)) options(op.jaid[toset])
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("jaid", libpath)
}
