
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Written by Seokhoon Joo. (Note, kcd terms arguments are monthly-basis)")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.vuw <- list(
    vuw.eps = 1e-8,
    vuw.scipen = 14,
    vuw.guess_max = 21474836
  )
  toset <- !(names(op.vuw) %in% names(op))
  if (any(toset)) options(op.vuw[toset])
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("jaid", libpath)
}
