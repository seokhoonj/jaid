#' @title mkdir
#'
#' @description
#' Same function of linux mkdir command
#'
#' @param folders A string vector specifying folder names
#' @return no return, this function makes folders. if the folders already exist,
#'         no effect
#'
#' @export
mkdir <- function(folders = c("dev", "data", "info", "inst", "output", "R",
                              "raw", "report", "rules")) {
  exst <- file.exists(folders)
  edir <- folders[exst]
  ndir <- folders[!exst]
  if (length(ndir) > 0) {
    sapply(ndir, dir.create)
    message(sprintf(
      "%s folder(s) created",
      paste0("'", paste(ndir, collapse = "', '"), "'")
    ))
  }
  if (length(edir) > 0) {
    message(sprintf(
      "%s folder(s) already exists",
      paste0("'", paste(edir, collapse = "', '"), "'")
    ))
  }
}
