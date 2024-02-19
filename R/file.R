#' loadRDS
#'
#' loadRDS is almost same as readRDS except for a pointer.
#'
#' @param file a \link{connection} or the name of the file where the R object is saved
#'  to or read from.
#' @param refhook a hook function for handling reference objects.
#'
#' @return an \R object
#'
#' @examples
#' # compare pointer values
#' \dontrun{data <- copy(women)
#' data.table::setDT(data)
#' saveRDS(data, "data.rds")
#' df <- readRDS("data.rds")
#' dt <- loadRDS("data.rds")
#' attributes(df)$.internal.selfref # <pointer: (nil)>
#' attributes(dt)$.internal.selfref}
#'
#' @export
loadRDS <- function(file, refhook = NULL) {
  df <- readRDS(file, refhook)
  if (inherits(df, "data.table"))
    return(alloc.col(df))
  return(df)
}
