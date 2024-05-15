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
  df <- readRDS(file, refhook = refhook)
  if (inherits(df, "data.frame"))
    return(data.table::setalloccol(df))
  if (inherits(df, "list")) {
    loc <- sapply(x, function(x) inherits(x, "data.frame"))
    if (any(loc)) {
      df[loc] <- lapply(df[loc], function(x) data.table::setalloccol(x))
      return(df)
    }
  }
  return(df)
}
