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
    loc <- sapply(df, function(x) inherits(x, "data.frame"))
    if (any(loc)) {
      df[loc] <- lapply(df[loc], function(x) data.table::setalloccol(x))
      return(df)
    }
  }
  return(df)
}

#' load_excel
#'
#' load_excel is almost same as [readxl::read_excel()] but it's convenient when not knowing the
#' sheet names. and the output type is a data.table not a tibble.
#'
#' @inheritParams readxl::read_excel
#' @return a data.table
#'
#' @export
load_excel <- function(path, sheet = NULL, range = NULL, col_names = TRUE,
                       col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                       guess_max = getOption("jaid.guess_max"), progress = readxl_progress(),
                       .name_repair = "unique") {
  if (is.null(sheet)) {
    op <- options(max.print = .Machine$integer.max)
    sheets <- readxl::excel_sheets(path = path)
    hprint(data.frame(no = seq_along(sheets), sheet = sheets))
    on.exit(op)
    sheet <- readline("Please insert the sheet name: ")
  }
  z <- readxl::read_excel(
    path = path, sheet = sheet, range = range, col_names = col_names,
    col_types = col_types, na = na, trim_ws = trim_ws, skip = skip,
    n_max = n_max, guess_max = guess_max, .name_repair = .name_repair
  )
  data.table::setDT(z)
  return(z)
}
