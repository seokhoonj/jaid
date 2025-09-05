#' Read an RDS file with data.table-friendly behavior
#'
#' Behaves like [base::readRDS()] but, when the result is a data.frame (or a
#' list containing data.frames), ensures a data.table-friendly internal layout
#' by calling [data.table::setalloccol()]. This helps preserve efficient pointer
#' semantics when working with large tables.
#'
#' @param file A [connection] or file path to the serialized R object (.rds).
#' @param refhook A hook function for handling reference objects; passed to
#'   [base::readRDS()].
#'
#' @return An R object. If a data.frame is read, a data.table with allocated
#'   columns is returned. For lists, any data.frame elements are converted
#'   similarly.
#'
#' @examples
#' \dontrun{
#' # Open a file chooser dialog
#' data <- read_rds()
#'
#' # Compare pointer values
#' data <- data.table::as.data.table(iris)
#' saveRDS(data, "data.rds")
#' df <- readRDS("data.rds")
#' dt <- read_rds("data.rds")
#' attr(df, ".internal.selfref", exact = TRUE)  # <pointer: (nil)>
#' attr(df, ".internal.selfref", exact = TRUE)
#' }
#'
#' @export
read_rds <- function(file, refhook = NULL) {
  if (missing(file)) {
    file <- gsub(sprintf("%s/", getwd()), "", file.choose())
    cat(sprintf("Path: %s\n", file))
    cat(sprintf("Code: jaid::read_rds(\"%s\")\n", file))
  }
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

#' Read an Excel sheet as a data.table
#'
#' Convenience wrapper around [readxl::read_excel()]. If `sheet` is not
#' provided, interactively lists available sheets and prompts for a selection.
#' The result is returned as a data.table (not a tibble).
#'
#' @param path Path to the Excel file.
#' @param sheet A sheet name or 1-based index. If `NULL`, an interactive
#'   selector is shown.
#' @param range Cell range to read, e.g., "A1:D10". `NULL` reads the full sheet.
#' @param col_names Whether the first row contains column names.
#' @param col_types Column types; see [readxl::read_excel()].
#' @param na Character vector of strings to interpret as missing values.
#' @param trim_ws Trim leading and trailing whitespace from character fields.
#' @param skip Number of rows to skip before reading data.
#' @param n_max Maximum number of data rows to read.
#' @param guess_max Maximum rows used for type guessing.
#' @param progress Whether to show a progress bar.
#' @param .name_repair Strategy for repairing duplicate/invalid names; see
#'   [tibble::as_tibble()].
#'
#' @return A data.table
#'
#' @export
read_xl <- function(path, sheet = NULL, range = NULL, col_names = TRUE,
                    col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
                    n_max = Inf, guess_max = getOption("jaid.guess_max"),
                    progress = readxl_progress(), .name_repair = "unique") {
  if (missing(path)) {
    path <- gsub(sprintf("%s/", getwd()), "", file.choose())
    cat(sprintf("Path: %s\n", path))
  }
  if (is.null(sheet)) {
    op <- options(max.print = .Machine$integer.max)
    sheets <- readxl::excel_sheets(path = path)
    dsheets <- data.frame(no = seq_along(sheets), sheet = sheets)
    hprint(dsheets, hchar = 5)
    on.exit(op)
    no <- readline("Please insert the sheet number (or Press `Enter` for the 1st sheet): ")
    if (no == "")
      no <- 1
    if (!grepl("^[1-9]\\d*$", no))
      stop(" Invalid type for a `sheet number`")
    no <- as.numeric(no)
    sheet <- dsheets[dsheets$no == no,]$sheet
    cat(sprintf("Code: jaid::read_xl(\"%s\", sheet = \"%s\")\n", path, sheet))
  }
  z <- readxl::read_excel(
    path = path, sheet = sheet, range = range, col_names = col_names,
    col_types = col_types, na = na, trim_ws = trim_ws, skip = skip,
    n_max = n_max, guess_max = guess_max, .name_repair = .name_repair
  )
  data.table::setDT(z)
  return(z)
}

#' Read an Excel sheet with openxlsx as a data.table
#'
#' Convenience wrapper around [openxlsx::readWorkbook()]. If `sheet` is not
#' provided, interactively lists available sheets and prompts for a selection.
#' The result is returned as a data.table.
#'
#' @param xlsx_file Path to the .xlsx file.
#' @param sheet A sheet name or 1-based index. If `NULL`, an interactive
#'   selector is shown.
#' @param start_row First row to read (1-based).
#' @param col_names Whether the first row contains column names.
#' @param row_names Whether the first column contains row names.
#' @param detect_dates Convert recognised date columns to Date.
#' @param skip_empty_rows Skip empty rows while reading.
#' @param skip_empty_cols Skip empty columns while reading.
#' @param rows Optional integer vector of rows to read.
#' @param cols Optional integer vector of columns to read.
#' @param check_names Make column names syntactically valid.
#' @param sep_names Separator to use when repairing duplicate names.
#' @param named_region Named region to read instead of specifying `rows/cols`.
#' @param na_strings Character vector of strings to interpret as missing values.
#' @param fill_merged_cells Fill merged cells with their corresponding values.
#'
#' @return A data.table.
#'
#' @export
read_wb <- function(xlsx_file, sheet = NULL, start_row = 1, col_names = TRUE,
                    row_names = FALSE, detect_dates = FALSE, skip_empty_rows = TRUE,
                    skip_empty_cols = TRUE, rows = NULL, cols = NULL,
                    check_names = FALSE, sep_names = ".", named_region = NULL,
                    na_strings = "NA", fill_merged_cells = FALSE) {
  if (missing(xlsx_file)) {
    xlsx_file <- gsub(sprintf("%s/", getwd()), "", file.choose())
    cat(sprintf("Path: %s\n", xlsx_file))
  }
  if (is.null(sheet)) {
    op <- options(max.print = .Machine$integer.max)
    sheets <- openxlsx::getSheetNames(file = xlsx_file)
    dsheets <- data.frame(no = seq_along(sheets), sheet = sheets)
    hprint(dsheets, hchar = 5)
    on.exit(op)
    no <- readline("Please insert the sheet number (or Press `Enter` for the 1st sheet): ")
    if (no == "")
      no <- 1
    if (!grepl("^[1-9]\\d*$", no))
      stop(" Invalid type for a `sheet number`")
    no <- as.numeric(no)
    sheet <- dsheets[dsheets$no == no,]$sheet
    cat(sprintf("Code: jaid::read_wb(\"%s\", sheet = \"%s\")\n", xlsx_file, sheet))
  }
  z <- openxlsx::readWorkbook(
    xlsxFile = xlsx_file, sheet = sheet, startRow = start_row,
    colNames = col_names, rowNames = row_names, detectDates = detect_dates,
    skipEmptyRows = skip_empty_rows, skipEmptyCols = skip_empty_cols,
    rows = rows, cols = cols, check.names = check_names, sep.names = sep_names,
    namedRegion = named_region, na.strings = na_strings,
    fillMergedCells = fill_merged_cells
  )
  data.table::setDT(z)
  return(z)
}
