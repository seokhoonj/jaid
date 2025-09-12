#' Create directories
#'
#' Create directories on disk, similar to the Linux `mkdir` command.
#' Directories are created recursively if needed.
#'
#' @param folder A character vector of directory paths to create.
#'
#' @return No return value, called for side effects. Creates the specified
#'   directories. If a directory already exists, a message is shown and
#'   no action is taken.
#'
#' @examples
#' \dontrun{
#' # Create two temporary directories
#' mkdir(c("tmp1", "tmp2"))
#'
#' # Re-running will show "Already exists"
#' mkdir(c("tmp1", "tmp2"))
#' }
#'
#' @export
mkdir <- function(folder = c("dev", "data", "info", "inst", "output", "R",
                             "raw", "report", "rules")) {
  exst <- file.exists(folder)
  ndir <- folder[!exst]
  edir <- folder[exst]
  if (length(ndir) > 0) {
    lapply(ndir, dir.create, recursive = TRUE, showWarnings = FALSE)
    message(sprintf(
      "Created folder(s): ",
      paste0("'", paste(ndir, collapse = "', '"), "'")
    ))
  }
  if (length(edir) > 0) {
    message(sprintf(
      "Already exists: ",
      paste0("'", paste(edir, collapse = "', '"), "'")
    ))
  }
}

#' Search files for lines containing a given pattern
#'
#' @description
#' Search within a folder, a single file, or a glob pattern (e.g. `./R/*.R`)
#' for lines that contain a given pattern. Results are returned as a named list,
#' where each element is a data frame with `line` and `text` columns.
#' In the console, matches are always highlighted in **red + bold** using the
#' [`cli`] package.
#'
#' @details
#' - If `path` is a file, only that file is searched.
#' - If `path` is a folder, all files are searched (recursively if `recursive = TRUE`).
#' - If `path` contains a glob pattern (e.g. `"*.R"`), it is expanded via [base::Sys.glob()].
#' - Matches are shown in red and bold in the console.
#' - If `show_lines = FALSE`, only filenames are shown (the returned data frames
#'   will be empty).
#'
#' @param path Character scalar. Path to a folder, file, or glob pattern.
#' @param word Character scalar. Pattern to search for. If `fixed = TRUE`, the
#'   pattern is matched literally, not as a regular expression.
#' @param recursive Logical. Whether to search subdirectories when `path` is a folder.
#'   Default is `TRUE`.
#' @param ignore_case Logical. Whether to ignore case during matching. Default is `TRUE`.
#' @param show_lines Logical. Whether to return and print matching line numbers
#'   and text. Default is `TRUE`.
#' @param fixed Logical. Whether to treat `word` as a fixed string rather than
#'   a regular expression. Default is `FALSE`.
#' @param skip_extensions Character vector. File extensions to skip during search
#'   (e.g., binary files, images, compressed archives, PDFs). Default includes
#'   common non-text formats such as `bin`, `dll`, `o`, `so`, `rda`, `rds`,
#'   images (`jpg`, `png`, etc.), compressed files (`zip`, `tar`, `gz`, etc.),
#'   and `pdf`.
#'
#' @return
#' A named list. Each element corresponds to one file, and is a data frame
#' with columns:
#' \describe{
#'   \item{line}{Integer line numbers of matches}
#'   \item{text}{Character strings of the matching lines}
#' }
#'
#' @examples
#' \dontrun{
#' # Search all R files for "as.numeric"
#' find_in_files("./R", "as.numeric")
#' }
#'
#' @export
find_in_files <- function(path = getwd(),
                          word,
                          recursive = TRUE,
                          ignore_case = TRUE,
                          show_lines = TRUE,
                          fixed = FALSE,
                          skip_extensions) {

  if (missing(skip_extensions)) {
    skip_extensions <- list(
      image_raster = c("bmp","gif","jpeg","jpg","png","tif","tiff","webp",
                       "apng","avif","heif","heic","ico"),
      image_vector = c("svg","ai","psd","xcf","icns","pdf","raw"),
      archive      = c("7z","gz","tar","xz","zip","bz2"),
      binary_build = c("bin","dll","o","so"),
      database     = c("db","sqlite","sqlite3","mdb","accdb"),
      office       = c("xls","xlsb","xlsm","xlsx","ods","numbers",
                       "doc","docx","ppt","pptx","pdf"),
      data_format  = c("RData","rda","rds","pkl","msgpack","feather",
                       "parquet","avro","orc","h5","hdf5",
                       "sas7bdat","sav","por","dta","mat")
      )
    skip_extensions <- unlist(skip_extensions, use.names = FALSE)
  }

  highlight_line <- function(text, pattern) {
    m <- gregexpr(pattern, text,
                  ignore.case = ignore_case,
                  perl = !fixed,
                  fixed = fixed)[[1L]]
    if (m[1L] == -1L)
      return(text)
    len <- attr(m, "match.length")
    parts <- character(0); last_end <- 0L
    for (i in seq_along(m)) {
      s <- m[i]; e <- s + len[i] - 1L
      if (s > last_end + 1L)
        parts <- c(parts, substr(text, last_end + 1L, s - 1L))
      parts <- c(parts, cli::col_red(cli::style_bold(substr(text, s, e))))
      last_end <- e
    }
    if (last_end < nchar(text))
      parts <- c(parts, substr(text, last_end + 1L, nchar(text)))
    paste0(parts, collapse = "")
  }

  # expand path: glob, file, or folder
  if (length(path) == 1L && grepl("[*?]", path)) {
    files <- Sys.glob(path)
  } else if (file.exists(path) && !dir.exists(path)) {
    files <- normalizePath(path, mustWork = FALSE)
  } else {
    files <- list.files(path, recursive = recursive, full.names = TRUE)
    files <- files[file.info(files)$isdir == FALSE]
  }
  extensions <- paste0("\\.(", paste0(skip_extensions, collapse = "|"), ")$")
  files <- files[!grepl(extensions, files, ignore.case = TRUE)]

  results <- list()
  for (f in files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next
    idx <- grep(word, lines, ignore.case = ignore_case, perl = !fixed,
                fixed = fixed)
    if (length(idx) == 0) next

    if (show_lines) {
      results[[f]] <- data.frame(line = idx, text = lines[idx],
                                 stringsAsFactors = FALSE)
    } else {
      results[[f]] <- data.frame(line = integer(0), text = character(0))
    }
  }

  # console output
  cat(cli::col_cyan(cli::rule("Search results", line = 2)), "\n")
  if (!length(results)) {
    cli::cat_line(cli::col_yellow("No matches found."))
  } else {
    for (fp in names(results)) {
      cli::cat_bullet(cli::col_blue(fp), bullet = "file")
      if (show_lines && nrow(results[[fp]]) > 0) {
        for (k in seq_len(nrow(results[[fp]]))) {
          ln <- results[[fp]]$line[k]
          tx <- results[[fp]]$text[k]
          cli::cat_line(
            paste0("  ", sprintf("%6d", ln), " | ", highlight_line(tx, word))
          )
        }
      }
    }
  }

  invisible(results)
}
