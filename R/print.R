#' Draw a horizontal line in the console
#'
#' Create a line made of repeated characters, typically used as a visual
#' separator in console output. The line width defaults to the console
#' option `getOption("width")`.
#'
#' @param width A numeric vector specifying the desired line width(s).
#'   Defaults to the current console width (`getOption("width")`).
#' @param mark A single string giving the character(s) to repeat.
#'   Defaults to `"="`.
#'
#' @return A character vector, each element being a line of repeated marks.
#'
#' @examples
#' \donttest{
#' # Default line
#' cat(draw_line())
#'
#' # Custom width
#' cat(draw_line(20))
#'
#' # Custom mark
#' cat(draw_line(30, mark = "="))
#' }
#'
#' @seealso [cli::cat_rule()]
#'
#' @export
draw_line <- function(width, mark = "=") {
  if (missing(width))
    width <- getOption("width")
  sapply(width, function(x)
    paste(rep(mark, times = ifelse(!is.na(x), min(x, getOption("width")), 0)),
          collapse = "")
  )
}

#' Print a data frame in a fixed-width table format
#'
#' Formats and prints a data frame with aligned column widths, similar to a
#' simple console table. Useful for quickly inspecting data frames with
#' controlled formatting.
#'
#' @param df A `data.frame` to print.
#' @param width Integer. Target width for each column (passed to
#'   `.adjust_names_width()`).
#' @param align Character string specifying text alignment within columns:
#'   `"right"`, `"left"`, or `"both"`. Default is `"right"`.
#' @param n Integer. Number of rows to print (default: 300).
#'
#' @details
#' - The function adjusts column names and values to the specified width,
#'   aligns text as requested, and prints the first `n` rows.
#' - Column names are printed as a header row between horizontal rules
#'   (via `cli::cat_rule()`).
#' - Output is printed directly to the console using `cat()`.
#'
#' @return No return value, called for side effects (prints to console).
#'
#' @examples
#' \donttest{
#' dprint(mtcars, width = 10, align = "left", n = 5)
#' }
#'
#' @export
dprint <- function(df, width, align = c("right", "both", "left"), n = 300L) {
  assert_class(df, "data.frame")
  align <- match.arg(align)
  df <- .adjust_names_width(head(df, n), width = width, align = align)
  txt <- paste_list(df)
  cols <- names(df)
  cli::cat_rule(line = 2)
  cat(paste0("|", paste0(cols, collapse = "|"), "\n"))
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
}

hprint <- function(x, width, align = c("right", "both", "left")) {
  align <- match.arg(align)
  df <- .adjust_names_width(x, width = width, align = align)
  txt <- paste_list(df)
  cols <- colnames(df)
  cli::cat_rule(line = 2)
  cat(paste0("|", paste0(cols, collapse = "|"), "\n"))
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
}

vprint <- function(x, width = 4, nchar_limit = 16,
                   align = c("right", "both", "left")) {
  align <- match.arg(align)
  df <- .adjust_names_width(x, width = width, align = align)
  txt <- paste_list(df)
  cols <- toupper(attr(df, "columns"))
  names_width <- max(nchar(cols))
  dots <- stringr::str_pad(cols, width = names_width, pad = " ", side = "right")
  vcols <- lapply(seq(1, min(nchar_limit + 1, names_width), width), function(x)
    paste0(stringr::str_pad(substr(dots, x, x + width - 1), width = attr(df, "width"),
                            pad = " ", side = align), collapse = "|"))
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", vcols), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
}

aprint <- function(x, width = 4, nchar_limit = 16,
                   align = c("right", "both", "left")) {
  align <- match.arg(align)
  df <- .adjust_names_width(x, width = width, align = align)
  txt <- paste_list(df)
  cols <- toupper(attr(df, "columns"))
  names_width <- max(nchar(cols))
  dots <- stringr::str_pad(cols, width = names_width, pad = " ", side = "right")
  vcols <- lapply(seq(1, min(nchar_limit + 1, names_width), width), function(x)
    paste0(stringr::str_pad(substr(dots, x, x + width - 1), width = attr(df, "width"),
                            pad = " ", side = align), collapse = "|"))
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", vcols), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
  cat(paste0("|", paste0(names(df), collapse = "|"), "\n"))
  cli::cat_rule(line = 2)
  cat(paste0(paste0("|", txt), collapse = "\n"), "\n")
  cli::cat_rule(line = 2)
}



# Internal helper functions -----------------------------------------------

.adjust_names_width <- function(df, width, align = c("right", "both", "left")) {
  align <- match.arg(align)
  cols <- names(df)
  nchar_cols <- nchar(cols)
  notc_cols_no <- which(sapply(df, class) != "character")
  if (length(notc_cols_no) > 0)
    df[, notc_cols_no] <- lapply(df[, notc_cols_no, drop = FALSE],
                                 as.character)
  names_width <- sapply(df, function(x) {
    if (all(is.na(x))) 2L else max(nchar(x), na.rm = T)
  })
  if (missing(width)) {
    width <- pmax(names_width)
  } else {
    width <- pmax(names_width, min(width, max(nchar_cols)))
  }
  df[] <- lapply(df, function(x) if (is.character(x)) ifelse(is.na(x), "", x) else x)
  side <- sapply(df, function(x) if (is.character(x)) "right" else "left")
  df[] <- lapply(seq_along(df), function(x)
    stringr::str_pad(df[[x]], width = width[x], side = side[x])
  )
  abb_cols <- substr(names(width), 1L, width)
  new_cols <- stringr::str_pad(abb_cols, width = width, pad = " ", side = align)
  names(df) <- new_cols
  attr(df, "columns") <- cols
  attr(df, "width") <- width
  attr(df, "side") <- side

  df
}

.reduce_rows <- function(x, n = 242L) {
  tn <- nrow(x)
  if (tn > 242L)
    return(rbind(head(x, n/2), tail(x, n/2)))
  return(x)
}
