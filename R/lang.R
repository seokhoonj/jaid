#' Is the text japanese?
#'
#' Is the text japanese?
#'
#' @param x a string vector
#' @return a boolean vector
#'
#' @export
is_japanese <- function(x) {
  japanese <- intToUtf8(c(91, 19968, 45, 40879, 12353, 45, 12435, 12449, 45,
                          12531, 93))
  grepl(japanese, x, perl = TRUE)
}

#' Get japanese columns?
#'
#' Get japanese columns?
#'
#' @param x a data frame
#' @return japanese column names
#'
#' @export
get_japanese_cols <- function(x) {
  japanese_cols <- sapply(x, function(s) any(is_japanese(s)))
  names(japanese_cols)[japanese_cols == TRUE]
}

#' Zenkaku to Hankaku
#'
#' Covert Zenkaku to Hankaku
#'
#' @param x a string vector
#' @return a string vector
#'
#' @export
zen2han <- function(x) {
  if (Encoding(x[1]) != "UTF-8")
    x <- iconv(x, from = "", to = "UTF-8")
  x <- stringi::stri_trans_general(x, "Halfwidth-Fullwidth")
  s <- strsplit(x, split = "")
  v <- sapply(seq_along(s), function(x) {
    i <- unlist(stringi::stri_enc_toutf32(s[[x]]))
    intToUtf8(ifelse(i >= 65281 & i <= 65374, i - 65248,
                     i))
  })
  gsub(intToUtf8(12288), " ", v)
}

#' Zenkaku to Hankaku for a data frame
#'
#' Covert Zenkaku columns to Hankaku columns
#'
#' @param df a string vector
#' @return a string vector
#'
#' @export
zen2han4dat <- function(df) {
  has_ptr(df, error_raise = TRUE)
  old_class <- class(df)
  set_dt(df)
  data.table::setnames(df, zen2han(names(df)))
  cols <- names(which(sapply(df, function(x) any(is_japanese(x)))))
  df[, `:=`((cols), lapply(.SD, zen2han)), .SDcols = cols]
  data.table::setattr(df, "class", old_class)
  invisible(df[])
}
