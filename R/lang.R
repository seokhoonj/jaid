#' Detect Japanese characters in strings
#'
#' Returns `TRUE` for elements of `x` that contain any Japanese characters
#' (Hiragana, Katakana, or CJK ideographs). Matching is performed with a
#' Unicode-range regular expression using `perl = TRUE`.
#'
#' @param x A character vector.
#'
#' @return A logical vector the same length as `x`.
#'
#' @examples
#' \dontrun{
#' is_japanese(c("abc", "こんにちは", "カタカナ", "漢字"))
#' }
#'
#' @export
is_japanese <- function(x) {
  # pattern <- intToUtf8(c(91, 19968, 45, 40879, 12353, 45, 12435, 12449, 45,
  #                        12531, 93))
  # Match Hiragana, Katakana (including prolonged sound mark), Han (CJK),
  # iteration marks, and halfwidth Katakana.
  pattern <- "(?:\\p{Hiragana}|\\p{Katakana}|\\p{Han}|[\\x{30FC}\\x{3005}\\x{3007}\\x{FF66}-\\x{FF9F}])"
  grepl(pattern, x, perl = TRUE)
}

#' Get columns containing Japanese text
#'
#' Scans each column of a data frame-like object and returns the names of
#' columns where at least one value contains Japanese characters
#' (as determined by [is_japanese()]).
#'
#' @param x A data.frame or data.table.
#'
#' @return A character vector of column names containing Japanese text.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = c("A", "い"), b = c("B", "C"))
#' get_japanese_cols(df)
#' }
#'
#' @name get_japanese_cols
#' @export
get_japanese_cols <- function(x) {
  japanese_cols <- sapply(x, function(s) any(is_japanese(s)))
  names(japanese_cols)[japanese_cols == TRUE]
}

#' Zenkaku to Hankaku
#'
#' Converts full-width forms to half-width for characters where such a mapping
#' exists. Input is converted to UTF-8 if necessary.
#'
#' @param x A character vector.
#'
#' @return A character vector with half-width forms where applicable.
#'
#' @examples
#' \dontrun{
#' zen_to_han(c("ＡＢＣ", "１２３", "ｱｲｳ", "テスト　"))
#' }
#'
#' @export
zen_to_han <- function(x) {
  if (Encoding(x[1]) != "UTF-8")
    x <- iconv(x, from = "", to = "UTF-8")
  x <- stringi::stri_trans_general(x, "Halfwidth-Fullwidth")
  s <- strsplit(x, split = "")
  v <- sapply(seq_along(s), function(x) {
    i <- unlist(stringi::stri_enc_toutf32(s[[x]]))
    intToUtf8(ifelse(i >= 65281 & i <= 65374, i - 65248, i))
  })
  gsub(intToUtf8(12288), " ", v)
}

#' Convert full-width to half-width for data.table columns
#'
#' Renames columns using [zen_to_han()] and also converts the contents of any
#' columns that contain Japanese text (as determined by [is_japanese()]).
#'
#' @param df A data.table. Modified in place.
#'
#' @return The updated data.table.
#'
#' @examples
#' \dontrun{
#' df <- data.table::data.table("Ａ" = c("Ａ", "Ｂ"))
#' replace_zen_to_han(df)
#' }
#'
#' @export
replace_zen_to_han <- function(df) {
  assert_class(df, "data.table")
  data.table::setnames(df, zen_to_han(names(df)))
  cols <- names(which(sapply(df, function(x) any(is_japanese(x)))))
  if (length(cols)) df[, `:=`((cols), lapply(.SD, zen_to_han)), .SDcols = cols]
  df
}
