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
  # pattern <- paste0(
  #   "(?:",
  #   "\\p{Hiragana}|\\p{Katakana}|\\p{Han}|",
  #   "[\\x{30FC}\\x{3005}\\x{3007}]|",
  #   "[\\x{FF66}-\\x{FF9F}]|",
  #   "[\\x{FF10}-\\x{FF19}]|",
  #   "[\\x{FF21}-\\x{FF3A}]|",
  #   "[\\x{FF41}-\\x{FF5A}]|",
  #   "[\\x{FF01}-\\x{FF60}]|[\\x{FFE0}-\\x{FFE6}]",  # 전각 기호(대표 범위)
  #   ")"
  # )
  # grepl(pattern, x, perl = TRUE)
  stringi::stri_detect_regex(
    x,
    "\\p{sc=Hiragana}|\\p{sc=Katakana}|\\p{sc=Han}|\\p{blk=Halfwidth_And_Fullwidth_Forms}"
  )
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

#' Convert full-width to half-width in data.frame/data.table
#'
#' This function renames columns using [zen_to_han()] and also converts the
#' contents of any columns that contain Japanese text (as determined by
#' [is_japanese()]).
#'
#' - If the input is a `data.table`, it will be modified **in place**.
#' - If the input is a `data.frame` or tibble, a copy is made internally and
#'   the original is left unchanged. The result is returned as the same class
#'   as the input.
#'
#' @param df A data.frame, tibble, or data.table.
#'
#' @return The updated object, with the same class as `df` (modified in place
#'   if `df` is a data.table).
#'
#' @examples
#' \dontrun{
#' # data.frame input → returns a modified copy
#' df <- data.frame("Ａ" = c("Ａ", "Ｂ"))
#' replace_zen_to_han(df)
#'
#' # data.table input → modified in place
#' dt <- data.table::data.table("Ａ" = c("Ａ", "Ｂ"))
#' replace_zen_to_han(dt)
#' dt  # column names and values converted
#' }
#'
#' @export
replace_zen_to_han <- function(df) {
  assert_class(df, "data.frame")
  env <- ensure_dt_env(df)
  dt <- env$dt
  data.table::setnames(dt, zen_to_han(names(dt)))
  cols <- names(which(sapply(dt, function(x) any(is_japanese(x)))))
  lapply(dt, is_japanese)
  if (length(cols))
    dt[, `:=`((cols), lapply(.SD, zen_to_han)), .SDcols = cols]
  env$restore(dt)
}
