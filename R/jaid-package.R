#' @description
#' Joo's functional aid toolkit for efficient programming.
#' @keywords internal
#' @useDynLib jaid, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table `:=` `.SD` `%chin%` address as.data.table copy
#' data.table set setalloccol setattr setDF setDT setnames setorder setorderv
#' @importFrom grDevices dev.list dev.off
#' @importFrom grid grid.raster
#' @importFrom kableExtra footnote kable_classic kbl save_kable
#' @importFrom methods is
#' @importFrom openssl aes_cbc_decrypt aes_cbc_encrypt sha256
#' @importFrom openxlsx addStyle addWorksheet createStyle createWorkbook
#' insertImage insertPlot saveWorkbook setColWidths writeData
#' @importFrom png readPNG
#' @importFrom readxl excel_sheets read_excel readxl_progress
#' @importFrom rlang as_name enexpr enquo enquos has_length quo_is_null
#' @importFrom stats IQR median sd
#' @importFrom stringi stri_enc_toutf32 stri_trans_general
#' @importFrom stringr str_pad
#' @importFrom tibble as_tibble
#' @importFrom usethis ui_path ui_yeah
#' @importFrom utils globalVariables head object.size tail
"_PACKAGE"
