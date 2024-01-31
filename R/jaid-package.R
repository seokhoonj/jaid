#' @description
#' Joo's functional aid toolkit for efficient programming.
#' @keywords internal
#' @useDynLib jaid, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom openssl aes_cbc_decrypt aes_cbc_encrypt sha256
#' @importFrom openxlsx addStyle addWorksheet createStyle createWorkbook
#'  insertPlot saveWorkbook setColWidths writeData
#' @importFrom utils head object.size tail
"_PACKAGE"
