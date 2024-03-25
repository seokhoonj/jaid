#' @description
#' Joo's functional aid toolkit for efficient programming.
#' @keywords internal
#' @useDynLib jaid, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table `:=` `.SD` `%chin%` address copy set setalloccol
#' setattr setDF setDT setnames setorder setorderv
#' @importFrom methods is
#' @importFrom openssl aes_cbc_decrypt aes_cbc_encrypt sha256
#' @importFrom openxlsx addStyle addWorksheet createStyle createWorkbook
#' insertPlot saveWorkbook setColWidths writeData
#' @importFrom rlang as_name enexpr enquo enquos has_length quo_is_null
#' @importFrom tibble as_tibble
#' @importFrom utils head object.size tail
"_PACKAGE"
