#' @description
#' Joo's functional aid toolkit for efficient programming.
#' @keywords internal
#' @useDynLib jaid, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom cli cli_alert col_cyan col_green cli_h2 col_red cli_text col_yellow
#' @importFrom data.table `:=` `.SD` `%chin%` address as.data.table copy
#' data.table fifelse mday month set setalloccol setattr setDF setDT setindex
#' setnames setorder setorderv year
#' @importFrom grDevices dev.list dev.off
#' @importFrom grid grid.raster
#' @importFrom kableExtra footnote kable_classic kbl save_kable
#' @importFrom methods is
#' @importFrom openssl aes_cbc_decrypt aes_cbc_encrypt sha256
#' @importFrom openxlsx addStyle addWorksheet createStyle createWorkbook
#' insertImage insertPlot saveWorkbook setColWidths writeData
#' @importFrom png readPNG
#' @importFrom readxl excel_sheets read_excel readxl_progress
#' @importFrom reticulate conda_create conda_list install_miniconda install_python
#' py_config py_install use_condaenv use_virtualenv virtualenv_create
#' @importFrom rlang as_name enexpr enquo enquos has_length quo_is_null
#' @importFrom scales comma
#' @importFrom stats IQR median sd
#' @importFrom stringi stri_enc_toutf32 stri_trans_general
#' @importFrom stringr str_pad
#' @importFrom tibble as_tibble
#' @importFrom usethis ui_path ui_yeah
#' @importFrom utils globalVariables head install.packages object.size
#' packageVersion tail
"_PACKAGE"
