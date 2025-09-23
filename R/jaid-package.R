#' @description
#' J's Aid ("jaid") is a lightweight, performance-oriented toolkit for data
#' analysis and programming. It emphasizes memory-efficient operations on large
#' vectors/matrices and **`data.table`** workflows.
#'
#' @details
#' Key features include:
#'
#' - **High-performance vector/matrix utilities (C-backed)**
#'   Row/column reducers: `row_sum()`, `row_max()`, `row_min()`,
#'   `col_sum()`, `col_max()`, `col_min()`.
#'   Group-by-name reducers: `sum_by_rownames()`, `max_by_rownames()`,
#'   `min_by_rownames()`, `sum_by_colnames()`, `max_by_colnames()`,
#'   `min_by_colnames()`, and two-way grouping: `sum_by_dimnames()`.
#'
#' - **`data.table`-friendly helpers**
#'   In-place, memory-aware transformations designed to avoid unnecessary copies.
#'
#' - **Metaprogramming utilities**
#'   Argument/name capture and call tracing (e.g., `capture_names()`,
#'   `trace_arg_expr()`), for building declarative APIs.
#'
#' - **Insurance inpatient (stay) utilities**
#'   Fast preprocessing of admission/discharge spans:
#'   `collapse_date_ranges()`, `count_stay()`, `limit_stay()`.
#'
#' - **I/O conveniences**
#'   Thin wrappers for pragmatic data loading: `read_rds()`, `read_xl()`, `read_wb()`.
#'
#' - **String utilities (Japanese text support)**
#'   Detection and **Zenkaku → Hankaku** conversion, plus simple regex pattern builders.
#'
#' Functions that operate on a **`data.table`** typically **modify in place**;
#' most others return new objects. See individual topics for details and examples.
#'
#' @useDynLib jaid, .registration = TRUE
#' @importFrom cli cat_bullet cat_line cat_rule cli_alert col_cyan col_green
#' cli_h2 col_red cli_text col_yellow rule style_bold style_hyperlink
#' @importFrom data.table `:=` `.SD` `%chin%` address as.data.table copy
#' data.table fifelse mday month set setalloccol setattr setDF setDT setindex
#' setnames setorder setorderv year
#' @importFrom grDevices as.raster
#' @importFrom grid grid.newpage grid.raster
#' @importFrom kableExtra footnote kable_classic kbl save_kable
#' @importFrom lifecycle badge deprecate_warn signal_stage
#' @importFrom openssl aes_cbc_decrypt aes_cbc_encrypt sha256
#' @importFrom openxlsx addStyle addWorksheet createStyle createWorkbook
#' getSheetNames insertImage insertPlot readWorkbook saveWorkbook setColWidths
#' writeData
#' @importFrom readxl excel_sheets read_excel readxl_progress
#' @importFrom reticulate conda_create conda_list install_miniconda
#' install_python py_config py_install use_condaenv use_virtualenv
#' virtualenv_create
#' @importFrom rlang as_name call_args call_name enexpr enquo enquos eval_tidy
#'   has_length is_call is_quosure is_symbol quo_is_null
#' @importFrom scales comma
#' @importFrom stats IQR median sd
#' @importFrom stringi stri_detect_regex stri_enc_toutf32 stri_trans_general
#' @importFrom stringr str_pad
#' @importFrom tibble as_tibble
#' @importFrom usethis ui_path ui_yeah
#' @importFrom utils globalVariables head install.packages object.size
#' packageVersion tail
"_PACKAGE"
