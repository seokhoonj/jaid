#' Generate a library.R file with package loading calls
#'
#' This function creates (or overwrites) a `library.R` file at the given
#' path. The file will contain calls to `library()` for a predefined set
#' of commonly used packages, wrapped in
#' `suppressPackageStartupMessages()` to reduce console noise.
#'
#' @param path A character string giving the path where the `library.R`
#'   file should be written. Defaults to `"library.R"` in the current
#'   working directory.
#' @return No return value. Called for side effects (file creation).
#'
#' @examples
#' \dontrun{
#' # Create or overwrite a library.R file in the current directory
#' create_library.R()
#'
#' # Create a library.R file at a custom path
#' create_library.R("R/library.R")
#' }
#'
#' @export
create_library.R <- function(path = "library.R") {
  ans <- TRUE
  if (file.exists(path)) {
    ans <- usethis::ui_yeah(
      x = "Overwrite pre-existing file {usethis::ui_path(path)}?"
    )
  }
  if (ans) {
    libs <- c(
      "aus", "auw", "bslib", "cli", "data.table", "devtools", "dplyr", "DT",
      "ecos", "forcat", "forecast", "ggplot2", "ggshort", "grid", "gridExtra",
      "igraph", "jaid", "kisopenapi", "kosis", "lossratio", "lubridate",
      "navergmail", "offline", "opendart", "openxlsx", "papagor", "plotly",
      "purrr", "randomForest", "RColorBrewer", "readr", "readxl", "rintrojs",
      "rlang", "ROSE", "scales", "shiny", "shinyBS", "shinybusy",
      "shinycssloaders", "shinydashboard", "shinyFiles", "shinyjs",
      "shinymodules", "shinyWidgets", "slickR", "smotefamily", "stringr",
      "statmod", "survival", "testthat", "tibble", "tidyr", "usethis",
      "visNetwork", "writexl", "xgboost", "zoo"
    )
    body <- c(sprintf("  library(%s)", libs), "  options(scipen = 14L)")
    code <- c("suppressPackageStartupMessages({", body, "})")
    code
    writeLines(code, path)
  }
}
