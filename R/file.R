#' Create library.R
#'
#' Create library.R file
#'
#' @param path a string value specifying the path of the file.
#' @return no return value
#' @examples
#' # create library.R
#' \donttest{create_library_r("library.R")}
#'
#' @export
create_library_r <- function(path = "library.R") {
  ans <- TRUE
  if (file.exists(path)) {
    ans <- usethis::ui_yeah("Overwrite pre-existing file {usethis::ui_path(path)}?")
  }
  if (ans) {
    file.create(path)
    libs <- c(
      "auw", "bslib", "data.table", "devtools", "dplyr", "DT", "ecos",
      "forecast", "ggplot2", "ggshort", "grid", "gridExtra", "igraph", "jaid",
      "kisopenapi", "kosis", "lossratio", "lubridate", "navergmail", "offline",
      "opendart", "openxlsx", "papagor", "plotly", "RColorBrewer", "readxl",
      "rintrojs", "rlang", "ROSE", "scales", "shiny", "shinyBS", "shinybusy",
      "shinycssloaders", "shinydashboard", "shinyFiles", "shinyjs",
      "shinymodules", "shinyWidgets", "slickR", "smotefamily", "stringr",
      "statmod", "tibble", "usethis", "visNetwork", "writexl", "zoo"
    )
    cat(
      "\nsuppressPackageStartupMessages({\n",
      paste0("\tlibrary(", libs, ")\n"),
      "})",
      file = path
    )
  }
}
