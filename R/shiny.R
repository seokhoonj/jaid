#' Check if a Shiny session is running
#'
#' This utility inspects whether Shiny is currently running.
#'
#' - If the `shiny` namespace is already loaded, it looks up and safely
#'   calls `shiny::isRunning()`.
#' - If `shiny` is not loaded, it simply returns `FALSE`.
#' - Any errors from `shiny::isRunning()` are caught and treated as `FALSE`.
#'
#' @return A logical scalar: `TRUE` if `shiny` is running, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Returns FALSE in non-Shiny environments
#' # In a Shiny app, will return TRUE once the session is active
#' .is_shiny_running()
#' }
#'
#' @keywords internal
.is_shiny_running <- function() {
  if ("shiny" %in% loadedNamespaces()) {
    fun <- tryCatch(get("isRunning", envir = asNamespace("shiny"), inherits = FALSE),
                    error = function(e) NULL)
    if (is.function(fun)) {
      return(isTRUE(tryCatch(fun(), error = function(e) FALSE)))
    }
  }
  FALSE
}
