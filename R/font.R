#' List available fonts on the system
#'
#' Returns a list of fonts available on the current system.
#'
#' @param pattern Optional pattern to filter font names.
#'
#' @return Character vector of available font names.
#'
#' @examples
#' \donttest{
#' # List all fonts
#' list_fonts()
#'
#' # Fonts containing "Arial"
#' list_fonts("Arial")
#' }
#'
#' @export
list_fonts <- function(pattern = NULL) {
  os_type <- Sys.info()[["sysname"]]

  if (os_type == "Linux") {
    tryCatch({
      # Get font families from fc-list
      result <- system(
        "fc-list : family | cut -d, -f1 | sort -u",
        intern = TRUE, ignore.stderr = TRUE
      )
      if (!is.null(pattern)) {
        result <- result[grepl(pattern, result, ignore.case = TRUE)]
      }
      return(result)
    }, error = function(e) {
      return(character(0))
    })
  } else if (os_type == "Darwin") {
    tryCatch({
      # Use system_profiler on macOS
      result <- system(
        "system_profiler SPFontsDataType 2>/dev/null | grep 'Full Name:' | sed 's/.*Full Name: //' | sort -u",
        intern = TRUE, ignore.stderr = TRUE
      )
      if (!is.null(pattern)) {
        result <- result[grepl(pattern, result, ignore.case = TRUE)]
      }
      return(result)
    }, error = function(e) {
      return(character(0))
    })
  } else {
    # Return common fonts for other systems
    common_fonts <- c("Arial", "Times New Roman", "Calibri", "Comic Sans MS",
                      "Courier New", "Verdana", "Tahoma", "Georgia", "Impact")
    if (!is.null(pattern)) {
      common_fonts <- common_fonts[grepl(pattern, common_fonts, ignore.case = TRUE)]
    }
    return(common_fonts)
  }
}

#' Check if font is available on the system
#'
#' Checks if a font is available on the current system.
#'
#' @param font_name Character string specifying the font name.
#'
#' @return Logical value indicating if font is available.
#'
#' @examples
#' \donttest{
#' # Check if font exists
#' is_font_available("Arial")
#' is_font_available("NonExistentFont")
#' }
#'
#' @export
is_font_available <- function(font_name) {
  if (is.null(font_name) || !is.character(font_name) || length(font_name) != 1L) {
    return(FALSE)
  }

  # Try different methods based on OS
  os_type <- Sys.info()[["sysname"]]

  if (os_type == "Linux") {
    # Use fc-list command on Linux
    tryCatch({
      result <- system(paste0("fc-list 2>/dev/null | grep -i '", font_name, "' 2>/dev/null"),
                      intern = TRUE, ignore.stderr = TRUE)
      return(length(result) > 0)
    }, error = function(e) {
      return(FALSE)
    })
  } else if (os_type == "Darwin") {
    # Use system_profiler on macOS
    tryCatch({
      result <- system("system_profiler SPFontsDataType 2>/dev/null | grep -i ",
                      shQuote(font_name), intern = TRUE, ignore.stderr = TRUE)
      return(length(result) > 0)
    }, error = function(e) {
      return(FALSE)
    })
  } else if (os_type == "Windows") {
    # Check Windows registry or use PowerShell
    tryCatch({
      # Simple check - assume common fonts are available
      common_fonts <- c("Arial", "Times New Roman", "Calibri", "Comic Sans MS",
                        "Courier New", "Verdana", "Tahoma")
      return(font_name %in% common_fonts)
    }, error = function(e) {
      return(FALSE)
    })
  }

  # Fallback: assume font is available if we can't check
  return(TRUE)
}
