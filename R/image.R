#' Plot a table as an image
#'
#' Render a data.frame into a PNG image using `kableExtra` and display it
#' on the current graphics device via `grid`. Uses `magick` for PNG
#' reading to avoid GPL dependencies.
#'
#' @param table A data.frame to render
#' @param caption The table caption.
#' @param footnote A vector of footnote texts, Footnotes here will be labeled with special symbols.
#' The vector here should not have more than 20 elements.
#' @param digits Maximum number of digits for numeric columns, passed to round(). This can also be a vector of length ncol(x), to set the
#' number of digits for individual columns.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have the preferable format for
#' `full_width`. If not specified, a HTML table will have full width by default but this option will be set to `FALSE` for a LaTeX table
#' @param html_font A string for HTML css font.
#' @param zoom A number specifying the zoom factor. A zoom factor of 2 will result in twice as many pixels vertically and horizontally. Note that
#' using 2 is not exactly the same as taking a screenshot on a HiDPI (Retina) device: it is like increasing the zoom to 200 doubling the
#' height and width of the browser window. This differs from using a HiDPI device because some web pages load different, higher-
#' resolution images when they know they will be displayed on a HiDPI device (but using zoom will not report that there is a HiDPI
#' device).
#' @param width A numeric vector or unit object specifying width.
#' @param height A numeric vector or unit object specifying height.
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#' # Plot a table image
#' plot_table_image(head(iris), height = .5)
#' }
#'
#' @export
plot_table_image <- function(table, caption = "Table.1", footnote = NULL,
                             digits = 2, full_width = FALSE,
                             html_font = "Comic Sans MS", zoom = 1.5,
                             width = NULL, height = .5) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Install 'magick' to use this function: install.packages('magick')",
         call. = FALSE)
  }

  file <- sprintf("%s.png", tempfile())
  op <- options(knitr.kable.NA = "")
  on.exit(op)

  kableExtra::kbl(table, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote) |>
    kableExtra::save_kable(file, zoom = zoom)

  img <- magick::image_read(file)

  grid::grid.newpage()
  grid::grid.raster(grDevices::as.raster(img), width = width, height = height,
                    interpolate = TRUE)

  invisible(img)
}

#' View a table as an HTML table
#'
#' Render a table into an HTML table and display it in the RStudio Viewer
#' (or browser if available).
#'
#' @param table A data.frame, matrix, or table object to be rendered as an HTML table.
#' @param caption The table caption.
#' @param footnote A vector of footnote texts, Footnotes here will be labeled with special symbols.
#' The vector here should not have more than 20 elements.
#' @param digits Maximum number of digits for numeric columns, passed to round(). This can also be a vector of length ncol(x), to set the
#' number of digits for individual columns.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have the preferable format for
#' `full_width`. If not specified, a HTML table will have full width by default but this option will be set to `FALSE` for a LaTeX table
#' @param html_font A string for HTML css font.
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#' # 2x2 Table with totals
#' set.seed(1)
#' smoker  <- sample(c("Yes","No"),  100, TRUE)
#' disease <- sample(c("Yes","No"),  100, TRUE)
#' tab <- table(Smoker = smoker, Disease = disease)
#' tab_tot <- addmargins(tab, margin = c(1, 2))
#' view_table_html(tab_tot, caption = "2x2 Table with Totals")
#' }
#'
#' @export
view_table_html <- function(table, caption = "Table.1", footnote = NULL,
                            digits = 2, full_width = FALSE,
                            html_font = "Comic Sans MS") {
  op <- options(knitr.kable.NA = "")
  on.exit(op)
  kableExtra::kbl(table, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote)
}

#' Save a table as an image
#'
#' Render a data.frame (or compatible object) into an HTML table and save it
#' as an image file (e.g., PNG).
#'
#' @param table A data.frame, matrix, or table object to be rendered as an HTML table.
#' @param caption The table caption.
#' @param footnote A vector of footnote texts, Footnotes here will be labeled with special symbols.
#' The vector here should not have more than 20 elements.
#' @param digits Maximum number of digits for numeric columns, passed to round(). This can also be a vector of length ncol(x), to set the
#' number of digits for individual columns.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have the preferable format for
#' `full_width`. If not specified, a HTML table will have full width by default but this option will be set to `FALSE` for a LaTeX table
#' @param html_font A string for HTML css font.
#' @param vwidth Viewport width. This is the width of the browser "window".
#' @param vheight Viewport height This is the height of the browser "window".
#' @param zoom A number specifying the zoom factor. A zoom factor of 2 will result in twice as many pixels vertically and horizontally. Note that
#' using 2 is not exactly the same as taking a screenshot on a HiDPI (Retina) device: it is like increasing the zoom to 200 doubling the
#' height and width of the browser window. This differs from using a HiDPI device because some web pages load different, higher-
#' resolution images when they know they will be displayed on a HiDPI device (but using zoom will not report that there is a HiDPI
#' device).
#' @param file A file path where the image will be saved.
#'
#' @return No return value.
#'
#' @export
save_table_image <- function(table, caption = "Table.1", footnote = NULL,
                             digits = 2, full_width = FALSE,
                             html_font = "Comic Sans MS",
                             vwidth = 992, vheight = 744, zoom = 1.5, file) {
  if (missing(file))
    file <- sprintf("%s.png", tempfile())

  op <- options(knitr.kable.NA = "")
  on.exit(op)

  kableExtra::kbl(table, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote) |>
    kableExtra::save_kable(file, vwidth = vwidth, vheight = vheight, zoom = zoom)
}

#' Plot an Image file
#'
#' Reads an image using the `magick` package and displays it on the
#' current graphics device via `grid`.
#'
#' @param file Path or URL to a PNG image file.
#' @param width Optional width for the image, passed to [grid::grid.raster()].
#'   Can be a [grid::unit()] object or `NULL` for default sizing.
#' @param height Optional height for the image, passed to [grid::grid.raster()].
#'   Can be a [grid::unit()] object or `NULL` for default sizing.
#' @param clear Logical flag; if `TRUE` (default), a new page is started with
#'   [grid::grid.newpage()] before drawing the image.
#'
#' @return Invisibly returns the `magick-image` object read from `file`.
#'
#' @examples
#' \dontrun{
#' plot_image(system.file("img", "Rlogo.png", package = "png"),
#'            width = grid::unit(0.1, "npc"),
#'            height = grid::unit(0.1, "npc"))
#' }
#'
#' @export
plot_image <- function(file, width = NULL, height = NULL, clear = TRUE) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Install 'magick' to use this function: install.packages('magick')", call. = FALSE)
  }
  img <- magick::image_read(file)
  if (isTRUE(clear))
    grid::grid.newpage()
  grid::grid.raster(grDevices::as.raster(img), width = width, height = height,
                    interpolate = TRUE)
  invisible(img)
}
