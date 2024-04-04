
write_data <- function(wb, sheet, data, rc = c(1L, 1L), rowNames = TRUE,
                       fontName = "Comic Sans MS", borderColour = "#000000",
                       widths = 8.43) {
  headerStyle1 <- openxlsx::createStyle(
    fontName = fontName,
    fontSize = 14,
    fontColour = "#000000",
    halign = "center",
    valign = "center",
    fgFill = "#E6E6E7",
    border = "TopRightBottom",
    borderColour = borderColour,
    borderStyle = c("thick", "thin", "double")
  )
  headerStyle2 <- openxlsx::createStyle(
    fontName = fontName,
    fontSize = 14,
    fontColour = "#000000",
    halign = "center",
    valign = "center",
    fgFill = "#E6E6E7",
    border = "TopBottom",
    borderColour = borderColour,
    borderStyle = c("thick", "double")
  )
  bodyStyle1 <- openxlsx::createStyle(
    fontName = fontName,
    border = "TopRightBottom",
    borderColour = borderColour
  )
  bodyStyle2 <- openxlsx::createStyle(
    fontName = fontName,
    border = "TopBottom",
    borderColour = borderColour
  )
  footerStyle1 <- openxlsx::createStyle(
    fontName = fontName,
    border = "TopRightBottom",
    borderColour = borderColour,
    borderStyle = c("thin", "thin", "thick")
  )
  footerStyle2 <- openxlsx::createStyle(
    fontName = fontName,
    border = "TopBottom",
    borderColour = borderColour,
    borderStyle = c("thin", "thick")
  )

  openxlsx::writeData(wb = wb, sheet = sheet, x = data, xy = rev(rc),
                      rowNames = rowNames)

  startCell <- rc
  endCell   <- startCell + dim(data)

  srow <- startCell[1L]
  scol <- startCell[2L]
  erow <- endCell[1L]
  ecol <- endCell[2L]

  if (!rowNames) ecol <- ecol - 1

  headerCols  <- scol:ecol
  headerRows1 <- srow
  headerCols1 <- scol:max(ecol-1, 1)
  headerRows2 <- srow
  headerCols2 <- ecol
  bodyRows1   <- (srow+1):max(erow-1, 2)
  bodyCols1   <- scol:max(ecol-1, 1)
  bodyRows2   <- (srow+1):max(erow-1, 2)
  bodyCols2   <- ecol
  footerRows1 <- erow
  footerCols1 <- scol:max(ecol-1, 1)
  footerRows2 <- erow
  footerCols2 <- ecol

  openxlsx::addStyle(wb, sheet = sheet, headerStyle1, rows = headerRows1,
                     cols = headerCols1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet, headerStyle2, rows = headerRows2,
                     cols = headerCols2, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet, bodyStyle1  , rows = bodyRows1,
                     cols = bodyCols1  , gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet, bodyStyle2  , rows = bodyRows2,
                     cols = bodyCols2  , gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet, footerStyle1, rows = footerRows1,
                     cols = footerCols1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet, footerStyle2, rows = footerRows2,
                     cols = footerCols2, gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet, cols = headerCols, widths = widths)
}

#' Write data in an excel file
#'
#' Write data in an excel file.
#'
#' @param data A data frame
#' @param file A file path
#' @param rc A vector of starting point row and column, default c(1L, 1L)
#' @param rowNames A boolean value contains row names or not
#' @param overwrite A boolean value that overwrite or not
#'
#' @return no return
#'
#' @examples
#' # write xlsx file
#' \dontrun{
#' write_xlsx(list(cars = cars, matcars = mtcars), "data.xlsx")}
#'
#' @export
data_xlsx <- function(data, file, rc = c(1L, 1L), rowNames = FALSE,
                      overwrite = FALSE) {
  if (is.data.frame(data))
    data <- list(data)
  if (!file.exists(file)) {
    wb <- openxlsx::createWorkbook()
    sheetNames <- NULL
  } else {
    wb <- openxlsx::loadWorkbook(file)
    sheetNames <- openxlsx::getSheetNames(file)
  }
  dataNames <- names(data)
  if (is.null(dataNames)) {
    dataNames <- sprintf("Sheet %s", seq_along(data))
  } else {
    dataLen <- length(dataNames[which(dataNames == "")])
    dataNames[which(dataNames == "")] <- sprintf("Sheet %s", seq_along(dataLen))
  }
  names(data) <- dataNames
  newNames <- setdiff(dataNames, sheetNames)
  lapply(seq_along(newNames), function(x)
    openxlsx::addWorksheet(wb = wb, sheetName = newNames[[x]], gridLines = FALSE))
  lapply(seq_along(data), function(x)
    write_data(wb, sheet = names(data)[[x]], data = data[[x]], rc = rc,
               rowNames = rowNames))
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

insert_plot <- function(wb, sheet, plot, rc = c(1L, 1L), width = 12, height = 6,
                        dpi = 300) {
  print(plot)
  openxlsx::insertPlot(wb, sheet = sheet, width = width, height = height,
                       startRow = rc[1L], startCol = rc[2L], dpi = dpi)
}

#' Draw plots in an excel file
#'
#' Draw plots in an excel file.
#'
#' @param plots ggplot files
#' @param file a file path
#' @param rc a vector of starting point row and column, default c(1L, 1L)
#' @param width image width
#' @param height image height
#' @param dpi image resolution
#' @param overwrite A boolean value that overwrite or not
#' @return no return
#'
#' @examples
#' # draw ggplot objects in an excel file
#' \dontrun{
#' draw_xlsx(list(image1, image2), "image.xlsx")}
#'
#' @export
plot_xlsx <- function(plots, file, rc = c(1L, 1L), width = 12, height = 6,
                      dpi = 300, overwrite = FALSE) {
  if (!file.exists(file)) {
    wb <- openxlsx::createWorkbook()
    sheetNames <- NULL
  } else {
    wb <- openxlsx::loadWorkbook(file)
    sheetNames <- openxlsx::getSheetNames(file)
  }
  plotNames <- names(plots)
  if (is.null(plotNames)) {
    plotNames <- sprintf("Sheet %s", seq_along(plots))
  } else {
    plotLen <- length(plotNames[which(plotNames == "")])
    plotNames[which(plotNames == "")] <- sprintf("Sheet %s", seq_along(plotLen))
  }
  names(plots) <- plotNames
  newNames <- setdiff(plotNames, sheetNames)
  lapply(seq_along(newNames), function(x)
    openxlsx::addWorksheet(wb = wb, sheetName = newNames[[x]], gridLines = FALSE))
  lapply(seq_along(plots), function(x) {
    print(plots[[x]])
    openxlsx::insertPlot(wb, sheet = names(plots)[[x]], width = width,
                         height = height, startRow = rc[1L], startCol = rc[2L],
                         dpi = dpi)
  })
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

#' Insert images in an excel file
#'
#' Insert images in an excel file.
#'
#' @param images image file paths
#' @param file a file path
#' @param rc a vector of starting point row and column, default c(1L, 1L)
#' @param width image width
#' @param height image height
#' @param dpi image resolution
#' @param overwrite A boolean value that overwrite or not
#' @return no return
#'
#' @examples
#' # insert images in an excel file
#' \dontrun{
#' image_xlsx(list(image1, image2), "image.xlsx")}
#'
#' @export
image_xlsx <- function(images, file, rc = c(1L, 1L), width = 12, height = 6,
                       dpi = 300, overwrite = FALSE) {
  if (!file.exists(file)) {
    wb <- openxlsx::createWorkbook()
    sheetNames <- NULL
  } else {
    wb <- openxlsx::loadWorkbook(file)
    sheetNames <- openxlsx::getSheetNames(file)
  }
  imageNames <- names(images)
  if (is.null(imageNames)) {
    imageNames <- sprintf("Sheet %s", seq_along(images))
  } else {
    imageLen <- length(imageNames[which(imageNames == "")])
    imageNames[which(imageNames == "")] <- sprintf("Sheet %s", seq_along(imageLen))
  }
  names(images) <- imageNames
  newNames <- setdiff(imageNames, sheetNames)
  lapply(seq_along(newNames), function(x)
    openxlsx::addWorksheet(wb = wb, sheetName = newNames[[x]], gridLines = FALSE)
  )
  lapply(seq_along(images), function(x)
    openxlsx::insertImage(wb, sheet = names(images)[[x]], width = width,
                          height = height, file = images[[x]],
                          startRow = rc[1L], startCol = rc[2L], dpi = dpi)
  )
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = overwrite)
}

# data image --------------------------------------------------------------

#' Plot a data image
#'
#' Plot a data image (html < png < Plots in RStudio).
#'
#' @param data a data.frame
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
#' @return no return value.
#'
#' @examples
#' # plot a data image
#' \dontrun{
#' plot_data_image(head(data), height = .5)}
#'
#' @export
plot_data_image <- function(data, caption = "Table.1", footnote = NULL,
                            digits = 2, full_width = FALSE,
                            html_font = "Comic Sans MS", zoom = 1.5,
                            width = NULL, height = .5) {
  file <- sprintf("%s.png", tempfile())
  op <- options(knitr.kable.NA = "")
  on.exit(op)
  kableExtra::kbl(data, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote) |>
    kableExtra::save_kable(file, zoom = zoom)
  png <- png::readPNG(file)
  while (!is.null(dev.list())) dev.off()
  grid::grid.raster(png, width = width, height = height)
}

#' View a html data image
#'
#' View a html data image (Viewer in RStudio).
#'
#' @param data a data.frame
#' @param caption The table caption.
#' @param footnote A vector of footnote texts, Footnotes here will be labeled with special symbols.
#' The vector here should not have more than 20 elements.
#' @param digits Maximum number of digits for numeric columns, passed to round(). This can also be a vector of length ncol(x), to set the
#' number of digits for individual columns.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have the preferable format for
#' `full_width`. If not specified, a HTML table will have full width by default but this option will be set to `FALSE` for a LaTeX table
#' @param html_font A string for HTML css font.
#' @return no return value.
#'
#' @examples
#' # plot a data image
#' \dontrun{
#' plot_data_image(head(data), height = .5)}
#'
#' @export
view_data_image <- function(data, caption = "Table.1", footnote = NULL,
                            digits = 2, full_width = FALSE,
                            html_font = "Comic Sans MS") {
  op <- options(knitr.kable.NA = "")
  on.exit(op)
  kableExtra::kbl(data, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote)
}

#' @rdname view_data_image
#' @param vwidth Viewport width. This is the width of the browser "window".
#' @param vheight Viewport height This is the height of the browser "window".
#' @param zoom A number specifying the zoom factor. A zoom factor of 2 will result in twice as many pixels vertically and horizontally. Note that
#' using 2 is not exactly the same as taking a screenshot on a HiDPI (Retina) device: it is like increasing the zoom to 200 doubling the
#' height and width of the browser window. This differs from using a HiDPI device because some web pages load different, higher-
#' resolution images when they know they will be displayed on a HiDPI device (but using zoom will not report that there is a HiDPI
#' device).
#' @param file A file path
#' @export
save_data_image <- function(data, caption = "Table.1", footnote = NULL,
                            digits = 2, full_width = FALSE,
                            html_font = "Comic Sans MS",
                            vwidth = 992, vheight = 744, zoom = 1.5, file) {
  if (missing(file))
    file <- sprintf("%s.png", tempfile())
  op <- options(knitr.kable.NA = "")
  on.exit(op)
  kableExtra::kbl(data, caption = caption, digits = digits,
                  format.args = list(big.mark = ",")) |>
    kableExtra::kable_classic(full_width = full_width, html_font = html_font) |>
    kableExtra::footnote(symbol = footnote) |>
    kableExtra::save_kable(file, vwidth = vwidth, vheight = vheight, zoom = zoom)
}
