
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
write_xlsx <- function(data, file, rc = c(1L, 1L), rowNames = FALSE,
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

draw_image <- function(wb, sheet, image, rc = c(1L, 1L), width = 12, height = 6) {
  print(image)
  openxlsx::insertPlot(wb, sheet = sheet, width = width, height = height,
                       xy = rev(rc))
}

#' Draw images in an excel file
#'
#' Draw images in an excel file.
#'
#' @param images image files
#' @param file A file path
#' @param rc A vector of starting point row and column, default c(1L, 1L)
#' @param width image width
#' @param height image height
#' @param overwrite A boolean value that overwrite or not
#'
#' @return no return
#'
#' @examples
#' # draw images in an excel file
#' \dontrun{
#' draw_xlsx(list(image1, image2), "image.xlsx")}
#'
#' @export
draw_xlsx <- function(images, file, rc = c(1L, 1L), width = 12, height = 6,
                      overwrite = FALSE) {
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
    openxlsx::addWorksheet(wb = wb, sheetName = newNames[[x]], gridLines = FALSE))
  lapply(seq_along(images), function(x)
    draw_image(wb, sheet = names(images)[[x]], image = images[[x]], rc = rc))
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
}


# save_xlsx <- function(..., file, width = 12, height = 6, overwrite = FALSE) {
#   data_list <- list(...)
#   wb <- createWorkbook()
#   sheetName <- NULL
#   for (i in seq_along(data_list)) {
#     data <- data_list[[i]][[1L]]
#     if (is.data.frame(data) | is.ggplot(data))
#       data <- list(data)
#     dataSheetName <- names(data)
#     if (is.null(dataSheetName))
#       dataSheetName <- sprintf("Sheet %s", seq_along(data))
#     sheetName <- c(sheetName, dataSheetName)
#   }
#   sheetName <- unique(sheetName)
#   lapply(seq_along(sheetName), function(x) {
#     addWorksheet(wb = wb, sheetName = sheetName[[x]], gridLines = FALSE)
#   })
#   for (i in seq_along(data_list)) {
#     data <- data_list[[i]][[1L]]
#     if (is.data.frame(data) | is.ggplot(data))
#       data <- list(data)
#     dataSheetName <- names(data)
#     if (is.null(dataSheetName))
#       dataSheetName <- sprintf("Sheet %s", seq_along(data))
#     if (is.data.frame(data) | is.ggplot(data))
#       data <- list(data)
#     rc <- if (length(data_list[[i]]) >= 2) data_list[[i]][[2L]] else c(1, 1)
#     w  <- if (length(data_list[[i]]) >= 3) data_list[[i]][[3L]] else width
#     h  <- if (length(data_list[[i]]) == 4) data_list[[i]][[4L]] else height
#     if (is.data.frame(data[[1L]]))
#       lapply(seq_along(data), function(x) {
#         write_data(wb, sheet = dataSheetName[[x]], data = data[[x]],
#                    rc = rc, rowNames = FALSE)
#       })
#     if (is.ggplot(data[[1L]]))
#       lapply(seq_along(data), function(x) {
#         draw_image(wb, sheet = dataSheetName[[x]], image = data[[x]],
#                    rc = rc, width = w, height = h)
#       })
#   }
#   saveWorkbook(wb = wb, file = file, overwrite = overwrite)
# }
