
plot_png <- function(png, width = NULL, height = NULL) {
  png <- png::readPNG(png)
  while (!is.null(dev.list())) dev.off()
  grid::grid.raster(png, width = width, height = height)
}
