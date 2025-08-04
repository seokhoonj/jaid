#' Scaler
#'
#' Scale a numeric vector using min-max, robust or standard method.
#'
#' @param x a numeric vector
#' @param method a string specifying a scaling method (minmax, robust, standard)
#' @return a scaled vector
#'
#' @examples
#' \dontrun{
#' # create scaled data
#' set.seed(123)
#' x <- rnorm(100)
#' df <- rbind(
#'   data.frame(method = "Min-max" , x = seq_along(x), y = scaler(x, "minmax")),
#'   data.frame(method = "Robust"  , x = seq_along(x), y = scaler(x, "robust")),
#'   data.frame(method = "Standard", x = seq_along(x), y = scaler(x, "standard"))
#' )
#' g <- ggshort::ggline(df, x = x, y = y, group = method, color = method) +
#'   ggplot2::labs(title = "Scaling Method: Min-max vs Robust vs Standard") +
#'   ggshort::theme_view()
#' plotly::ggplotly(g)
#' }
#'
#' @export
scaler <- function(x, method = c("minmax", "robust", "standard")) {
  if (length(x) > 1) {
    method <- match.arg(method)
    switch(method, minmax = minmax_scaler(x), standard = standard_scaler(x),
           robust = robust_scaler(x))
  } else {
    return(0)
  }
}

minmax_scaler <- function(x) {
  xmin <- min(x, na.rm = TRUE)
  xmax <- max(x, na.rm = TRUE)
  return((x - xmin)/(xmax - xmin))
}

robust_scaler <- function(x) {
  med <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  return((x - med) / iqr)
}

standard_scaler <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return((x - mu) / sigma)
}
