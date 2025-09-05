#' Scaler
#'
#' Apply common scaling transformations to a numeric vector.
#' Supports **min-max scaling**, **robust scaling** (median/IQR),
#' and **standard scaling** (z-score).
#'
#' @param x A numeric vector.
#' @param method Character string specifying the scaling method.
#'   One of `"minmax"`, `"robust"`, or `"standard"`.
#'
#' @return A numeric vector of the same length as `x`, scaled according
#'   to the chosen method.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(plotly)
#'
#' set.seed(123)
#' x <- rnorm(100)
#' df <- rbind(
#'   data.frame(method = "Min-max" , x = seq_along(x), y = scaler(x, "minmax")),
#'   data.frame(method = "Robust"  , x = seq_along(x), y = scaler(x, "robust")),
#'   data.frame(method = "Standard", x = seq_along(x), y = scaler(x, "standard"))
#' )
#'
#' ggplot(df, aes(x = x, y = y, group = method, color = method)) +
#'   geom_line() +
#'   labs(title = "Scaling Method: Min-max vs Robust vs Standard") +
#'   theme_bw()
#'
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
