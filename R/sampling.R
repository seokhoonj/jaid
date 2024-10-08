#' Stratified Sampling
#'
#' Stratified Sampling
#'
#' @param df data.table object
#' @param group_var names of group variables
#' @param size a positive numeric sampling size. if the size < 0, it's proportion.
#' @param replace should sampling be with replacement?
#' @param contain0 whether to include a specific group if the group proportion is 0
#' @param method a rounding method c('round', 'floor', 'ceiling')
#' @param verbose if `TRUE`sampling summary will be shown.
#' @param seed a single value, interpreted as integer, or NULL
#' @return a data.table
#'
#' @examples
#' \dontrun{
#' dt <- iris
#' data.table::setDT(dt)
#' strati_sampling(dt, size = 0.1)
#' strati_sampling(dt, group_var = Species, size = 0.1)}
#'
#' @export
strati_sampling <- function(df, group_var, size, replace = TRUE, contain0 = FALSE,
                            method = c("round", "floor", "ceiling"), verbose = TRUE,
                            seed = 123) {
  assert_class(df, "data.table")
  # group_var <- match_cols(df, vapply(substitute(group_var), deparse, "character"))
  group_var <- match_cols(df, sapply(rlang::enexpr(group_var), rlang::as_name))
  group <- df[, .(n = .N), keyby = group_var]
  data.table::set(group, j = "g", value = seq_len(nrow(group)))
  if (size > 0 & size < 1) {
    method <- match.arg(method)
    data.table::set(group, j = "s", value = do.call(method, list(x = group$n * size)))
  }
  else if (size >= 1) {
    method <- "none"
    data.table::set(group, j = "s", value = size)
  }
  if (!contain0)
    data.table::set(group, i = which(group$s == 0), j = "s", value = 1)
  data.table::set(group, j = "p", value = group$s / group$n)
  if (verbose) {
    cat(draw_line(), "\n")
    cat(sprintf("Target prop: %.2f %% (method = %s, replace = %s)\n",
                size * 100, method, replace))
    cat(sprintf("Population : %s unit\n",
                stringr::str_pad(scales::comma(sum(group$n)), widfh = 14,
                                 pad = " ")))
    cat(sprintf("Sample     : %s unit\n",
                stringr::str_pad(scales::comma(sum(group$s)), widfh = 14,
                                 pad = " ")))
    cat(sprintf("Actual prop: %.2f %%\n", sum(group$s)/sum(group$n) * 100))
    cat(draw_line(), "\n")
    print(cbind(group, prop = sprintf("%.2f %%", group$p * 100)))
    cat(draw_line(), "\n")
  }
  if (nrow(group) > 1) {
    g <- NULL
    df[group, on = group_var, `:=`(g, g)]
  }
  else {
    data.table::set(df, j = "g", value = 1L)
  }
  n <- group$n
  s <- group$s
  spl <- split(seq_len(nrow(df)), df$g)
  if (!missing(seed))
    set.seed(seed)
  v <- sort(unlist(lapply(seq_along(spl), function(x) {
    if (n[x] > 1) {
      sample(spl[[x]], s[x], replace = replace)
    }
    else {
      sample(unname(spl[x]), s[x], replace = replace)
    }
  })))
  z <- df[v]
  data.table::setorder(z, g)
  data.table::setattr(z, "group", group)
  rm_cols(z, g)
  return(z[])
}

#' Random sampling
#'
#' Random sampling for a `data.frame` or a vector
#'
#' @param x either a `data.frame` or a vector
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param seed a single value, interpreted as an integer, or NULL.
#' @return either a `data.frame` or a vector
#'
#' @examples
#' \dontrun{
#' random_sampling(iris)}
#'
#' @export
random_sampling <- function(x, size, replace = TRUE, prob = NULL, seed = 123) {
  if (is.vector(x))
    return(x[sample.int(NROW(x), size, replace, prob)])
  if (is.data.frame(x))
    return(x[sample.int(NROW(x), size, replace, prob),])
  stop("Not an object of class: ", class(x), call. = FALSE)
}
