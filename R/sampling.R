#' Stratified sampling (by data.table groups)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Perform stratified sampling on a data.table, drawing a specified number
#' of rows **within each group**.
#'
#' @details
#' Let *n<sub>g</sub>* be the size of group *g*.
#'
#' * If `0 < size < 1`, the target sample size per group is computed as
#'   `method(`*n<sub>g</sub>*` * size)` where `method` is one of `"round"`, `"floor"`,
#'   or `"ceiling"`.
#' * If `size >= 1`, the target sample size per group is the same constant
#'   `size` for all groups.
#' * If `contain0 = FALSE` (default), any group whose target becomes 0 is
#'   bumped up to 1 so that **every group contributes at least one row**.
#' * Sampling is done independently within each group using
#'   `base::sample.int()` with `replace` as specified.
#'
#' The result carries an attribute `"group"` (a data.table of
#' group counts and final per-group sample sizes) for auditing.
#'
#' @param df A data.table.
#' @param group_var Grouping columns. Supply bare names in a tidy style,
#'   e.g. `.(Species)` or `.(g1, g2)`.
#' @param size A positive number controlling per-group sample size:
#'   a proportion if `0 < size < 1`, otherwise an absolute count
#'   (same for all groups).
#' @param replace Logical; sample with replacement? Default `TRUE`.
#' @param contain0 Logical; if `TRUE`, groups with computed target 0 are
#'   allowed to contribute 0 rows. If `FALSE` (default), such groups are
#'   forced to contribute 1 row.
#' @param method Rounding method used when `0 < size < 1`.
#'   One of `"round"`, `"floor"`, or `"ceiling"`. Ignored when `size >= 1`.
#' @param verbose Logical; if `TRUE`, print a sampling summary table.
#'   Default `TRUE`.
#' @param seed Optional integer; if supplied, sets the RNG seed for
#'   reproducibility. If `NULL`, no seed is set. (Default `123`.)
#'
#' @return A data.table consisting of the sampled rows. An attribute
#'   `"group"` is attached describing per-group population size, target
#'   sample size, and realized proportion.
#'
#' @examples
#' \donttest{
#' dt <- data.table::as.data.table(iris)
#'
#' # 10% per-group sample (rounded), with replacement
#' stratified_sampling(dt, group_var = .(Species), size = 0.1, method = "round")
#'
#' # Fixed 5 rows per group (same count for all groups)
#' stratified_sampling(dt, group_var = .(Species), size = 5, replace = FALSE)
#'
#' # Allow groups to contribute zero when proportion rounds to 0
#' stratified_sampling(dt, group_var = .(Species), size = 0.01,
#'                     method = "floor", contain0 = TRUE)
#'
#' # Reproducible result with a fixed seed
#' stratified_sampling(dt, group_var = .(Species), size = 0.2, seed = 42)
#' }
#'
#' @export
stratified_sampling <- function(df, group_var, size, replace = TRUE,
                                contain0 = FALSE,
                                method = c("round", "floor", "ceiling"),
                                verbose = TRUE, seed = 123) {
  lifecycle::signal_stage("experimental", "stratified_sampling()")
  assert_class(df, "data.frame")

  env <- ensure_dt_env(df)
  dt <- env$dt

  group_var <- capture_names(dt, !!rlang::enquo(group_var))
  group <- dt[, .(n = .N), keyby = group_var]
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
  if (verbose && size < 1) {
    cli::cat_rule(line = 2)
    cat(sprintf("Target prop: %.2f %% (method = %s, replace = %s)\n",
                size * 100, method, replace))
    cat(sprintf("Population : %s unit\n",
                stringr::str_pad(scales::comma(sum(group$n)), width = 14,
                                 pad = " ")))
    cat(sprintf("Sample     : %s unit\n",
                stringr::str_pad(scales::comma(sum(group$s)), width = 14,
                                 pad = " ")))
    cat(sprintf("Actual prop: %.2f %%\n", sum(group$s)/sum(group$n) * 100))
    cli::cat_rule(line = 2)
    print(cbind(group, prop = sprintf("%.2f %%", group$p * 100)))
    cli::cat_rule(line = 2)
  }
  if (nrow(group) > 1) {
    g <- NULL
    dt[group, on = group_var, `:=`(g, g)]
  }
  else {
    data.table::set(dt, j = "g", value = 1L)
  }
  n <- group$n
  s <- group$s
  spl <- split(seq_len(nrow(dt)), dt$g)
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
  z <- dt[v]
  data.table::setorder(z, g)
  data.table::setattr(z, "group", group)
  data.table::set(z, j = "g", value = NULL)

  env$restore(z)
}

#' Randomly sample rows from a data frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draw a random sample of rows from a data frame. This is a simple wrapper
#' around [base::sample.int()] that preserves the data frame structure.
#'
#' @param x A data.frame.
#' @param size A non-negative integer giving the number of rows to sample.
#' @param replace Logical. Should sampling be with replacement? Defaults to `TRUE`.
#' @param prob A numeric vector of probability weights for sampling. Its length
#'   must equal `nrow(x)`.
#' @param seed An optional integer. If supplied, sets the random seed for
#'   reproducibility. If `NULL` (default), no seed is set.
#'
#' @return A data.frame containing the sampled rows.
#'
#' @examples
#' \donttest{
#' # Sample 5 rows with replacement
#' random_sampling(iris, size = 5)
#'
#' # Sample rows without replacement
#' random_sampling(iris, size = 5, replace = FALSE)
#'
#' # Weighted sampling
#' w <- runif(nrow(iris))
#' random_sampling(iris, size = 5, prob = w)
#'
#' # Reproducible sampling
#' random_sampling(iris, size = 5, seed = 42)
#' }
#'
#' @export
random_sampling <- function(x, size, replace = TRUE, prob = NULL, seed = NULL) {
  lifecycle::signal_stage("experimental", "random_sampling()")
  if (!inherits(x, "data.frame"))
    stop("`x` must be a data frame.", call. = FALSE)

  if (!is.null(seed)) {
    old_seed <- .Random.seed
    on.exit({
      if (exists("old_seed", inherits = FALSE)) .Random.seed <<- old_seed
    }, add = TRUE)
    set.seed(seed)
  }

  idx <- sample.int(nrow(x), size, replace, prob)
  x[idx, , drop = FALSE]
}
