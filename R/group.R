#' Find group break positions
#'
#' Returns the break indices between groups of consecutive identical values
#' (or rows, if `x` is a data.frame/data.table with multiple columns).
#'
#' @param x A vector, data.frame, or data.table. Consecutive runs of equal
#'   values are treated as groups. For multiple columns, rows are compared
#'   elementwise.
#'
#' @return An integer vector of break indices (1-based, end positions of groups).
#'
#' @details
#' - This is a thin wrapper over the C routine `FindGroupBreaks`.
#' - The first element (`0`) returned by the C function is dropped.
#' - Output always includes the final row index.
#'
#' @examples
#' \donttest{
#' find_group_breaks(c("a","a","b","b","c"))
#' # [1] 2 4 5
#' }
#'
#' @export
find_group_breaks <- function(x) {
  .Call(FindGroupBreaks, x)[-1L]
}

#' Find group start positions
#'
#' Returns the starting indices of groups of consecutive identical values.
#'
#' @inheritParams find_group_breaks
#'
#' @return An integer vector of group start indices (1-based).
#'
#' @details
#' - Computed from `FindGroupBreaks`.
#' - Equivalent to shifting the break indices by +1 and dropping the last element.
#'
#' @examples
#' \donttest{
#' find_group_starts(c("a","a","b","b","c"))
#' # [1] 1 3 5
#' }
#'
#' @export
find_group_starts <- function(x) {
  z <- .Call(FindGroupBreaks, x)
  (z + 1L)[-length(z)]
}

#' Find group sizes
#'
#' Returns the size (row count) of each run of consecutive identical values.
#'
#' @inheritParams find_group_breaks
#'
#' @return An integer vector of group sizes.
#'
#' @details
#' - Computed by taking differences of group break positions.
#' - Implemented in C for efficiency (`FindGroupSizes`).
#'
#' @examples
#' \donttest{
#' find_group_sizes(c("a","a","b","b","c"))
#' # [1] 2 2 1
#' }
#'
#' @export
find_group_sizes <- function(x) {
  .Call(FindGroupSizes, x)
}
