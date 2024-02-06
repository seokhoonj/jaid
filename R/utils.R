
sort_group_by <- function(x) {
  .Call(SortGroupBy, x)
}

match_cols <- function(df, cols) {
  colnames(df)[match(cols, colnames(df), 0L)]
}
