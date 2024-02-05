
# Combine overlapping dates
# unique_date_range_overlap <- function(df, id_var, merge_var, from_var, to_var, interval = 0) {
#   id_var    <- match_cols(df, vapply(substitute(id_var)   , deparse, "character"))
#   merge_var <- match_cols(df, vapply(substitute(merge_var), deparse, "character"))
#   from_var  <- match_cols(df, deparse(substitute(from_var)))
#   to_var  <- match_cols(df, deparse(substitute(to_var)))
#   vars <- c(id_var, merge_var, from_var, to_var)
#   tmp <- df[, ..vars]
#   setnames(tmp, c(id_var, merge_var, "from", "to"))
#   setorderv(tmp, c(id_var, "from", "to"))
#   set(tmp, j = "sub_stay", value = 0)
#   ind <- .Call(IndexDateRangeOverlap, tmp[, ..id_var],
#                as_integer(tmp$from),
#                as_integer(tmp$to),
#                as_integer(interval))
#   set(tmp, j = "loc", value = ind$loc)
#   set(tmp, j = "sub", value = ind$sub)
#   group <- c(id_var, "loc")
#   m <- tmp[, lapply(.SD, glue_code), keyby = group, .SDcols = merge_var]
#   s <- tmp[, .(from = min(from), to = max(to), sub_stay = sum(sub_stay) + sum(sub)),
#            keyby = group]
#   z <- m[s, on = group]
#   set(z, j = "loc", value = NULL)
#   set(z, j = "stay", value = as.numeric(z$to - z$from + 1 - z$sub_stay))
#   set(z, j = "sub_stay", value = NULL)
#   setnames(z, c(vars, "stay"))
#   return(z)
# }
