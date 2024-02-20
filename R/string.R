
# pste_string <- function(x, collapse = "|") paste(x, collapse = collapse)
# glue_string <- function(x, collapse = "|") paste(unique(x[!is.na(x)]), collapse = collapse)
# sort_string <- function(x, collapse = "|") paste(sort(unique(x[!is.na(x)])), collapse = collapse)
# splt_string <- function(x, split = "\\|") {z <- strsplit(x, split = split)[[1L]]; z[!z %in% c(NA, "NA", "")]}
# srch_string <- function(x) glue_string(paste0(x, "$"))
# melt_string <- function(x) srch_string(splt_string(pste_string(x)))
# excl_string <- function(x) paste0('^((?!', x, ').)*$')
# remv_string <- function(string, x) gsub(string, "", x)
# pull_string <- function(string, x, ignore.case = TRUE) {
#   r <- regexpr(string, x, ignore.case = ignore.case, perl = TRUE)
#   z <- rep(NA, length(x))
#   z[r != -1] <- regmatches(x, r)
#   return(z)
# }
# pull_string_all <- function(string, x, collapse = "|", ignore.case = TRUE) {
#   r <- gregexpr(string, x, ignore.case = ignore.case, perl = TRUE)
#   z <- regmatches(x, r)
#   sapply(z, function(s) paste(s, collapse = collapse))
# }
# pull_excl_part <- function(x, ignore.case = TRUE) {
#   r <- gregexpr("\\(.*?\\)", x, ignore.case = ignore.case, perl = TRUE)
#   z <- regmatches(x, r)
#   gsub("[\\(\\)]", "", z)
# }
# pull_excl_term <- function(x, ignore.case = TRUE) {
#   as_integer(gsub("[0-9].EXCL|EXCL|\\(.*?\\)", "", x, ignore.case = ignore.case, perl = TRUE))
# }
