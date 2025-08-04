pkgname <- "jaid"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('jaid')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_mon")
### * add_mon

flush(stderr()); flush(stdout())

### Name: add_mon
### Title: Add months
### Aliases: add_mon

### ** Examples

# add months




cleanEx()
nameEx("add_year")
### * add_year

flush(stderr()); flush(stdout())

### Name: add_year
### Title: Add years
### Aliases: add_year

### ** Examples

# add years




cleanEx()
nameEx("as_comma")
### * as_comma

flush(stderr()); flush(stdout())

### Name: as_comma
### Title: As comma applied label
### Aliases: as_comma

### ** Examples

# convert to a comma applied string vector




cleanEx()
nameEx("assert_class")
### * assert_class

flush(stderr()); flush(stdout())

### Name: assert_class
### Title: Assert class
### Aliases: assert_class

### ** Examples

# assert object class




cleanEx()
nameEx("bmonth")
### * bmonth

flush(stderr()); flush(stdout())

### Name: bmonth
### Title: Beginning of the month, End of the month
### Aliases: bmonth emonth

### ** Examples

# the beginning of the month

# the end of the month




cleanEx()
nameEx("check_col_spec")
### * check_col_spec

flush(stderr()); flush(stdout())

### Name: check_col_spec
### Title: Check Column Specification Against a Data Frame
### Aliases: check_col_spec

### ** Examples


df <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 28),
  paid = c(TRUE, FALSE, TRUE)
)

col_spec <- list(
  id = "character",
  name = "character",
  age = "integer",
  premium = "numeric"
)

check_col_spec(df, col_spec)




cleanEx()
nameEx("col_max")
### * col_max

flush(stderr()); flush(stdout())

### Name: col_max
### Title: max, min, sum on each column of a matrix
### Aliases: col_max col_min col_sum

### ** Examples

# column max

# column min

# column sum




cleanEx()
nameEx("combine_overlapping_date_range")
### * combine_overlapping_date_range

flush(stderr()); flush(stdout())

### Name: combine_overlapping_date_range
### Title: Combine overlapping date ranges
### Aliases: combine_overlapping_date_range

### ** Examples

# combine overlapping date ranges
## Not run: 
##D id <- c("A", "A", "B")
##D work <- c("cleansing", "analysis", "cleansing")
##D sdate <- as.Date(c("2022-03-01", "2022-03-05", "2022-03-08"))
##D edate <- as.Date(c("2022-03-06", "2022-03-09", "2022-03-10"))
##D df <- data.table::data.table(id = id, work = work, sdate = sdate, edate = edate)
##D combine_overlapping_date_range(df, id, work, sdate, edate, interval = 0)
## End(Not run)




cleanEx()
nameEx("count_pattern")
### * count_pattern

flush(stderr()); flush(stdout())

### Name: count_pattern
### Title: Count pattern matched strings
### Aliases: count_pattern

### ** Examples

# count pattern matched strings from a string vector




cleanEx()
nameEx("create_library_r")
### * create_library_r

flush(stderr()); flush(stdout())

### Name: create_library_r
### Title: Create library.R
### Aliases: create_library_r

### ** Examples

# create library.R file
## Not run: create_library_r()




cleanEx()
nameEx("data_xlsx")
### * data_xlsx

flush(stderr()); flush(stdout())

### Name: data_xlsx
### Title: Write data in an excel file
### Aliases: data_xlsx

### ** Examples

# write xlsx file
## Not run: 
##D write_xlsx(list(cars = cars, matcars = mtcars), "data.xlsx")
## End(Not run)




cleanEx()
nameEx("del_pattern")
### * del_pattern

flush(stderr()); flush(stdout())

### Name: del_pattern
### Title: Delete patterns
### Aliases: del_pattern

### ** Examples

# delete patterns from a string vector




cleanEx()
nameEx("del_ptr")
### * del_ptr

flush(stderr()); flush(stdout())

### Name: del_ptr
### Title: Delete external pointer
### Aliases: del_ptr

### ** Examples

# delete pointer




cleanEx()
nameEx("desub")
### * desub

flush(stderr()); flush(stdout())

### Name: desub
### Title: Desub
### Aliases: desub desubs

### ** Examples

# desub

# desubs




cleanEx()
nameEx("diff_cols")
### * diff_cols

flush(stderr()); flush(stdout())

### Name: diff_cols
### Title: Different columns
### Aliases: diff_cols

### ** Examples

# different columns




cleanEx()
nameEx("dolock")
### * dolock

flush(stderr()); flush(stdout())

### Name: dolock
### Title: dolock, unlock
### Aliases: dolock unlock

### ** Examples

## dolock
## Not run: dolock("your data", "your password")

## unlock
## Not run: unlock("your data", "your password")




cleanEx()
nameEx("draw_line")
### * draw_line

flush(stderr()); flush(stdout())

### Name: draw_line
### Title: Draw a line
### Aliases: draw_line

### ** Examples





cleanEx()
nameEx("equal")
### * equal

flush(stderr()); flush(stdout())

### Name: equal
### Title: Equal columns of two data frames.
### Aliases: equal

### ** Examples

# Are the columns of two data frames equal?




cleanEx()
nameEx("fill_one_before_first_one")
### * fill_one_before_first_one

flush(stderr()); flush(stdout())

### Name: fill_one_before_first_one
### Title: Fill with one before the first one
### Aliases: fill_one_before_first_one set_one_before_first_one

### ** Examples

# fill with one before the first one appears by rownames

# Set to one before the first one appears by rownames




cleanEx()
nameEx("fill_zero_not_first_pos")
### * fill_zero_not_first_pos

flush(stderr()); flush(stdout())

### Name: fill_zero_not_first_pos
### Title: Fill with or set to zero except for the first positive values
### Aliases: fill_zero_not_first_pos set_zero_not_first_pos

### ** Examples

# fill with zero except for the first positive values of a numerical matrix
# by rownames

# set to zero except for the first positive values of a numerical matrix by
# rownames




cleanEx()
nameEx("get_copied_dt")
### * get_copied_dt

flush(stderr()); flush(stdout())

### Name: get_copied_dt
### Title: Get a copied data.table
### Aliases: get_copied_dt

### ** Examples

# get copied data.table




cleanEx()
nameEx("get_pattern")
### * get_pattern

flush(stderr()); flush(stdout())

### Name: get_pattern
### Title: Get a first pattern
### Aliases: get_pattern

### ** Examples

# get a first pattern from a string vector




cleanEx()
nameEx("get_pattern_all")
### * get_pattern_all

flush(stderr()); flush(stdout())

### Name: get_pattern_all
### Title: Get all patterns
### Aliases: get_pattern_all

### ** Examples

# get all patterns from a string vector




cleanEx()
nameEx("get_prop")
### * get_prop

flush(stderr()); flush(stdout())

### Name: get_prop
### Title: Get proportion from a vector
### Aliases: get_prop

### ** Examples

## Not run: 
##D get_prop(sample(1:10, 1000, replace = TRUE))
## End(Not run)




cleanEx()
nameEx("get_ptr")
### * get_ptr

flush(stderr()); flush(stdout())

### Name: get_ptr
### Title: Get external pointer
### Aliases: get_ptr

### ** Examples

# get pointer
## Not run: 
##D df <- data.frame(x = c(1:3), y = c("a", "b", "c"))
##D get_ptr(df)
##D df <- setalloccol(df)
##D get_ptr(df)
## End(Not run)




cleanEx()
nameEx("get_stat_by")
### * get_stat_by

flush(stderr()); flush(stdout())

### Name: get_stat_by
### Title: Get statistic by groups
### Aliases: get_stat_by

### ** Examples

# get statistics by groups
## Not run: 
##D set_ptr(mtcars)
##D get_stat_by(mtcars, .(cyl, vs), value_var = .(hp, drat), fun = sum)
## End(Not run)




cleanEx()
nameEx("grepl_and")
### * grepl_and

flush(stderr()); flush(stdout())

### Name: grepl_and
### Title: grepl with 'And' conditions
### Aliases: grepl_and

### ** Examples

# grepl with "And" conditions




cleanEx()
nameEx("has_attr")
### * has_attr

flush(stderr()); flush(stdout())

### Name: has_attr
### Title: Has attributes
### Aliases: has_attr

### ** Examples

# has attributes




cleanEx()
nameEx("has_cols")
### * has_cols

flush(stderr()); flush(stdout())

### Name: has_cols
### Title: Has columns
### Aliases: has_cols

### ** Examples

# has columns

# raise an error
## Not run: 
##D has_cols(mtcars, c("cyl", "iris"), error_raise = TRUE)
## End(Not run)




cleanEx()
nameEx("has_len")
### * has_len

flush(stderr()); flush(stdout())

### Name: has_len
### Title: Has a length
### Aliases: has_len

### ** Examples

# has a length

# raise an error
## Not run: 
##D has_len(c(numeric(), character()), error_raise = TRUE)
## End(Not run)




cleanEx()
nameEx("has_ptr")
### * has_ptr

flush(stderr()); flush(stdout())

### Name: has_ptr
### Title: Has not a null pointer?
### Aliases: has_ptr

### ** Examples

# Has not null external pointer?




cleanEx()
nameEx("has_rows")
### * has_rows

flush(stderr()); flush(stdout())

### Name: has_rows
### Title: Has rows
### Aliases: has_rows

### ** Examples

# has rows
## Not run: 
##D df <- data.frame()
##D has_rows(df)
## End(Not run)

# raise an error
## Not run: 
##D df <- data.frame()
##D has_rows(df, error_raise = TRUE)
## End(Not run)




cleanEx()
nameEx("icol")
### * icol

flush(stderr()); flush(stdout())

### Name: icol
### Title: order numbers of columns
### Aliases: icol

### ** Examples

# order numbers of columns
icol(mtcars, c("disp", "drat", "qsec", "am", "carb"))




cleanEx()
nameEx("image_xlsx")
### * image_xlsx

flush(stderr()); flush(stdout())

### Name: image_xlsx
### Title: Insert images in an excel file
### Aliases: image_xlsx

### ** Examples

# insert images in an excel file
## Not run: 
##D image_xlsx(list(image1, image2), "image.xlsx")
## End(Not run)




cleanEx()
nameEx("is.null.externalptr")
### * is.null.externalptr

flush(stderr()); flush(stdout())

### Name: is.null.externalptr
### Title: Is a null external pointer?
### Aliases: is.null.externalptr

### ** Examples

# is null external pointer?
## Not run: 
##D p <- new("externalptr")
##D is.null.externalptr(p)
## End(Not run)




cleanEx()
nameEx("match_attr")
### * match_attr

flush(stderr()); flush(stdout())

### Name: match_attr
### Title: Matched attributes
### Aliases: match_attr

### ** Examples

# matched attributes




cleanEx()
nameEx("match_cols")
### * match_cols

flush(stderr()); flush(stdout())

### Name: match_cols
### Title: Match columns
### Aliases: match_cols

### ** Examples

# match columns




cleanEx()
nameEx("max_by_colnames")
### * max_by_colnames

flush(stderr()); flush(stdout())

### Name: max_by_colnames
### Title: max, min, sum by column names
### Aliases: max_by_colnames min_by_colnames sum_by_colnames

### ** Examples

# max by col nm

# min by col nm

# sum by col nm




cleanEx()
nameEx("max_by_dimnames")
### * max_by_dimnames

flush(stderr()); flush(stdout())

### Name: max_by_dimnames
### Title: max, min, sum by dim (row and column) names
### Aliases: max_by_dimnames min_by_dimnames sum_by_dimnames

### ** Examples

# max by dim names

# min by dim names

# sum by dim names




cleanEx()
nameEx("max_by_rownames")
### * max_by_rownames

flush(stderr()); flush(stdout())

### Name: max_by_rownames
### Title: max, min, sum by row names
### Aliases: max_by_rownames min_by_rownames sum_by_rownames

### ** Examples

# max by row nm

# min by row nm

# sum by row nm




cleanEx()
nameEx("meta")
### * meta

flush(stderr()); flush(stdout())

### Name: meta
### Title: Meta information
### Aliases: meta

### ** Examples





cleanEx()
nameEx("mondiff")
### * mondiff

flush(stderr()); flush(stdout())

### Name: mondiff
### Title: Month difference
### Aliases: mondiff

### ** Examples

# mondiff
sdate <- as.Date("1999-12-31")
edate <- as.Date("2000-01-01")
mondiff(sdate, edate)




cleanEx()
nameEx("mostfreq")
### * mostfreq

flush(stderr()); flush(stdout())

### Name: mostfreq
### Title: Most frequent value (mode, modal value)
### Aliases: mostfreq

### ** Examples

# get the most frequent values




cleanEx()
nameEx("paste_comma")
### * paste_comma

flush(stderr()); flush(stdout())

### Name: paste_comma
### Title: Paste comma
### Aliases: paste_comma

### ** Examples

# paste comma




cleanEx()
nameEx("paste_list")
### * paste_list

flush(stderr()); flush(stdout())

### Name: paste_list
### Title: Paste vectors of a list
### Aliases: paste_list

### ** Examples

# paste length and width of iris
iris$size <- paste_list(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
head(iris)




cleanEx()
nameEx("paste_sort_uni_str")
### * paste_sort_uni_str

flush(stderr()); flush(stdout())

### Name: paste_sort_uni_str
### Title: Paste sorted unique strings
### Aliases: paste_sort_uni_str

### ** Examples

# paste sorted unique string




cleanEx()
nameEx("paste_str")
### * paste_str

flush(stderr()); flush(stdout())

### Name: paste_str
### Title: Paste strings
### Aliases: paste_str

### ** Examples

# paste string




cleanEx()
nameEx("paste_uni_str")
### * paste_uni_str

flush(stderr()); flush(stdout())

### Name: paste_uni_str
### Title: Paste unique strings
### Aliases: paste_uni_str

### ** Examples

# paste unique string




cleanEx()
nameEx("plot_data_image")
### * plot_data_image

flush(stderr()); flush(stdout())

### Name: plot_data_image
### Title: Plot a data image
### Aliases: plot_data_image

### ** Examples

# plot a data image
## Not run: 
##D plot_data_image(head(data), height = .5)
## End(Not run)




cleanEx()
nameEx("plot_xlsx")
### * plot_xlsx

flush(stderr()); flush(stdout())

### Name: plot_xlsx
### Title: Draw plots in an excel file
### Aliases: plot_xlsx

### ** Examples

# draw ggplot objects in an excel file
## Not run: 
##D draw_xlsx(list(image1, image2), "image.xlsx")
## End(Not run)




cleanEx()
nameEx("quote_comma")
### * quote_comma

flush(stderr()); flush(stdout())

### Name: quote_comma
### Title: Quote and paste comma
### Aliases: quote_comma

### ** Examples

# quote comma




cleanEx()
nameEx("random_sampling")
### * random_sampling

flush(stderr()); flush(stdout())

### Name: random_sampling
### Title: Random sampling
### Aliases: random_sampling

### ** Examples

## Not run: 
##D random_sampling(iris)
## End(Not run)




cleanEx()
nameEx("rands")
### * rands

flush(stderr()); flush(stdout())

### Name: rands
### Title: Rands
### Aliases: rands

### ** Examples

# rands
rands(c(5, 5))




cleanEx()
nameEx("read_rds")
### * read_rds

flush(stderr()); flush(stdout())

### Name: read_rds
### Title: read_rds
### Aliases: read_rds

### ** Examples

# compare pointer values
## Not run: 
##D data <- copy(women)
##D data.table::setDT(data)
##D saveRDS(data, "data.rds")
##D df <- readRDS("data.rds")
##D dt <- read_rds("data.rds")
##D attributes(df)$.internal.selfref # <pointer: (nil)>
##D attributes(dt)$.internal.selfref
## End(Not run)




cleanEx()
nameEx("regex_attr")
### * regex_attr

flush(stderr()); flush(stdout())

### Name: regex_attr
### Title: Matched attributes using regular expressions
### Aliases: regex_attr

### ** Examples

# matched attributes using regular expressions




cleanEx()
nameEx("regex_cols")
### * regex_cols

flush(stderr()); flush(stdout())

### Name: regex_cols
### Title: Find columns using regular expression pattern
### Aliases: regex_cols

### ** Examples

# find columns using regular expression pattern




cleanEx()
nameEx("rep_row")
### * rep_row

flush(stderr()); flush(stdout())

### Name: rep_row
### Title: Data replication function
### Aliases: rep_row rep_row.matrix rep_row.data.frame rep_row.data.table

### ** Examples

rep_row(iris, times = 3)
rep_row(iris, each  = 3)




cleanEx()
nameEx("replace_a_with_b")
### * replace_a_with_b

flush(stderr()); flush(stdout())

### Name: replace_a_with_b
### Title: Replace A with B
### Aliases: replace_a_with_b

### ** Examples

## Not run: 
##D df <- data.table(x = c("A", "A", "C"), y = c("A", "C", "C"))
##D set_ptr(df)
##D data.table::address(df)
##D replace_a_with_b(df, a = "A", b = "B")
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("replace_empty_with_na")
### * replace_empty_with_na

flush(stderr()); flush(stdout())

### Name: replace_empty_with_na
### Title: Replace empty with NA
### Aliases: replace_empty_with_na

### ** Examples

## Not run: 
##D df <- data.table(x = c("A", "B", ""), y = c(1, NA, 3), z = c("", "E", ""))
##D set_ptr(df)
##D data.table::address(df)
##D replace_empty_with_na(df)
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("replace_na_with_empty")
### * replace_na_with_empty

flush(stderr()); flush(stdout())

### Name: replace_na_with_empty
### Title: Replace NA with empty
### Aliases: replace_na_with_empty

### ** Examples

## Not run: 
##D df <- data.table(x = c("A", "B", NA), y = c(1, NA, 3), z = c(NA, "E", NA))
##D set_ptr(df)
##D data.table::address(df)
##D replace_na_with_empty(df)
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("replace_na_with_zero")
### * replace_na_with_zero

flush(stderr()); flush(stdout())

### Name: replace_na_with_zero
### Title: Replace NA with zero
### Aliases: replace_na_with_zero

### ** Examples

## Not run: 
##D df <- data.table(x = c(1, NA, 3), y = c("A", "B", NA), z = c(NA, 5, NA))
##D set_ptr(df)
##D data.table::address(df)
##D replace_na_with_zero(df)
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("replace_zero_with_na")
### * replace_zero_with_na

flush(stderr()); flush(stdout())

### Name: replace_zero_with_na
### Title: Replace zero with NA
### Aliases: replace_zero_with_na

### ** Examples

## Not run: 
##D df <- data.table(x = c(1, 0, 3), y = c("A", "B", NA), z = c(0, 5, 0))
##D set_ptr(df)
##D data.table::address(df)
##D replace_zero_with_na(df)
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("reverse")
### * reverse

flush(stderr()); flush(stdout())

### Name: reverse
### Title: Reverse a vector
### Aliases: reverse

### ** Examples

# reverse a vector




cleanEx()
nameEx("rm_cols")
### * rm_cols

flush(stderr()); flush(stdout())

### Name: rm_cols
### Title: Remove columns
### Aliases: rm_cols

### ** Examples

# remove columns




cleanEx()
nameEx("rm_punct")
### * rm_punct

flush(stderr()); flush(stdout())

### Name: rm_punct
### Title: Remove punctuations
### Aliases: rm_punct

### ** Examples

# remove punctuations
## Not run: 
##D df <- data.frame(x = c("A3-", "$+_B", "C+_&"), y = c("123", "R&", "4q_++"))
##D set_ptr(df)
##D rm_punct(df)
##D df
## End(Not run)




cleanEx()
nameEx("rotate")
### * rotate

flush(stderr()); flush(stdout())

### Name: rotate
### Title: rotate a matrix
### Aliases: rotate

### ** Examples

# 90 degree rotation

# 180 degree rotation

# 270 degree rotation




cleanEx()
nameEx("row_max")
### * row_max

flush(stderr()); flush(stdout())

### Name: row_max
### Title: max, min, sum on each row of a matrix
### Aliases: row_max row_min row_sum

### ** Examples

# row max

# row min

# row sum




cleanEx()
nameEx("rowvec")
### * rowvec

flush(stderr()); flush(stdout())

### Name: rowvec
### Title: Change vector into row vector or column vector
### Aliases: rowvec colvec

### ** Examples

# change into column vector
rowvec(c(1, 2, 3, 4, 5))
colvec(c(1, 2, 3, 4, 5))




cleanEx()
nameEx("scaler")
### * scaler

flush(stderr()); flush(stdout())

### Name: scaler
### Title: Scaler
### Aliases: scaler

### ** Examples

## Not run: 
##D # create scaled data
##D set.seed(123)
##D x <- rnorm(100)
##D df <- rbind(
##D   data.frame(method = "Min-max" , x = seq_along(x), y = scaler(x, "minmax")),
##D   data.frame(method = "Robust"  , x = seq_along(x), y = scaler(x, "robust")),
##D   data.frame(method = "Standard", x = seq_along(x), y = scaler(x, "standard"))
##D )
##D g <- ggshort::ggline(df, x = x, y = y, group = method, color = method) +
##D   ggplot2::labs(title = "Scaling Method: Min-max vs Robust vs Standard") +
##D   ggshort::theme_view()
##D plotly::ggplotly(g)
## End(Not run)




cleanEx()
nameEx("set_attr")
### * set_attr

flush(stderr()); flush(stdout())

### Name: set_attr
### Title: Set attributes
### Aliases: set_attr

### ** Examples

# set attributes
## Not run: 
##D df <- data.frame(a = 1:3, b = 4:6)
##D set_attr(df, "flag", TRUE)
##D attr(df, "flag")
## End(Not run)




cleanEx()
nameEx("set_attr_class")
### * set_attr_class

flush(stderr()); flush(stdout())

### Name: set_attr_class
### Title: Set matched attributes' class
### Aliases: set_attr_class

### ** Examples

# set matched attributes' class




cleanEx()
nameEx("set_col_lower")
### * set_col_lower

flush(stderr()); flush(stdout())

### Name: set_col_lower
### Title: Change columns from uppercase to lowercase or from lowercase to
###   uppercase
### Aliases: set_col_lower set_col_upper

### ** Examples

# Change columns case




cleanEx()
nameEx("set_col_order")
### * set_col_order

flush(stderr()); flush(stdout())

### Name: set_col_order
### Title: Fast column reordering of data.table by reference
### Aliases: set_col_order

### ** Examples

## Not run: 
##D # set_col_order
##D df <- mtcars
##D set_col_order(df, .(gear, carb), after = mpg)
##D set_col_order(df, .(gear, carb), after = am)
## End(Not run)




cleanEx()
nameEx("set_dimnames")
### * set_dimnames

flush(stderr()); flush(stdout())

### Name: set_dimnames
### Title: Set dimension, row, column names
### Aliases: set_dimnames set_rownames set_colnames

### ** Examples

# set dimension names

# set row names

# set column names




cleanEx()
nameEx("set_labels")
### * set_labels

flush(stderr()); flush(stdout())

### Name: set_labels
### Title: Set labels
### Aliases: set_labels get_labels

### ** Examples

# set labels
## Not run: 
##D df <- data.frame(Q1 = c(0, 1, 1), Q2 = c(1, 0, 1))
##D set_labels(df, labels = c("Rainy?", "Umbrella?"))
##D View(df)
## End(Not run)




cleanEx()
nameEx("set_ptr")
### * set_ptr

flush(stderr()); flush(stdout())

### Name: set_ptr
### Title: Set external pointer
### Aliases: set_ptr

### ** Examples

# set pointer




cleanEx()
nameEx("set_stat_by")
### * set_stat_by

flush(stderr()); flush(stdout())

### Name: set_stat_by
### Title: Set statistic by groups
### Aliases: set_stat_by

### ** Examples

# set statistics by groups
## Not run: 
##D set_ptr(mtcars)
##D set_stat_by(mtcars, .(cyl, vs), value_var = hp, fun = cumsum)
##D mtcars[, c("cyl", "vs", "chp")]
## End(Not run)




cleanEx()
nameEx("setup_python_env")
### * setup_python_env

flush(stderr()); flush(stdout())

### Name: setup_python_env
### Title: Setup a Python or Miniconda Environment
### Aliases: setup_python_env

### ** Examples

# Create a virtual environment and install packages
## Not run: 
##D setup_python_env(
##D   env_name = "r-reticulate",
##D   packages = c("numpy", "pandas", "git+https://github.com/seokhoonj/underwriter"),
##D   use_miniconda = FALSE,
##D   force_reinstall = FALSE
##D )
## End(Not run)

# Create a Conda environment and force reinstall packages
## Not run: 
##D setup_python_env(
##D   env_name = "r-reticulate",
##D   packages = c("numpy", "pandas", "git+https://github.com/seokhoonj/underwriter"),
##D   use_miniconda = TRUE,
##D   force_reinstall = TRUE
##D )
## End(Not run)




cleanEx()
nameEx("sizeof")
### * sizeof

flush(stderr()); flush(stdout())

### Name: sizeof
### Title: Object size
### Aliases: sizeof

### ** Examples

## Not run: 
##D sizeof(x = sys.frame(), unit = "kb")
## End(Not run)



cleanEx()
nameEx("split_and_paste_uni_str")
### * split_and_paste_uni_str

flush(stderr()); flush(stdout())

### Name: split_and_paste_uni_str
### Title: Split and paste unique strings
### Aliases: split_and_paste_uni_str split_and_paste_sort_uni_str

### ** Examples

# split and paste unique strings




cleanEx()
nameEx("split_str")
### * split_str

flush(stderr()); flush(stdout())

### Name: split_str
### Title: Split strings
### Aliases: split_str

### ** Examples

# split strings




cleanEx()
nameEx("strati_sampling")
### * strati_sampling

flush(stderr()); flush(stdout())

### Name: strati_sampling
### Title: Stratified Sampling
### Aliases: strati_sampling

### ** Examples

## Not run: 
##D dt <- iris
##D data.table::setDT(dt)
##D strati_sampling(dt, size = 0.1)
##D strati_sampling(dt, group_var = Species, size = 0.1)
## End(Not run)




cleanEx()
nameEx("timeit")
### * timeit

flush(stderr()); flush(stdout())

### Name: timeit
### Title: Time it
### Aliases: timeit

### ** Examples

# time it




cleanEx()
nameEx("to_a1_col")
### * to_a1_col

flush(stderr()); flush(stdout())

### Name: to_a1_col
### Title: To a A1 format column
### Aliases: to_a1_col

### ** Examples

# convert a cell or column numeric from R1C1 to A1 format.




cleanEx()
nameEx("to_r1c1_col")
### * to_r1c1_col

flush(stderr()); flush(stdout())

### Name: to_r1c1_col
### Title: To a R1C1 format column
### Aliases: to_r1c1_col

### ** Examples

# convert a cell or column string from A1 to R1C1 format




cleanEx()
nameEx("traverse")
### * traverse

flush(stderr()); flush(stdout())

### Name: traverse
### Title: Traverse two vectors
### Aliases: traverse

### ** Examples

# traverse two vectors




cleanEx()
nameEx("trim_ws")
### * trim_ws

flush(stderr()); flush(stdout())

### Name: trim_ws
### Title: Trim white space
### Aliases: trim_ws

### ** Examples

## Not run: 
##D df <- data.frame(x = c(" A", "B ", " C "), y = c(1, 2, 3))
##D set_ptr(df)
##D data.table::address(df)
##D trim_ws(df)
##D data.table::address(df)
##D df
## End(Not run)




cleanEx()
nameEx("type")
### * type

flush(stderr()); flush(stdout())

### Name: type
### Title: Class and type information
### Aliases: type

### ** Examples





cleanEx()
nameEx("unilen")
### * unilen

flush(stderr()); flush(stdout())

### Name: unilen
### Title: Length of a unique vector
### Aliases: unilen

### ** Examples

# length of unique vector




cleanEx()
nameEx("valid_cols")
### * valid_cols

flush(stderr()); flush(stdout())

### Name: valid_cols
### Title: Validate columns
### Aliases: valid_cols

### ** Examples

# different columns
## Not run: valid_cols(mtcars, c("mpg", "cyl", "disp", "hp", "drat"))




cleanEx()
nameEx("view_data_image")
### * view_data_image

flush(stderr()); flush(stdout())

### Name: view_data_image
### Title: View a html data image
### Aliases: view_data_image save_data_image

### ** Examples

# plot a data image
## Not run: 
##D plot_data_image(head(data), height = .5)
## End(Not run)




cleanEx()
nameEx("yearmon")
### * yearmon

flush(stderr()); flush(stdout())

### Name: yearmon
### Title: Year month
### Aliases: yearmon

### ** Examples

# get year month
x <- as.Date(c("1999-12-31", "2000-01-01"))
yearmon(x)




cleanEx()
nameEx("zeros")
### * zeros

flush(stderr()); flush(stdout())

### Name: zeros
### Title: Zeros
### Aliases: zeros

### ** Examples

# zeros
zeros(c(5, 5))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
