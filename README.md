# jaid

Joo's aid toolkit for the efficient programming.

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/jaid)](https://CRAN.R-project.org/package=jaid) [![R-CMD-check](https://github.com/seokhoonj/jaid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/jaid/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Installation

You can install the development version of jaid from [GitHub](https://github.com/seokhoonj/jaid) with:

``` r
# install.packages("devtools")
devtools::install_github("seokhoonj/jaid")
```

## Vector

```r
library(jaid)

x <- c(1, 1, 2, 3, 4, 5, 5)
unilen(x)
#> [1] 5

x <- 1:10
reverse(x)
#> [1] 10  9  8  7  6  5  4  3  2  1

x <- c(1, 3, 5, 7)
y <- c(2, 4, 6, 8)
traverse(x, y)
#> [1] 1 2 3 4 5 6 7 8

```

## Matrix

``` r
library(jaid)

# create matrix
x <- matrix(c(1:9), nrow = 3)

# set row names
set_row_nm(x, c("a", "a", "b"))
set_row_nm(x, NULL)

# set column names
set_row_nm(x, c("a", "b", "b"))
set_row_nm(x, NULL)

# set dimension names
set_dim_nm(x, list(c("a", "a", "b"), c("a", "b", "b")))
#>   a b b
#> a 1 4 7
#> a 2 5 8
#> b 3 6 9

# row max, min, sum
row_max(x)
#> [1] 7 8 9
row_min(x)
#> [1] 1 2 3
row_sum(x)
#> [1] 12 15 18

# stats by row names
max_by_row_nm(x)
#>   a b b
#> a 2 5 8
#> b 3 6 9
min_by_row_nm(x)
#>   a b b
#> a 1 4 7
#> b 3 6 9
sum_by_row_nm(x)
#>   a b  b
#> a 3 9 15
#> b 3 6  9

# stats by column names
max_by_col_nm(x)
#>   a b
#> a 1 7
#> a 2 8
#> b 3 9
min_by_col_nm(x)
#>   a b
#> a 1 4
#> a 2 5
#> b 3 6
sum_by_col_nm(x)
#>   a  b
#> a 1 11
#> a 2 13
#> b 3 15

# stats by dimension (row and column) names
max_by_dim_nm(x)
#>   a b
#> a 2 8
#> b 3 9
min_by_dim_nm(x)
#>   a b
#> a 1 4
#> b 3 6
sum_by_dim_nm(x)
#>   a  b
#> a 3 24
#> b 3 15

```
