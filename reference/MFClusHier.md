# MFClusHier

Calculate mitigated fraction directly from hierarchial nested data.
Combines [MFh](https://abs-dev.github.io/MF/reference/MFh.md) and
[MFnest](https://abs-dev.github.io/MF/reference/MFnest.md) into a single
function.

## Usage

``` r
MFClusHier(formula, data, compare = c("con", "vac"), which.factor = "All")
```

## Arguments

- formula:

  Formula of the form y ~ x + a/b/c, where y is a continuous response, x
  is a factor with two levels of treatment, and a/b/c are grouping
  variables corresponding to the clusters. Nesting is assumed to be in
  order, left to right, highest to lowest. So a single level of "a" will
  contain multiple levels of "b" and a single level of "b" will contain
  multiple levels of "c".

- data:

  a data.frame or tibble with the variables specified in formula.
  Additional variables will be ignored.

- compare:

  Text vector stating the factor levels - `compare[1]` is the control or
  reference group to which `compare[2]` is compared.

- which.factor:

  one or more variable(s) of interest. This can be any of the core or
  nest variables from the data set. If none or NULL is specified, MF
  will be calculated for the whole tree.

## Value

A list with the following elements:

- `MFh`: as output from
  [MFh](https://abs-dev.github.io/MF/reference/MFh.md).

- `MFnest`: as output from
  [MFnest](https://abs-dev.github.io/MF/reference/MFnest.md).

## Note

`Core` variable is the variable corresponding to the lowest nodes of the
hierarchical tree. `Nest` variables are those above the core. `All`
refers to a summary of the entire tree.

## See also

[MFh](https://abs-dev.github.io/MF/reference/MFh.md),
[MFnest](https://abs-dev.github.io/MF/reference/MFnest.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
a <- data.frame(
room = paste("Room", rep(c("W", "Z"), each = 24)),
pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
litter = paste("Litter", rep(11:22, each = 4)),
tx = rep(rep(c("vac", "con"), each = 2), 12))
set.seed(76153)
a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
thismf <- MFClusHier(lung ~ tx + room / pen / litter, a)
thismf$MFnest
#> # A tibble: 1 × 9
#>   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#> 1 All      All   0.875    48    45    24    24        7.24        4.91
aCore <- thismf$MFh
aCore
#> # A tibble: 12 × 10
#>    room   pen   litter    con_medResp con_n     w vac_medResp vac_n  n1n2     u
#>    <chr>  <chr> <chr>           <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl> <dbl>
#>  1 Room W Pen A Litter 11        8.24     2     7        5.13     2     4     4
#>  2 Room W Pen A Litter 12        4.91     2     5        3.81     2     4     2
#>  3 Room W Pen B Litter 13        8.10     2     7        5.23     2     4     4
#>  4 Room W Pen B Litter 14        8.11     2     7        5.59     2     4     4
#>  5 Room W Pen C Litter 15        8.09     2     7        5.26     2     4     4
#>  6 Room W Pen C Litter 16        6.77     2     7        4.50     2     4     4
#>  7 Room Z Pen D Litter 17        5.58     2     7        4.26     2     4     4
#>  8 Room Z Pen D Litter 18        7.44     2     6        6.33     2     4     3
#>  9 Room Z Pen E Litter 19        7.98     2     7        4.58     2     4     4
#> 10 Room Z Pen E Litter 20        6.78     2     7        4.86     2     4     4
#> 11 Room Z Pen F Litter 21        6.82     2     7        5.36     2     4     4
#> 12 Room Z Pen F Litter 22        7.27     2     7        5.13     2     4     4
aCore$data
#> # A tibble: 48 × 5
#>    room   pen   litter    tgroup  resp
#>    <chr>  <chr> <chr>     <chr>  <dbl>
#>  1 Room W Pen A Litter 11 vac     5.63
#>  2 Room W Pen A Litter 11 vac     4.62
#>  3 Room W Pen A Litter 11 con     9.20
#>  4 Room W Pen A Litter 11 con     7.28
#>  5 Room W Pen A Litter 12 vac     3.76
#>  6 Room W Pen A Litter 12 vac     3.86
#>  7 Room W Pen A Litter 12 con     6.31
#>  8 Room W Pen A Litter 12 con     3.52
#>  9 Room W Pen B Litter 13 vac     4.36
#> 10 Room W Pen B Litter 13 vac     6.10
#> # ℹ 38 more rows
aCore$formula
#> lung ~ tx + room/pen/litter
#> <environment: 0x56520add5b70>
aCore$compare
#> [1] "con" "vac"
```
