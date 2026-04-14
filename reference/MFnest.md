# Summations to calculate the MF for nested data from a rank table.

Summations to calculate the MF for nested data from a rank table.

## Usage

``` r
MFnest(Y, which.factor = "All")
```

## Arguments

- Y:

  rank table (tibble or data.frame), structured as `$coreTbl` output
  from [MFh](https://abs-dev.github.io/MF/reference/MFh.md) or returned
  object from [MFh](https://abs-dev.github.io/MF/reference/MFh).

- which.factor:

  one or more grouping variable(s) of interest. This can be any of the
  core or nest variables from the data set. If none or `All` is
  specified, a summary MF will be calculated for the whole tree.

## Value

A tibble with each unique level of a variable as a row. Other values
include:

- `MF`: Mitigated fraction for the particular level of the variable in
  this row.

- `N1N2`: Sum of the `n1n2` variable in `$coreTbl` field of
  [mfhierdata](https://abs-dev.github.io/MF/reference/mfhierdata-class.md)
  object output by [MFh](https://abs-dev.github.io/MF/reference/MFh.md)
  for this particular variable-level combination.

- `U`: Sum of u variable in `$coreTbl` field of
  [mfhierdata](https://abs-dev.github.io/MF/reference/mfhierdata-class.md)
  object output by [MFh](https://abs-dev.github.io/MF/reference/MFh.md)
  for this particular variable-level combination.

- `_N`: Sum of the `_n` variable in `$coreTbl` field of
  [mfhierdata](https://abs-dev.github.io/MF/reference/mfhierdata-class.md)
  object output by [MFh](https://abs-dev.github.io/MF/reference/MFh.md)
  for this particular variable-level combination.

- `_medResp`: Median of observed responses for each comparison group for
  this particular variable-level combination.

## Note

Core variable is the variable corresponding to the lowest nodes of the
hierarchial tree. Nest variables are those above the core. All refers to
a summary of the entire tree.

## See also

[MFh](https://abs-dev.github.io/MF/reference/MFh.md)

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

aCore <- MFh(lung ~ tx + room / pen / litter, a)
MFnest(aCore)
#> # A tibble: 1 × 9
#>   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#> 1 All      All   0.875    48    45    24    24        7.24        4.91
# # A tibble: 1 x 9
#   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
# 1 All      All   0.875    48    45    24    24        7.24        4.91

MFnest(aCore$coreTbl)
#> Skipping median summary, no response data provided.
#> # A tibble: 1 × 7
#>   variable level    MF  N1N2     U con_N vac_N
#>   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 All      All   0.875    48    45    24    24
# Skipping median summary, no response data provided.
# # A tibble: 1 x 7
#   variable level    MF  N1N2     U con_N vac_N
#   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 All      All   0.875    48    45    24    24

MFnest(aCore, "room")
#> # A tibble: 2 × 9
#>   variable level     MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>   <fct>    <chr>  <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#> 1 room     Room W 0.833    24    22    12    12        7.79        4.85
#> 2 room     Room Z 0.917    24    23    12    12        6.71        4.98
# # A tibble: 2 x 9
#   variable level     MF  N1N2     U con_N vac_N con_medResp vac_medResp
#   <fct>    <chr>  <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
# 1 room     Room W 0.833    24    22    12    12        7.79        4.85
# 2 room     Room Z 0.917    24    23    12    12        6.71        4.98

MFnest(aCore, "pen")
#> Complete separation observed for variable(s): pen
#> # A tibble: 6 × 9
#>   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#> 1 pen      Pen A  0.5      8     6     4     4        6.79        4.24
#> 2 pen      Pen B  1        8     8     4     4        8.11        5.59
#> 3 pen      Pen C  1        8     8     4     4        7.69        4.85
#> 4 pen      Pen D  0.75     8     7     4     4        6.10        4.98
#> 5 pen      Pen E  1        8     8     4     4        6.86        4.86
#> 6 pen      Pen F  1        8     8     4     4        6.88        5.13
# Complete separation observed for variable(s): pen
# # A tibble: 6 x 9
#   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
# 1 pen      Pen A  0.5      8     6     4     4        6.79        4.24
# 2 pen      Pen B  1        8     8     4     4        8.11        5.59
# 3 pen      Pen C  1        8     8     4     4        7.69        4.85
# 4 pen      Pen D  0.75     8     7     4     4        6.10        4.98
# 5 pen      Pen E  1        8     8     4     4        6.86        4.86
# 6 pen      Pen F  1        8     8     4     4        6.88        5.13

MFnest(aCore, c("All", "litter"))
#> Complete separation observed for variable(s): litter
#> # A tibble: 13 × 9
#>    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#>  1 All      All       0.875    48    45    24    24        7.24        4.91
#>  2 litter   Litter 11 1         4     4     2     2        8.24        5.13
#>  3 litter   Litter 12 0         4     2     2     2        4.91        3.81
#>  4 litter   Litter 13 1         4     4     2     2        8.10        5.23
#>  5 litter   Litter 14 1         4     4     2     2        8.11        5.59
#>  6 litter   Litter 15 1         4     4     2     2        8.09        5.26
#>  7 litter   Litter 16 1         4     4     2     2        6.77        4.50
#>  8 litter   Litter 17 1         4     4     2     2        5.58        4.26
#>  9 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
#> 10 litter   Litter 19 1         4     4     2     2        7.98        4.58
#> 11 litter   Litter 20 1         4     4     2     2        6.78        4.86
#> 12 litter   Litter 21 1         4     4     2     2        6.82        5.36
#> 13 litter   Litter 22 1         4     4     2     2        7.27        5.13
# Complete separation observed for variable(s): litter
# # A tibble: 13 x 9
#    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#  1 All      All       0.875    48    45    24    24        7.24        4.91
#  2 litter   Litter 11 1         4     4     2     2        8.24        5.13
#  3 litter   Litter 12 0         4     2     2     2        4.91        3.81
#  4 litter   Litter 13 1         4     4     2     2        8.10        5.23
#  5 litter   Litter 14 1         4     4     2     2        8.11        5.59
#  6 litter   Litter 15 1         4     4     2     2        8.09        5.26
#  7 litter   Litter 16 1         4     4     2     2        6.77        4.50
#  8 litter   Litter 17 1         4     4     2     2        5.58        4.26
#  9 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
# 10 litter   Litter 19 1         4     4     2     2        7.98        4.58
# 11 litter   Litter 20 1         4     4     2     2        6.78        4.86
# 12 litter   Litter 21 1         4     4     2     2        6.82        5.36
# 13 litter   Litter 22 1         4     4     2     2        7.27        5.13

MFnest(aCore, "litter")
#> Complete separation observed for variable(s): litter
#> # A tibble: 12 × 9
#>    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#>  1 litter   Litter 11   1       4     4     2     2        8.24        5.13
#>  2 litter   Litter 12   0       4     2     2     2        4.91        3.81
#>  3 litter   Litter 13   1       4     4     2     2        8.10        5.23
#>  4 litter   Litter 14   1       4     4     2     2        8.11        5.59
#>  5 litter   Litter 15   1       4     4     2     2        8.09        5.26
#>  6 litter   Litter 16   1       4     4     2     2        6.77        4.50
#>  7 litter   Litter 17   1       4     4     2     2        5.58        4.26
#>  8 litter   Litter 18   0.5     4     3     2     2        7.44        6.33
#>  9 litter   Litter 19   1       4     4     2     2        7.98        4.58
#> 10 litter   Litter 20   1       4     4     2     2        6.78        4.86
#> 11 litter   Litter 21   1       4     4     2     2        6.82        5.36
#> 12 litter   Litter 22   1       4     4     2     2        7.27        5.13
# Complete separation observed for variable(s): litter
# # A tibble: 12 x 9
#    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#  1 litter   Litter 11   1       4     4     2     2        8.24        5.13
#  2 litter   Litter 12   0       4     2     2     2        4.91        3.81
#  3 litter   Litter 13   1       4     4     2     2        8.10        5.23
#  4 litter   Litter 14   1       4     4     2     2        8.11        5.59
#  5 litter   Litter 15   1       4     4     2     2        8.09        5.26
#  6 litter   Litter 16   1       4     4     2     2        6.77        4.50
#  7 litter   Litter 17   1       4     4     2     2        5.58        4.26
#  8 litter   Litter 18   0.5     4     3     2     2        7.44        6.33
#  9 litter   Litter 19   1       4     4     2     2        7.98        4.58
# 10 litter   Litter 20   1       4     4     2     2        6.78        4.86
# 11 litter   Litter 21   1       4     4     2     2        6.82        5.36
# 12 litter   Litter 22   1       4     4     2     2        7.27        5.13

MFnest(aCore, c("room", "pen", "litter"))
#> Complete separation observed for variable(s): litter, pen
#> # A tibble: 20 × 9
#>    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#>    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#>  1 room     Room W    0.833    24    22    12    12        7.79        4.85
#>  2 room     Room Z    0.917    24    23    12    12        6.71        4.98
#>  3 pen      Pen A     0.5       8     6     4     4        6.79        4.24
#>  4 pen      Pen B     1         8     8     4     4        8.11        5.59
#>  5 pen      Pen C     1         8     8     4     4        7.69        4.85
#>  6 pen      Pen D     0.75      8     7     4     4        6.10        4.98
#>  7 pen      Pen E     1         8     8     4     4        6.86        4.86
#>  8 pen      Pen F     1         8     8     4     4        6.88        5.13
#>  9 litter   Litter 11 1         4     4     2     2        8.24        5.13
#> 10 litter   Litter 12 0         4     2     2     2        4.91        3.81
#> 11 litter   Litter 13 1         4     4     2     2        8.10        5.23
#> 12 litter   Litter 14 1         4     4     2     2        8.11        5.59
#> 13 litter   Litter 15 1         4     4     2     2        8.09        5.26
#> 14 litter   Litter 16 1         4     4     2     2        6.77        4.50
#> 15 litter   Litter 17 1         4     4     2     2        5.58        4.26
#> 16 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
#> 17 litter   Litter 19 1         4     4     2     2        7.98        4.58
#> 18 litter   Litter 20 1         4     4     2     2        6.78        4.86
#> 19 litter   Litter 21 1         4     4     2     2        6.82        5.36
#> 20 litter   Litter 22 1         4     4     2     2        7.27        5.13
# # A tibble: 20 x 9
#    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#  1 room     Room W    0.833    24    22    12    12        7.79        4.85
#  2 room     Room Z    0.917    24    23    12    12        6.71        4.98
#  3 pen      Pen A     0.5       8     6     4     4        6.79        4.24
#  4 pen      Pen B     1         8     8     4     4        8.11        5.59
#  5 pen      Pen C     1         8     8     4     4        7.69        4.85
#  6 pen      Pen D     0.75      8     7     4     4        6.10        4.98
#  7 pen      Pen E     1         8     8     4     4        6.86        4.86
#  8 pen      Pen F     1         8     8     4     4        6.88        5.13
#  9 litter   Litter 11 1         4     4     2     2        8.24        5.13
# 10 litter   Litter 12 0         4     2     2     2        4.91        3.81
# 11 litter   Litter 13 1         4     4     2     2        8.10        5.23
# 12 litter   Litter 14 1         4     4     2     2        8.11        5.59
# 13 litter   Litter 15 1         4     4     2     2        8.09        5.26
# 14 litter   Litter 16 1         4     4     2     2        6.77        4.50
# 15 litter   Litter 17 1         4     4     2     2        5.58        4.26
# 16 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
# 17 litter   Litter 19 1         4     4     2     2        7.98        4.58
# 18 litter   Litter 20 1         4     4     2     2        6.78        4.86
# 19 litter   Litter 21 1         4     4     2     2        6.82        5.36
# 20 litter   Litter 22 1         4     4     2     2        7.27        5.13
```
