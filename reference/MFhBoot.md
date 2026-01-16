# MFhBoot

Calculate rank tables for MF using bootstrapping.

## Usage

``` r
MFhBoot(
  formula,
  data,
  compare = c("con", "vac"),
  nboot = 10000,
  boot.unit = TRUE,
  boot.cluster = TRUE,
  seed = sample(1:1e+05, 1)
)
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

- nboot:

  number of bootstrapping events

- boot.unit:

  Boolean whether to sample observations from within those of the same
  core.

- boot.cluster:

  Boolean whether to sample which cores are present. If TRUE, some trees
  have all the cores while others only have a subset.

- seed:

  to initialize random number generator for reproducibility. Passed to
  `set.seed`.

## Value

A list with the following elements:

- `bootmfh`: Rank table for the bootstrapped values as output from
  [MFh](https://abs-dev.github.io/MF/reference/MFh.md). Includes a new
  `bootID` variable to distinguish each bootstrapped incidence.

- `clusters`: Table of unique nodes with an ID.

- `compare`: Compare vector as specified by user.

- `mfh`: MFh run on original data input.

## See also

[MFClusBootHier](https://abs-dev.github.io/MF/reference/MFClusBootHier.md),
[MFnestBoot](https://abs-dev.github.io/MF/reference/MFnestBoot.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
set.seed(76153)
a <- data.frame(room = paste("Room", rep(c("W", "Z"), each = 24)),
                pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
                litter = paste("Litter", rep(11:22, each = 4)),
                tx = rep(rep(c("vac", "con"), each = 2), 12))
a[a$tx == "vac", "lung"] <-  rnorm(24, 5, 1.3)
a[a$tx == "con", "lung"] <- rnorm(24, 7, 1.3)
a
#>      room   pen    litter  tx     lung
#> 1  Room W Pen A Litter 11 vac 5.633023
#> 2  Room W Pen A Litter 11 vac 4.618143
#> 3  Room W Pen A Litter 11 con 9.196909
#> 4  Room W Pen A Litter 11 con 7.277479
#> 5  Room W Pen A Litter 12 vac 3.760431
#> 6  Room W Pen A Litter 12 vac 3.856939
#> 7  Room W Pen A Litter 12 con 6.307358
#> 8  Room W Pen A Litter 12 con 3.521230
#> 9  Room W Pen B Litter 13 vac 4.356031
#> 10 Room W Pen B Litter 13 vac 6.098324
#> 11 Room W Pen B Litter 13 con 7.866934
#> 12 Room W Pen B Litter 13 con 8.339948
#> 13 Room W Pen B Litter 14 vac 5.389661
#> 14 Room W Pen B Litter 14 vac 5.799638
#> 15 Room W Pen B Litter 14 con 8.275256
#> 16 Room W Pen B Litter 14 con 7.952039
#> 17 Room W Pen C Litter 15 vac 4.855498
#> 18 Room W Pen C Litter 15 vac 5.657725
#> 19 Room W Pen C Litter 15 con 7.658824
#> 20 Room W Pen C Litter 15 con 8.517004
#> 21 Room W Pen C Litter 16 vac 4.169034
#> 22 Room W Pen C Litter 16 vac 4.837650
#> 23 Room W Pen C Litter 16 con 5.822156
#> 24 Room W Pen C Litter 16 con 7.718757
#> 25 Room Z Pen D Litter 17 vac 3.595465
#> 26 Room Z Pen D Litter 17 vac 4.921762
#> 27 Room Z Pen D Litter 17 con 5.222559
#> 28 Room Z Pen D Litter 17 con 5.928738
#> 29 Room Z Pen D Litter 18 vac 7.622600
#> 30 Room Z Pen D Litter 18 vac 5.036121
#> 31 Room Z Pen D Litter 18 con 8.620428
#> 32 Room Z Pen D Litter 18 con 6.265424
#> 33 Room Z Pen E Litter 19 vac 3.787388
#> 34 Room Z Pen E Litter 19 vac 5.380694
#> 35 Room Z Pen E Litter 19 con 9.454640
#> 36 Room Z Pen E Litter 19 con 6.505600
#> 37 Room Z Pen E Litter 20 vac 5.300123
#> 38 Room Z Pen E Litter 20 vac 4.417709
#> 39 Room Z Pen E Litter 20 con 7.211453
#> 40 Room Z Pen E Litter 20 con 6.351508
#> 41 Room Z Pen F Litter 21 vac 4.690320
#> 42 Room Z Pen F Litter 21 vac 6.035818
#> 43 Room Z Pen F Litter 21 con 6.643916
#> 44 Room Z Pen F Litter 21 con 6.995050
#> 45 Room Z Pen F Litter 22 vac 4.896764
#> 46 Room Z Pen F Litter 22 vac 5.371713
#> 47 Room Z Pen F Litter 22 con 6.773614
#> 48 Room Z Pen F Litter 22 con 7.772144

formula <- lung ~ tx + room / pen / litter
nboot <- 10000
boot.cluster <- TRUE
boot.unit <- TRUE
which.factors <- c("All", "room", "pen", "litter")

system.time(test1 <- MFhBoot(formula, a,
                            nboot = 10000,
                             boot.cluster = TRUE,
                             boot.unit = TRUE,
                             seed = 12345))
#>    user  system elapsed 
#>   1.891   0.009   1.900 
test1$bootmfh
#> # A tibble: 120,000 × 11
#>    bootID     w     u  n1n2 con_n vac_n con_medResp vac_medResp room   pen  
#>     <int> <dbl> <dbl> <int> <int> <int>       <dbl>       <dbl> <chr>  <chr>
#>  1      1     7     4     4     2     2        8.10        5.23 Room W Pen B
#>  2      1     7     4     4     2     2        6.82        5.36 Room Z Pen E
#>  3      1     7     4     4     2     2        6.77        4.50 Room Z Pen F
#>  4      1     7     4     4     2     2        8.11        5.59 Room Z Pen D
#>  5      1     6     3     4     2     2        7.44        6.33 Room Z Pen E
#>  6      1     7     4     4     2     2        7.44        6.33 Room Z Pen F
#>  7      1     7     4     4     2     2        5.58        4.26 Room Z Pen D
#>  8      1     7     4     4     2     2        7.98        4.58 Room W Pen A
#>  9      1     7     4     4     2     2        5.58        4.26 Room W Pen C
#> 10      1     7     4     4     2     2        6.82        5.36 Room Z Pen F
#> # ℹ 119,990 more rows
#> # ℹ 1 more variable: litter <chr>
```
