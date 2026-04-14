# MFnestBoot

MFnest using bootstrapping

## Usage

``` r
MFnestBoot(x, which.factor = "All", alpha = 0.05)
```

## Arguments

- x:

  output from
  [MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md)

- which.factor:

  one or more grouping variable(s) of interest. This can be any of the
  core or nest variables from the data set. A MF value will be
  calculated for each level of the variable(s) specified. Default is
  "All", to sum over entire tree.

- alpha:

  Passed to `emp_hpd` to calculate eq tailed upper and high lower of
  mitigated fraction

## Value

A list with the following elements:

- `mfnest_details`: The MF and summary statistics as calculated for each
  bootstrap event. Variables as in
  [MFnest](https://abs-dev.github.io/MF/reference/MFnest.md) output.

- `mfnest_summary`: Statistical summary of bootstrapped MF with each
  unique level of a core or nest variable passed to `which.factor` as a
  row. Other variables include:

  - `median`: Median of MFs from all of the bootstrap events.

  - `etlower`: Lower value of equal tailed range.

  - `etupper`: Upper value of equal tailed range.

  - `hdlower`: Lower value of the highest posterior density range.

  - `hdupper`: Upper value of the highest posterior density range.

  - `mf.obs`: MF calculated from data using
    [MFh](https://abs-dev.github.io/MF/reference/MFh.md).

## See also

[MFClusBootHier](https://abs-dev.github.io/MF/reference/MFClusBootHier.md),
[MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
set.seed(76153)
a <- data.frame(room = paste("Room", rep(c("W", "Z"), each = 24)),
                pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
                litter = paste("Litter", rep(11:22, each = 4)),
                tx = rep(rep(c("vac", "con"), each = 2), 12))
a[a$tx == "vac", "lung"] <- rnorm(24, 5, 1.3)
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

#################

test1 <- MFhBoot(formula, a,
                 nboot = 10000,
                 boot.cluster = TRUE, boot.unit = TRUE, seed = 12345)
MFnestBoot(test1, c("All", "litter"))
#> Complete separation observed for variable(s): litter
#> $mfnest_details
#> # A tibble: 87,760 × 8
#> # Groups:   variable, level [13]
#>    variable level bootID     U  N1N2 con_N vac_N    MF
#>    <chr>    <chr>  <int> <dbl> <int> <int> <int> <dbl>
#>  1 All      All        1    47    48    24    24 0.958
#>  2 All      All        2    40    48    24    24 0.667
#>  3 All      All        3    44    48    24    24 0.833
#>  4 All      All        4    46    48    24    24 0.917
#>  5 All      All        5    44    48    24    24 0.833
#>  6 All      All        6    44    48    24    24 0.833
#>  7 All      All        7    46    48    24    24 0.917
#>  8 All      All        8    44    48    24    24 0.833
#>  9 All      All        9    48    48    24    24 1    
#> 10 All      All       10    48    48    24    24 1    
#> # ℹ 87,750 more rows
#> 
#> $mfnest_summary
#> # A tibble: 13 × 8
#>    variable level     median etlower etupper hdlower hdupper mf.obs
#>    <fct>    <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
#>  1 All      All        0.917   0.625       1   0.667       1  0.875
#>  2 litter   Litter 11  1       0           1   0           1  1    
#>  3 litter   Litter 12  1       0           1   0           1  0    
#>  4 litter   Litter 13  1       0           1   0           1  1    
#>  5 litter   Litter 14  1       0           1   0           1  1    
#>  6 litter   Litter 15  1       0           1   0           1  1    
#>  7 litter   Litter 16  1       0           1   0           1  1    
#>  8 litter   Litter 17  1       0           1   0           1  1    
#>  9 litter   Litter 18  1       0           1   0           1  0.5  
#> 10 litter   Litter 19  1       0           1   0           1  1    
#> 11 litter   Litter 20  1       0           1   0           1  1    
#> 12 litter   Litter 21  1       0           1   0           1  1    
#> 13 litter   Litter 22  1       0           1   0           1  1    
#> 
#> $seed
#> [1] 12345
#> 

if (FALSE) { # \dontrun{
system.time(test2 <- MFnestBoot(test1, which.factors))
test2
system.time(test3 <- MFnestBoot(test1, which.factors[1]))
test3
system.time(test4 <- MFnestBoot(test1, which.factors[2]))
test4
system.time(test5 <- MFnestBoot(test1, which.factors[2:3]))
test5
system.time(test6 <- MFnestBoot(test1, which.factors[2:4]))
test6
} # }
```
