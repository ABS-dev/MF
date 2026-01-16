# MFClusBootHier

Combines [MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md)
and [MFnestBoot](https://abs-dev.github.io/MF/reference/MFnestBoot.md)
into a single function.

## Usage

``` r
MFClusBootHier(
  formula,
  data,
  compare = c("con", "vac"),
  nboot = 10000,
  boot.unit = TRUE,
  boot.cluster = TRUE,
  which.factor = "All",
  alpha = 0.05,
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

- which.factor:

  Which variables to include in the mitigated fraction summation.
  Default is "All", to sum over entire tree.

- alpha:

  Passed to `emp_hpd` to calculate high tailed upper and high tailed
  lower of mitigated fraction.

- seed:

  Passed to [MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md)
  to to initialize random number generator for reproducibility.

## Value

A list with the following elements:

- `MFhBoot`: as output from
  [MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md).

- `MFnestBoot`: as output from
  [MFnestBoot](https://abs-dev.github.io/MF/reference/MFnestBoot.md).

## Note

`Core` variable is the variable corresponding to the lowest nodes of the
hierarchical tree. `Nest` variables are those above the core. `All`
refers to a summary of the entire tree.

## See also

[MFhBoot](https://abs-dev.github.io/MF/reference/MFhBoot.md),
[MFnestBoot](https://abs-dev.github.io/MF/reference/MFnestBoot.md).

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
thismf1 <- MFClusBootHier(lung ~ tx + room / pen / litter, a, nboot = 10000,
                       boot.cluster = TRUE, boot.unit = TRUE, seed = 12345)
thismf1
#> # A tibble: 1 × 8
#>   variable level median etlower etupper hdlower hdupper mf.obs
#>   <fct>    <chr>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
#> 1 All      All    0.917   0.625       1   0.667       1  0.875
```
