# Subject components of mitigated fraction

Estimates the subject components of the mitigated fraction.

## Usage

``` r
MFSubj(formula, data, compare = c("con", "vac"))
```

## Arguments

- formula:

  Formula of the form `y ~ x`, where y is a continuous response and x is
  a factor with two levels

- data:

  Data frame

- compare:

  Text vector stating the factor levels - `compare[1]` is the control or
  reference group to which `compare[2]` is compared

## Value

a
[mfcomponents](https://abs-dev.github.io/MF/reference/mfcomponents-class.md)
data object

## Details

The mitigated fraction is an estimator that quantifies an intervention's
effect on reducing the severity of a condition. Since its units are on
the probability scale, it is often a good idea to accompany it with an
estimator on the original scale of measurement.

The subject components are the individual contributions of the treated
subjects to *MF*, which is the average of the subject components.

## References

Siev D. (2005). An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* **4:500–508**

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
x <- MFSubj(lesion ~ group, calflung)
x
#> 
#> MF = 0.44 comparing vac to con 
#> 
#> MF Subject Components
#> 
#>   mf_j freq    min_y   max_y
#>   1.00    6 0.000030 0.00970
#>   0.84    1 0.012500 0.01250
#>   0.76    3 0.016650 0.02030
#>   0.68    6 0.023250 0.03190
#>   0.04    1 0.132100 0.13210
#>  -0.04    3 0.144575 0.16325
#>  -0.20    2 0.210000 0.21925
#>  -0.36    1 0.292000 0.29200
#>  -0.52    1 0.356500 0.35650
#>  -0.84    1 0.461500 0.46150
#> 

#  MF = 0.44 comparing vac to con
#
#  MF Subject Components
#
#    mf_j freq    min_y   max_y
#    1.00    6 0.000030 0.00970
#    0.84    1 0.012500 0.01250
#    0.76    3 0.016650 0.02030
#    0.68    6 0.023250 0.03190
#    0.04    1 0.132100 0.13210
#   -0.04    3 0.144575 0.16325
#   -0.20    2 0.210000 0.21925
#   -0.36    1 0.292000 0.29200
#   -0.52    1 0.356500 0.35650
#   -0.84    1 0.461500 0.46150


mean(x$subj[, "mf_j"])
#> [1] 0.44

# [1] 0.44
```
