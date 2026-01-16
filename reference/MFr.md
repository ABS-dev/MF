# Mitigated fraction

Mitigated fraction comparing treatment to control.

## Usage

``` r
MFr(formula, data, compare = c("con", "vac"))
```

## Arguments

- formula:

  Formula of the form `y ~ x`, where y is a continuous response and x is
  a factor with two levels

- data:

  Data frame

- compare:

  Text vector stating the factor levels – `compare[1]` is the control or
  reference group to which `compare[2]` is compared

## Value

The estimated mitigated fraction.

## Details

The mitigated fraction is an estimator that quantifies an intervention's
effect on reducing the severity of a condition. Since its units are on
the probability scale, it is often a good idea to accompany it with an
estimator on the original scale of measurement.

## References

Siev D, 2005. An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* 4:500-508

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
MFr(lesion ~ group, calflung)
#> [1] 0.44
# [1] 0.44
```
