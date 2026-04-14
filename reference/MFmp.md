# Mitigated fraction from matched pairs

Estimates mitigated fraction from matched pairs.

## Usage

``` r
MFmp(
  formula = NULL,
  data = NULL,
  compare = c("con", "vac"),
  x = NULL,
  alpha = 0.05,
  df = NA,
  tdist = TRUE
)
```

## Arguments

- formula:

  Formula of the form `y ~ x + cluster(w)`, where y is a continuous
  response, x is a factor with two levels of treatment, and w is a
  factor indicating the clusters.

- data:

  Data frame

- compare:

  Text vector stating the factor levels - `compare[1]` is the control or
  reference group to which `compare[2]` is compared

- x:

  Trinomial vector \\\\\Sigma I(x \< y), \Sigma I(x = y), \Sigma I(x \>
  y)\\\\

- alpha:

  Complement of the confidence level.

- df:

  Degrees of freedom. Default N-2

- tdist:

  Use quantiles of t or Gaussian distribution for confidence interval?
  Default t distribution.

## Value

a [mfmp](https://abs-dev.github.io/MF/reference/mfmp-class.md) data
object

## Details

Estimates *MF* from matched pairs by the difference of multinomial
fractions \\(\Sigma I(x\<y) - \Sigma I(x \> y)) / N\\. The trinomial
vector is \\\\\Sigma I(x\<y), \Sigma I(x = y), \Sigma I(x \> y)\\\\

## Note

upper confidence interval is truncated to 1; lower confidence interval
is truncated to -1. Point estimate of 1.0 indicates complete separation.

## References

Siev D. (2005). An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* **4:500–508**

## See also

[mfmp](https://abs-dev.github.io/MF/reference/mfmp-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
MFmp(les ~ tx + cluster(cage), mlesions, compare = c("con", "vac"))
#> 95% t intervals on 24 df
#>  
#>     point     lower     upper 
#> 0.3846154 0.1316679 0.6375628 
MFmp(x = c(12, 12, 2))
#> 95% t intervals on 24 df
#>  
#>     point     lower     upper 
#> 0.3846154 0.1316679 0.6375628 
```
