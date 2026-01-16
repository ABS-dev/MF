# Bootstrap MF CI

Estimates bootstrap confidence intervals for the mitigated fraction.

## Usage

``` r
MFBoot(
  formula,
  data,
  compare = c("con", "vac"),
  b = 100,
  B = 100,
  alpha = 0.05,
  hpd = TRUE,
  bca = FALSE,
  return.boot = FALSE,
  trace.it = FALSE,
  seed = sample(1:1e+05, 1)
)
```

## Arguments

- formula:

  Formula of the form `y ~ x`, where y is a continuous response and x is
  a factor with two levels.

- data:

  Data frame

- compare:

  Text vector stating the factor levels - `compare[1]` is the control or
  reference group to which `compare[2]` is compared

- b:

  Number of bootstrap samples to take with each cycle

- B:

  Number of cycles, giving the total number of samples = B \* b

- alpha:

  Complement of the confidence level

- hpd:

  Estimate highest density intervals?

- bca:

  Estimate BCa intervals?

- return.boot:

  Save the bootstrap sample of the MF statistic?

- trace.it:

  Verbose tracking of the cycles?

- seed:

  to initialize random number generator for reproducibility. Passed to
  `set.seed`.

## Value

a [mfboot](https://abs-dev.github.io/MF/reference/mfboot-class.md) data
object

## Details

Resamples the data and produces bootstrap confidence intervals. Equal
tailed intervals are estimated by the percentile method. Highest density
intervals are estimated by selecting the shortest of all possible
intervals. For BCa intervals, see Efron and Tibshirani section 14.3.

## References

Siev D. (2005). An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* **4:500–508**

Efron B, Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman and
Hall, New York, 1993.

## See also

[mfboot](https://abs-dev.github.io/MF/reference/mfboot-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
MFBoot(lesion ~ group, calflung, seed = 12345)
#> 10000 bootstrap samples
#>  95% confidence interval
#> Seed =  12345
#> 
#> Comparing vac to con 
#>                 observed median  lower  upper
#> Equal Tailed        0.44 0.4464 0.1328 0.7120
#> Highest Density     0.44 0.4464 0.1456 0.7184
#> 

# 10000 bootstrap samples
# 95% confidence interval
# Seed = 12345
#
# Comparing vac to con
# observed median lower  upper
# Equal Tailed        0.44 0.4496 0.152 0.7088
# Highest Density     0.44 0.4496 0.152 0.7088
```
