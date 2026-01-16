# Bootstrap CI for MF, HL, and Qdif

Estimates bootstrap confidence intervals for MF, HL, and Qdif.

## Usage

``` r
HLBoot(
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

  Formula of the form `y ~ x + cluster(w)`, where y is a continuous
  response, x is a factor with two levels of treatment, and w is a
  factor indicating the clusters.

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

  Boolean whether to estimate highest density intervals for MF and HL.

- bca:

  Boolean whether to estimate BCa intervals for MF.

- return.boot:

  Boolean whether to save the bootstrap samples of the statistics.

- trace.it:

  Boolean whether to display verbose tracking of the cycles.

- seed:

  to initialize random number generator for reproducibility. Passed to
  `set.seed`.

## Value

a [mfhlboot](https://abs-dev.github.io/MF/reference/mfhlboot-class.md)
data object

## Details

Estimates bootstrap confidence intervals for the mitigated fraction
(MF), Hodge-Lehmann estimator (HL), and the difference of medians and
quartiles (Qdif). Equal tailed intervals are provided for all three,
highest density intervals are optionally provided for MF and HL, and BCa
intervals are optionally provided for MF. The Hodges-Lehmann estimator
is the median difference; it assumes that the two distributions have the
same shape and differ by a constant shift. Assumes data is single pool
(no nesting).

## References

Hodges JL, Lehmann EL, (1963). Estimates of location based on rank
tests. *Annals of Mathematical Statistics.* **34:598–611**.

Siev D, (2005). An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* **4:500–508**.

Efron B, Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman and
Hall, New York, 1993.

## See also

[mfhlboot](https://abs-dev.github.io/MF/reference/mfhlboot-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
HLBoot(lesion ~ group, calflung, seed = 12345)
#> 
#> Bootstrapping
#> . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#> 
#> 10000 bootstrap samples
#> 95% confidence intervals
#> Comparing vac to con 
#> 
#> Seed =  12345
#> Mitigated Fraction
#> 
#>                 observed median  lower  upper
#> Equal Tailed        0.44 0.4464 0.1328 0.7120
#> Highest Density     0.44 0.4464 0.1456 0.7184
#> 
#> 
#> Hodges-Lehmann
#> 
#>                 observed   median    lower    upper
#> Equal Tailed    -0.07335 -0.07335 -0.17375 -0.01445
#> Highest Density -0.07335 -0.07335 -0.16330 -0.01077
#> 
#> 
#> Quartile Differences (quartiles of vac - quartiles of con)
#> 
#>      observed    median   lower     upper
#> Q25 -0.041500 -0.041300 -0.1034 -0.000905
#> Q50 -0.112525 -0.111175 -0.2819  0.023200
#> Q75 -0.168000 -0.170425 -0.3889  0.030000
#> 
#> 
#> Quartiles of con 
#>     observed   median   lower   upper
#> Q25 0.054000 0.054000 0.01525 0.11275
#> Q50 0.139275 0.139275 0.06140 0.31000
#> Q75 0.315000 0.315000 0.17300 0.45250
#> 
#> 
#> Quartiles of vac 
#>     observed  median   lower    upper
#> Q25  0.01250 0.01250 0.00125 0.026000
#> Q50  0.02675 0.02675 0.01665 0.144575
#> Q75  0.14700 0.14700 0.02810 0.219250
#> 

# Bootstrapping
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . .
#
# 10000 bootstrap samples
# 95% confidence intervals
# Comparing vac to con
#
#
# Mitigated Fraction
#
# observed median lower  upper
# Equal Tailed        0.44 0.4496 0.152 0.7088
# Highest Density     0.44 0.4496 0.152 0.7088
#
#
# Hodges-Lehmann
#
# observed   median    lower       upper
# Equal Tailed    -0.07335 -0.07615 -0.17220 -0.01565000
# Highest Density -0.07335 -0.07615 -0.15635 -0.00850065
#
#
# Quartile Differences (quartiles of vac - quartiles of con)
#
# observed    median    lower     upper
# Q25 -0.041500 -0.041500 -0.10340 -0.000905
# Q50 -0.112525 -0.111175 -0.28115  0.019350
# Q75 -0.168000 -0.170425 -0.38890  0.005300
#
#
# Quartiles of con
# observed   median    lower   upper
# Q25 0.054000 0.054000 0.021005 0.11275
# Q50 0.139275 0.139275 0.061400 0.31000
# Q75 0.315000 0.315000 0.173000 0.44625
#
#
# Quartiles of vac
# observed  median   lower    upper
# Q25  0.01250 0.01250 0.00125 0.026000
# Q50  0.02675 0.02675 0.01665 0.144575
# Q75  0.14700 0.14700 0.02810 0.219250
```
