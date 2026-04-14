# Bootstrap MF CI from clustered data

Estimates bootstrap confidence intervals for the mitigated fraction from
clustered or stratified data.

## Usage

``` r
MFClusBoot(
  formula,
  data,
  compare = c("con", "vac"),
  boot.cluster = TRUE,
  boot.unit = TRUE,
  b = 100,
  B = 100,
  alpha = 0.05,
  hpd = TRUE,
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

  Data frame. See `Note` for handling of input data with more than two
  levels.

- compare:

  Text vector stating the factor levels - `compare[1]` is the control or
  reference group to which `compare[2]` is compared

- boot.cluster:

  Boolean whether to resample the clusters.

- boot.unit:

  Boolean whether to resample the units within cluster.

- b:

  Number of bootstrap samples to take with each cycle

- B:

  Number of cycles, giving the total number of samples = B \* b

- alpha:

  Complement of the confidence level

- hpd:

  Boolean whether to estimate highest density intervals.

- return.boot:

  Boolean whether to save the bootstrap sample of the MF statistic.

- trace.it:

  Boolean whether to display verbose tracking of the cycles.

- seed:

  to initialize random number generator for reproducibility. Passed to
  `set.seed`.

## Value

a
[mfbootcluster](https://abs-dev.github.io/MF/reference/mfbootcluster-class.md)
data object

## Details

Resamples the data and produces bootstrap confidence intervals. Equal
tailed intervals are estimated by the percentile method. Highest density
intervals are estimated by selecting the shortest of all possible
intervals.

## Note

If input data contains more than two levels of treatment, rows
associated with unused treatment levels will be removed.

Factor levels for treatments not present in the input data will be
ignored.

Clusters with missing treatments will be excluded. See
[mfbootcluster](https://abs-dev.github.io/MF/reference/mfbootcluster-class.md)
or use `trace.it` to identify excluded clusters.

## References

Siev D. (2005). An estimator of intervention effect on disease severity.
*Journal of Modern Applied Statistical Methods.* **4:500–508**

Efron B, Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman and
Hall, New York, 1993.

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)

## Examples

``` r
if (FALSE) { # \dontrun{
MFClusBoot(lesion ~ group + cluster(litter), piglung, seed = 12345)
# Bootstrapping clusters. . . . . . . . . . . . . . . . .
#
# Bootstrapping units. . . . . . . . . . . . . . . . . .
#
# 10000 bootstrap samples of clusters and units in treatment in cluster
# Comparing vac to con
#
# 95% confidence interval
#
# observed    median       lower     upper
# Equal Tailed    0.3533835 0.3648649 -0.01409471 0.7109966
# Highest Density 0.3533835 0.3648649  0.00000000 0.7236842
#
# Excluded Clusters
# M, Q, R, B, O, V, I, C
} # }
```
