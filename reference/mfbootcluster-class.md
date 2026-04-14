# Class mfbootcluster

Class mfbootcluster is created from output of function MFClusBoot

## Fields

- `nboot`: numeric value specifying number of samples

- `alpha`: numeric value specifying complement of confidence interval

- `seed`: vector of integers specifying seed for pseudo-random number
  generator used

- `compare`: vector of character strings naming groups compared

- `rng`: character string naming type of random number generator

- `stat`: matrix matrix with columns *observed*, *median*, *lower*,
  *upper* for estimates

- `what`: character vector naming what was resampled: *clusters*,
  *units*, *both*

- `excludedClusters`: character vector naming clusters excluded because
  of missing treatment(s)

- `call`: the call to `MFClusBoot`

- `sample`: what is this?

- `All`: Field "All" from MFClus call.

## Contains

[mf](https://abs-dev.github.io/MF/reference/mf-class.md)

## See also

[MFClusBoot](https://abs-dev.github.io/MF/reference/MFClusBoot.md)

Other mf:
[`mf-class`](https://abs-dev.github.io/MF/reference/mf-class.md),
[`mfboot-class`](https://abs-dev.github.io/MF/reference/mfboot-class.md),
[`mfhlboot-class`](https://abs-dev.github.io/MF/reference/mfhlboot-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)
