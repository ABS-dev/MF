# Class mfhlboot

class for data objects produced by HLBoot, contains class mf with
additional fields *MFstat*, *HLstat*, *QDIFstat*, *QXstat*, *QYstat*

## Fields

- `nboot`: Numeric value specifying number of samples.

- `alpha`: Numeric value specifying complement of confidence interval.

- `seed`: Vector of integers specifying seed for pseudo-random number
  generator used.

- `compare`: Vector of character strings naming groups compared.

- `rng`: Character string naming type of random number generator.

- `sample`: The bootstrapped values.

- `MFstat`: Matrix with columns *observed*, *median*, *lower*, *upper*
  for Equal Tailed and Highest Density estimates of mitigated fraction
  (MF).

- `HLstat`: Matrix with columns *observed*, *median*, *lower*, *upper*
  for Equal Tailed and Highest Density estimates of Hodge-Lehmann
  estimator (HL).

- `QDIFstat`: Matrix with columns *observed*, *median*, *lower*, *upper*
  for estimates of Quartile Differences.

- `QXstat`: Matrix with columns *observed*, *median*, *lower*, *upper*
  for quartiles of treatments, equal tailed.

- `QYstat`: Matrix with columns *observed*, *median*, *lower*, *upper*
  for quartiles of response, equal tailed.

## Contains

[mf](https://abs-dev.github.io/MF/reference/mf-class.md)

## See also

[HLBoot](https://abs-dev.github.io/MF/reference/HLBoot.md)

Other mf:
[`mf-class`](https://abs-dev.github.io/MF/reference/mf-class.md),
[`mfboot-class`](https://abs-dev.github.io/MF/reference/mfboot-class.md),
[`mfbootcluster-class`](https://abs-dev.github.io/MF/reference/mfbootcluster-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)
