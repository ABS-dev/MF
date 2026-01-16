# Class mfcluster

Class mfcluster is created from output of function MFClus

## Fields

- `All`: vector with elements

  - `w`: Wilcoxon statistic

  - `u`: Mann-Whitney statistic

  - `r`: mean ridit

  - `n1`: size of group 1

  - `n2`: size of group 2

  - `mf`: mitigated fraction

- `byCluster`: As for All, by clusters

- `excludedClusters`: character vector naming clusters excluded because
  of missing treatment

- `call`: the call to `MFClus`

- `compare`: character vector naming groups compared

## See also

[MFClus](https://abs-dev.github.io/MF/reference/MFClus.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)
