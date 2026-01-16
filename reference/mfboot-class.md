# Class mfboot

class for data objects produced by MFBoot, contains class mf with the
two additional fields *stat* and *stuff*.

## Fields

- `nboot`: numeric value specifying number of samples

- `alpha`: numeric value specifying complement of confidence interval

- `seed`: vector of integers specifying seed for pseudo-random number
  generator used

- `compare`: vector of character strings naming groups compared

- `rng`: character string naming type of random number generator

- `sample`: what is this?

- `stat`: matrix of estimates

## Contains

[mf](https://abs-dev.github.io/MF/reference/mf-class.md)

## See also

[MFBoot](https://abs-dev.github.io/MF/reference/MFBoot.md)

Other mf:
[`mf-class`](https://abs-dev.github.io/MF/reference/mf-class.md),
[`mfbootcluster-class`](https://abs-dev.github.io/MF/reference/mfbootcluster-class.md),
[`mfhlboot-class`](https://abs-dev.github.io/MF/reference/mfhlboot-class.md)

## Author

[MF-package](https://abs-dev.github.io/MF/reference/MF-package.md)
