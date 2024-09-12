\docType{class}
\name{mf-class}
\alias{mf-class}
\title{Class mf}
\usage{
mf$new(nboot, alpha, seed, compare, rng)
}
\description{
Parent class for package MF data objects.
}
\section{Fields}{
  \itemize{ \item{\code{nboot: } }{numeric value specifying
  number of samples} \item{\code{alpha: }}{numeric value
  specifying complement of confidence interval}
  \item{\code{seed: }}{vector of integers specifying seed
  for pseudo-random number generator used}
  \item{\code{compare: }}{vector of character strings
  naming groups compared} \item{\code{rng: }}{character
  string naming type of random number generator} }
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
Other mf: \code{\link{mfboot-class}};
\code{\link{mfbootcluster-class}};
\code{\link{mfhlboot-class}}
}
\keyword{documentation}

