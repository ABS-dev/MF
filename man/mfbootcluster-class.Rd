\docType{class}
\name{mfbootcluster-class}
\alias{mfbootcluster-class}
\title{Class mfbootcluster}
\usage{
mfbootcluster$new(nboot, alpha, seed, compare, rng, stat, what, excludedClusters,
call, sample)
}
\description{
Class mfbootcluster is created from output of function
MFClusBoot
}
\section{Fields}{
  \itemize{ \item{\code{nboot: } }{numeric value specifying
  number of samples} \item{\code{alpha: }}{numeric value
  specifying complement of confidence interval}
  \item{\code{seed: }}{vector of integers specifying seed
  for pseudo-random number generator used}
  \item{\code{compare: }}{vector of character strings
  naming groups compared} \item{\code{rng: }}{character
  string naming type of random number generator}
  \item{\code{stat: }}{matrix matrix with columns
  \emph{observed, median, lower, upper} for estimates}
  \item{\code{what: }}{character vector naming what was
  resampled: \emph{clusters}, \emph{units}, \emph{both}}
  \item{\code{excludedClusters: }}{character vector naming
  clusters excluded because of missing treatment(s)}
  \item{\code{call: }}{the call to \code{MFClusBoot}}
  \item{\code{sample: }}{what is this?} }
}

\section{Contains}{
  \code{\link{mf-class}}
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
\code{\link{MFClusBoot}}

Other mf: \code{\link{mf-class}};
\code{\link{mfboot-class}}; \code{\link{mfhlboot-class}}
}
\keyword{documentation}

