\docType{class}
\name{mfhlboot-class}
\alias{mfhlboot-class}
\title{Class mfhlboot}
\usage{
mfhlboot$new(nboot, alpha, seed, compare, rng, sample, MFstat, HLstat,
QDIFstat, QXstat, QYstat)
}
\description{
class for data objects produced by HLBoot, contains class
mf with addtional fields \emph{MFstat, HLstat, QDIFstat,
QXstat, QYstat}
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
  \item{\code{sample: }}{ what is this?}
  \item{\code{MFstat}}{matrix with columns \emph{observed,
  median, lower, upper} for Equal Tailed and Highest
  Density estimates of mitigated fraction (MF)}
  \item{\code{HLstat}}{matrix with columns \emph{observed,
  median, lower, upper} for Equal Tailed and Highest
  Density estimates of Hodge-Lehmann estimator (HL)}
  \item{\code{QDIFstat}}{matrix with columns
  \emph{observed, median, lower, upper} for estimates of
  Quartile Differences} \item{\code{QXstat}}{matrix with
  columns \emph{observed, median, lower, upper} for
  quartiles of treatments} \item{\code{QYstat}}{matrix with
  columns \emph{observed, median, lower, upper} for
  quartiles of responses} \item{\code{sample: }}{ what is
  this?} }
}

\section{Contains}{
  \code{\link{mf-class}}
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
\code{\link{HLBoot}}

Other mf: \code{\link{mf-class}};
\code{\link{mfboot-class}};
\code{\link{mfbootcluster-class}}
}
\keyword{documentation}

