\docType{class}
\name{mfboot-class}
\alias{mfboot-class}
\title{Class mfboot}
\usage{
mfboot$new(nboot, alpha, seed, compare, rng, sample, stat, stuff)
}
\description{
class for data objects produced by MFBoot, contains class
mf with the two additional fields \emph{stat} and
\emph{stuff}.
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
  \item{\code{sample: }}{ what is this?} \item{\code{stat:}
  }{matrix of estimates} }
}

\section{Contains}{
  \code{\link{mf-class}}
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
\code{\link{MFBoot}}

Other mf: \code{\link{mf-class}};
\code{\link{mfbootcluster-class}};
\code{\link{mfhlboot-class}}
}
\keyword{documentation}

