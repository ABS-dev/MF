\name{MFSubj}
\alias{MFSubj}
\title{Subject components of mitigated fraction}
\usage{
MFSubj(formula, data, compare = c("con", "vac"))
}
\arguments{
  \item{formula}{Formula of the form \code{y ~ x}, where y
  is a continuous response and x is a factor with two
  levels}

  \item{data}{Data frame}

  \item{compare}{Text vector stating the factor levels -
  \code{compare[1]} is the control or reference group to
  which \code{compare[2]} is compared}
}
\value{
a \code{\link{mfcomponents-class}} data object
}
\description{
Estimates the subject components of the mitigated fraction.
}
\details{
The mitigated fraction is an estimator that quantifies an
intervention's effect on reducing the severity of a
condition. Since its units are on the probability scale, it
is often a good idea to accompany it with an estimator on
the original scale of measurement. \cr \cr The subject
components are the individual contributions of the treated
subjects to \emph{MF}, which is the average of the subject
components.
}
\examples{
x <- MFSubj(lesion ~ group, calflung)
x

#  MF = 0.44 comparing vac to con
#
#  MF Subject Components
#
#    mf.j freq    min.y   max.y
#    1.00    6 0.000030 0.00970
#    0.84    1 0.012500 0.01250
#    0.76    3 0.016650 0.02030
#    0.68    6 0.023250 0.03190
#    0.04    1 0.132100 0.13210
#   -0.04    3 0.144575 0.16325
#   -0.20    2 0.210000 0.21925
#   -0.36    1 0.292000 0.29200
#   -0.52    1 0.356500 0.35650
#   -0.84    1 0.461500 0.46150


mean(x$subj[,'mf.j'])

# [1] 0.44
}
\author{
David Siev \email{david.siev@aphis.usda.gov}
}
\references{
Siev D. (2005). An estimator of intervention effect on
disease severity. \emph{Journal of Modern Applied
Statistical Methods.} \bold{4:500--508}
}

