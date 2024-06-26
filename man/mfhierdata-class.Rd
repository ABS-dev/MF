% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.r
\docType{class}
\name{mfhierdata-class}
\alias{mfhierdata-class}
\alias{mfhierdata}
\title{Class mfhierdata}
\description{
Class mfhierdata is created from output of function MFh
}

\section{Fields}{

\describe{
\item{\code{coreTbl}}{data.frame with one row for each unique core level
showing values for \verb{nx``ny``N``w``u}and
median observed response.}
\item{\code{data}}{data.frame is the restructured input data used for
calculations in MFh and MFnest.}
\item{\code{compare}}{character vector naming groups being compared.}
\item{\code{formula}}{formula that was called by user.}
}
}

\seealso{
\link{MFh}
}
\author{
\link{MF-package}
}
\concept{mfhierdata}
\keyword{documentation}
