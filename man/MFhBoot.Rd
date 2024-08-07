% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MFnestBoot.r
\name{MFhBoot}
\alias{MFhBoot}
\title{MFhBoot}
\usage{
MFhBoot(
  formula,
  data,
  compare = c("con", "vac"),
  nboot = 10000,
  boot.unit = TRUE,
  boot.cluster = TRUE,
  seed = sample(1:1e+05, 1)
)
}
\arguments{
\item{formula}{Formula of the form y ~ x + a/b/c, where y is a continuous
response, x is a factor with two levels of treatment, and a/b/c are
grouping variables corresponding to the clusters. Nesting is assumed to be
in order, left to right, highest to lowest. So a single level of "a" will
contain multiple levels of "b" and a single level of "b" will contain
multiple levels of "c".}

\item{data}{a data.frame or tibble with the variables specified in formula.
Additional variables will be ignored.}

\item{compare}{Text vector stating the factor levels - \code{compare[1]} is the
control or reference group to which \code{compare[2]} is compared.}

\item{nboot}{number of bootstrapping events}

\item{boot.unit}{Boolean whether to sample observations from within those of
the same core.}

\item{boot.cluster}{Boolean whether to sample which cores are present. If
TRUE, some trees have all the cores while others only have a subset.}

\item{seed}{to initialize random number generator for reproducibility. Passed
to \code{set.seed}.}
}
\value{
A list with the following elements:
\itemize{
\item \code{bootmfh}: Rank table for the bootstrapped values as output from
\link{MFh}. Includes a new \code{bootID} variable to distinguish each bootstrapped
incidence.
\item \code{clusters}: Table of unique nodes with an ID.
\item \code{compare}: Compare vector as specified by user.
\item \code{mfh}: MFh run on original data input.
}
}
\description{
Calculate rank tables for MF using bootstrapping.
}
\examples{
set.seed(76153)
a <- data.frame(room = paste("Room", rep(c("W", "Z"), each = 24)),
                pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
                litter = paste("Litter", rep(11:22, each = 4)),
                tx = rep(rep(c("vac", "con"), each = 2), 12))
a[a$tx == "vac", "lung"] <-  rnorm(24, 5, 1.3)
a[a$tx == "con", "lung"] <- rnorm(24, 7, 1.3)
a

formula <- lung ~ tx + room / pen / litter
nboot <- 10000
boot.cluster <- TRUE
boot.unit <- TRUE
which.factors <- c("All", "room", "pen", "litter")

system.time(test1 <- MFhBoot(formula, a,
                            nboot = 10000,
                             boot.cluster = TRUE,
                             boot.unit = TRUE,
                             seed = 12345))
test1$bootmfh
}
\seealso{
\link{MFClusBootHier}, \link{MFnestBoot}
}
\author{
\link{MF-package}
}
