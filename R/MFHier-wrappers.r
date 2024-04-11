#' @name MFClusHier
#' @title MFClusHier
#' @description Calculate mitigated fraction directly from hierarchial nested
#'   data. Combines \code{\link{MFh}} and \code{\link{MFnest}} into a single
#'   function.
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous
#'   response, x is a factor with two levels of treatment, and a/b/c are
#'   grouping variables corresponding to the clusters. Nesting is assumed to be
#'   in order, left to right, highest to lowest. So a single level of "a" will
#'   contain multiple levels of "b" and a single level of "b" will contain
#'   multiple levels of "c".
#' @param data a data.frame or tibble with the variables specified in formula.
#'   Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - `compare[1]` is the
#'   control or reference group to which `compare[2]` is compared.
#' @param which.factor one or more variable(s) of interest. This can be any of
#'   the core or nest variables from the data set. If none or NULL is specified,
#'   MF will be calculated for the whole tree.
#' @return A list with the following elements: \cr \cr \itemize{ \item
#'   \strong{MFh} as output from \code{\link{MFh}}. \item \strong{MFnest} as
#'   output from \code{\link{MFnest}}. }
#' @note \code{Core} variable is the variable corresponding to the lowest nodes
#'   of the hierarchical tree. \code{Nest} variables are those above the core.
#'   \code{All} refers to a summary of the entire tree.
#' @export
#' @seealso \code{\link{MFh}}, \code{\link{MFnest}}
#' @author \link{MF-package}
#' @examples
#' a <- data.frame(
#' room = paste('Room', rep(c('W','Z'), each=24)),
#' pen = paste('Pen', rep(LETTERS[1:6], each=8)),
#' litter = paste('Litter', rep(11:22, each=4)),
#' tx = rep(rep(c('vac','con'), each=2), 12),
#' stringsAsFactors = FALSE
#' )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx=='con'] <- rnorm(24, 7, 1.3)
#' thismf <- MFClusHier(lung ~ tx + room / pen / litter, a)
#' thismf$MFnest
#' aCore <- thismf$MFh
#' aCore
#' aCore$data
#' aCore$formula
#' aCore$compare
MFClusHier <- function(formula, data, compare = c("con", "vac"),
                       which.factor = "All") {
  aCore <- MFh(formula, data, compare)
  out <- mfclushier$new(MFh = aCore, MFnest = MFnest(aCore, which.factor))
  return(out)
}


#' @title MFClusBootHier
#' @name MFClusBootHier
#' @description Combines \code{\link{MFhBoot}} and \code{\link{MFnestBoot}} into
#'   a single function.
#' @param formula formula Formula of the form y ~ x + a/b/c, where y is a
#'   continuous response, x is a factor with two levels of treatment, and a/b/c
#'   are grouping variables corresponding to the clusters. Nesting is assumed to
#'   be in order, left to right, highest to lowest. So a single level of "a"
#'   will contain multiple levels of "b" and a single level of "b" will contain
#'   multiple levels of "c".
#' @param data a data.frame or tibble with the variables specified in formula.
#'   Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - `compare[1]` is the
#'   control or reference group to which `compare[2]` is compared.
#' @param nboot number of bootstrapping events
#' @param boot.unit Boolean whether to sample observations from within those of
#'   the same core.
#' @param boot.cluster Boolean whether to sample which cores are present. If
#'   TRUE, some trees have all the cores while others only have a subset.
#' @param which.factor Which variables to include in the mitigated fraction
#'   summation. Default is 'All', to sum over entire tree.
#' @param alpha Passed to \code{emp_hpd} to calculate high tailed upper and high
#'   tailed lower of mitigated fraction.
#' @param seed Passed to \code{\link{MFhBoot}} to to initialize random number
#'   generator for reproducibility.
#' @return A list with the following elements: \cr \cr \itemize{ \item
#'   \strong{MFhBoot} as output from \code{\link{MFhBoot}}. \item
#'   \strong{MFnestBoot} as output from \code{\link{MFnestBoot}}. }
#' @note \code{Core} variable is the variable corresponding to the lowest nodes
#'   of the hierarchical tree. \code{Nest} variables are those above the core.
#'   \code{All} refers to a summary of the entire tree.
#' @seealso \code{\link{MFhBoot}}, \code{\link{MFnestBoot}}.
#' @author \link{MF-package}
#' @export
#' @examples
#' a <- data.frame(
#'   room = paste('Room', rep(c('W','Z'), each=24)),
#'   pen = paste('Pen', rep(LETTERS[1:6], each=8)),
#'   litter = paste('Litter', rep(11:22, each=4)),
#'   tx = rep(rep(c('vac','con'), each=2), 12),
#'   stringsAsFactors = FALSE
#' )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx=='con'] <- rnorm(24, 7, 1.3)
#' thismf1 <- MFClusBootHier(lung ~ tx + room / pen / litter, a, nboot = 10000,
#'                        boot.cluster = TRUE, boot.unit = TRUE, seed = 12345)
#' thismf1
MFClusBootHier <- function(formula, data, compare = c("con", "vac"),
                           nboot = 10000, boot.unit = TRUE, boot.cluster = TRUE,
                           which.factor = "All", alpha = 0.05,
                           seed = sample(1:1e+05, 1)) {
  thisbootmfh <- MFhBoot(formula = formula, data = data, compare = compare,
                         nboot = nboot, boot.unit = boot.unit,
                         boot.cluster = boot.cluster,
                         seed = seed)
  out <- mfclusboothier$new(MFhBoot = thisbootmfh,
                            MFnestBoot = MFnestBoot(thisbootmfh,
                                                    which.factor, alpha))
  return(out)
}
