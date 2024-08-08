#' Estimates mitigated fraction from clustered or stratified data.
#'
#' Averages the U statistic over the clusters and computes MF from it. Clusters
#' are excluded if they do not include both treatments.
#'
#' @title Clustered mitigated fraction
#' @param formula Formula of the form `y ~ x + cluster(w)`, where y is a
#'   continuous response, x is a factor with two levels of treatment, and w is a
#'   factor indicating the clusters.
#' @param data Data frame.  See `Note` for handling of input data with more than
#'   two levels.
#' @param vac_grp The name of the vaccinated group.
#' @param con_grp The name of the control group.
#' @param trace.it Verbose tracking of the cycles? Default FALSE.
#' @param compare `r badge("deprecated")` Text vector stating the factor levels: `compare[1]` is the
#'   control or reference group to which `compare[2]` (vaccinate) is compared
#' @returns a [mfcluster-class] data object
#' @note If input data contains more than two levels of treatment, rows
#'   associated with unused treatment levels will be removed.
#'
#'   Factor levels for treatments not present in the input data will be ignored.
#'
#'   Clusters with missing treatments will be excluded. See
#'   [mfbootcluster-class] or use `trace.it` to identify excluded clusters.
#' @references Siev D. (2005). An estimator of intervention effect on disease
#'   severity. *Journal of Modern Applied Statistical Methods.*
#'   **4:500--508**
#' @author [MF-package]
#' @seealso [mfcluster-class]
#' @examples
#' \dontrun{
#' MFClus(lesion ~ group + cluster(litter), piglung)
#' }
#' @importFrom lifecycle badge deprecate_warn is_present deprecated
#' @export
MFClus <- function(formula,
                   data,
                   vac_grp = "vac",
                   con_grp = "con",
                   trace.it = FALSE,
                   compare = deprecated()) {
  # formula of the form response ~ treatment + cluster(clustername)
  # based on prob{F(y) < F(x)}
  # within-cluster ranking only
  # 3/19/01 initial coding
  # revised 10/3/06 to eliminate clusters without both treatments represented
  # revised 8/27/13 - remove group levels if no observations from that level
  #     are present in original data
  # revised 9/03/13 - subset initial data by comparison group levels
  # revised 9/03/13 - move data reshaping shared by MFClusBoot and MFClus to
  #     external function
  dat <- NULL
  group <- NULL
  clusters <- NULL
  strat <- NULL
  reshape_cluster(data = data, formula = formula, vac_grp = vac_grp,
                  con_grp = con_grp, envir = environment())
  out <- matrix(NA, length(strat), 6,
                dimnames = list(strat, c("w", "u", "r", "n1", "n2", "mf")))
  excluded.clusters <- NULL
  for (stratum in strat) {
    x <- dat[group == con_grp & as.character(clusters) == stratum]
    y <- dat[group == vac_grp & as.character(clusters) == stratum]
    n_x <- length(x)
    n_y <- length(y)
    if (n_x > 0 && n_y > 0) {
      x_y <- c(x, y)
      w <- sum(rank(x_y)[1:n_x])
      u <- w - (n_x * (n_x + 1)) / 2
      r <- u / (n_x * n_y)
      mf <- 2 * r - 1
      out[stratum, ] <- c(w, u, r, n_x, n_y, mf)
    } else {
      if (trace.it) {
        cat("Cluster", stratum, "missing a treatment\n")
      }
      out[stratum, ] <- c(NA, NA, NA, n_x, n_y, NA)
      excluded.clusters <- c(excluded.clusters, stratum)
    }
  }
  All <- apply(out, 2, sum, na.rm = TRUE)
  R <- All["u"] / sum(out[, "n1"] * out[, "n2"])
  MF <- 2 * R - 1
  All["r"] <- R
  All["mf"] <- MF
  All <- data.frame(t(All))
  dimnames(All)[[1]] <- "All"

  if (round(All$mf, digits = 1) == 1.0) {
    message("Complete separation observed.")
  }
  return(mfcluster$new(All = All, byCluster = out,
                       excludedClusters = excluded.clusters,
                       call = match.call(),
                       vac_grp = vac_grp, con_grp = con_grp))
}
