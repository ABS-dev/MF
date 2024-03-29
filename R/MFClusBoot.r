#' @description Estimates bootstrap confidence intervals for the mitigated
#'   fraction from clustered or stratified data.
#' @details Resamples the data and produces bootstrap confidence intervals.
#'   Equal tailed intervals are estimated by the percentile method. Highest
#'   density intervals are estimated by selecting the shortest of all possible
#'   intervals.
#' @title Bootstrap MF CI from clustered data
#' @param formula Formula of the form \code{y ~ x + cluster(w)}, where y is a
#'   continuous response, x is a factor with two levels of treatment, and w is a
#'   factor indicating the clusters.
#' @param data Data frame. See \code{Note} for handling of input data with more
#'   than two levels.
#' @param compare Text vector stating the factor levels - \code{compare[1]} is
#'   the control or reference group to which \code{compare[2]} is compared
#' @param boot.cluster Boolean whether to resample the clusters.
#' @param boot.unit Boolean whether to resample the units within cluster.
#' @param b Number of bootstrap samples to take with each cycle
#' @param B Number of cycles, giving the total number of samples = B * b
#' @param alpha Complement of the confidence level
#' @param hpd Boolean whether to estimate highest density intervals.
#' @param return.boot Boolean whether to save the bootstrap sample of the MF
#'   statistic.
#' @param trace.it Boolean whether to display verbose tracking of the cycles.
#' @param seed to initialize random number generator for reproducibility. Passed
#'   to \code{set.seed}.
#' @return a \code{\link{mfbootcluster-class}} data object
#' @note If input data contains more than two levels of treatment, rows
#' associated with unused treatment levels will be removed. \cr Factor levels
#' for treatments not present in the input data will be ignored. \cr Clusters
#' with missing treatments will be excluded. See
#' \code{\link{mfbootcluster-class}} or use \code{trace.it} to identify excluded
#' clusters.
#' @references Siev D. (2005). An estimator of intervention effect on disease
#'   severity. \emph{Journal of Modern Applied Statistical Methods.}
#'   \bold{4:500--508}\cr \cr Efron B, Tibshirani RJ. \emph{An Introduction to
#'   the Bootstrap.} Chapman and Hall, New York, 1993.
#' @author \link{MF-package}
#' @examples
#' \dontrun{
#' MFClusBoot(lesion ~ group + cluster(litter), piglung, seed = 12345)
#' Bootstrapping clusters. . . . . . . . . . . . . . . . .
#'
#' Bootstrapping units. . . . . . . . . . . . . . . . . .
#'
#' 10000 bootstrap samples of clusters and units in treatment in cluster
#' Comparing vac to con
#'
#' 95% confidence interval
#'
#' observed    median       lower     upper
#' Equal Tailed    0.3533835 0.3648649 -0.01409471 0.7109966
#' Highest Density 0.3533835 0.3648649  0.00000000 0.7236842
#'
#' Excluded Clusters
#' M, Q, R, B, O, V, I, C
#' }
#' @importFrom stats quantile
#' @export

#--------------------------------------------------------------------
# Bootstrap stratified or clustered MF
#--------------------------------------------------------------------
MFClusBoot <- function(formula, data, compare = c("con", "vac"),
                       boot.cluster = TRUE, boot.unit = TRUE, b = 100,
                       B = 100, alpha = 0.05, hpd = TRUE, return.boot = FALSE,
                       trace.it = FALSE, seed = sample(1:100000, 1)) {
  ## set seed
  set.seed(seed)
  # short circuit if no bootstrapping!
  if (!boot.cluster && !boot.unit) {
    stop("No bootstrapping specified")
  }
  # takes b bootstrap samples B times, so nboot = B * b
  # 3/19/01 initial coding
  # revised 6/30/05 to allow bootstrapping clusters, units, or both
  # revised 8/25/06 to allow possibility there may be only one unit assigned
  #     to one of the groups within a cluster
  # revised 10/3/06 to eliminate clusters without both treatments represented
  # revised 03/07/07 by MMR to add the compare argument in the call to MFClus
  # revised 6/15/07 by DS - moved lines 59-60 from original location to
  #    correctly identify clusters that are eliminated
  # R version 5/6/10 - added quotes in switch()
  # revised 5/25/10 - added empirical HPD interval
  # revised 8/27/13 - remove group levels if no observations from that level
  #      are present in original data
  # revised 9/03/13 - subset initial data by comparison group levels
  # revised 9/03/13 - move data reshaping shared by MFClusBoot and MFClus to
  #      external function
  # revised 1/10/14 - move empirical HPD interval to external function shared

  rng <- "Mersenne-Twister"
  RNGkind(rng)
  dat <- NULL
  group <- NULL
  clusters <- NULL
  strat <- NULL
  reshape_cluster(data = data, formula = formula, compare = compare,
                  envir = environment())
  id <- compare
  keep <- apply(table(group, clusters), 2, function(x) {
    all(x > 0)
  })[strat]
  if (sum(!keep) > 0) {
    if (trace.it) {
      cat("Clusters eliminated because of missing treatments:",
          strat[!keep], "\n")
    }
  }
  excluded.clusters <- strat[!keep]
  strat <- strat[keep]
  n.strat <- length(strat)
  if (boot.cluster) {
    cat("\nBootstrapping clusters")
    if (trace.it) {
      cat("\n")
    }
    strat.b <- matrix(NA, b * B, n.strat)
    for (i in 1:B) {
      strat.b[((i - 1) * b + 1):((i - 1) * b + b), ] <-
        sample(strat, size = b * n.strat,  replace = TRUE)
      if (trace.it) {
        cat("bootstrap clusters, samples", (i - 1) * b + 1, "to",
            (i - 1) * b + b, "\n")
      } else {
        cat(". ")
      }
    }
    cat("\n")
  } else {
    strat.b <- matrix(strat, b * B, n.strat, byrow = TRUE)
  }
  if (!boot.unit) {
    # sum of ranks in each cluster
    w <- u <- n1n2 <- rep(NA, n.strat)
    names(w) <- names(u) <- names(n1n2) <- strat
    for (stratum in strat) {
      x <- dat[group == id[1] & as.character(clusters) == stratum]
      y <- dat[group == id[2] & as.character(clusters) == stratum]
      n.x <- length(x)
      n.y <- length(y)
      x.y <- c(x, y)
      w[stratum] <- sum(rank(x.y)[1:n.x])
      u[stratum] <- w[stratum] - (n.x * (n.x + 1)) / 2
      n1n2[stratum] <- n.x * n.y
    }
    # unused? W <- apply(matrix(w[strat.b], b * B, n.strat), 1, sum)
    U <- apply(matrix(u[strat.b], b * B, n.strat), 1, sum)
    N1N2 <- apply(matrix(n1n2[strat.b], b * B, n.strat), 1, sum)
    R <- U / N1N2
    MF <- 2 * R - 1
  }

  if (boot.unit) {
    # bootstrap units within cluster also
    cat("\nBootstrapping units")
    if (trace.it) {
      cat("\n")
    }
    w.boot <- function(x, y, n.b) {
      n.x <- length(x)
      n.y <- length(y)
      # unused? out <- rep(NA, n.b)
      x.b <- matrix(switch(as.character(n.x == 1),
                           "TRUE" = rep(x, n.b),
                           "FALSE" = sample(x, size = n.b * n.x,
                                            replace = TRUE)),
                    n.b, n.x)
      y.b <- matrix(switch(as.character(n.y == 1),
                           "TRUE" = rep(y, n.b),
                           "FALSE" = sample(y, size = n.b * n.y,
                                            replace = TRUE)),
                    n.b, n.y)
      w <- apply(cbind(x.b, y.b), 1, function(x, n.x) {
        sum(rank(x)[1:n.x])
      }, n.x)
      return(w)
    }
    # how many of each cluster
    w <- u <- n1n2 <- matrix(NA, b * B, n.strat)
    n.each <- n12 <- rep(NA, n.strat)
    names(n.each) <- names(n12) <- strat
    for (stratum in strat) {
      if (trace.it) {
        cat("bootstrapping within cluster", stratum, "\n")
      } else {
        cat(". ")
      }
      x <- dat[group == id[1] & as.character(clusters) == stratum]
      y <- dat[group == id[2] & as.character(clusters) == stratum]
      n.x <- length(x)
      n.y <- length(y)
      n.each[stratum] <- sum(strat.b == stratum)
      w[strat.b == stratum] <- w.boot(x, y, n.each[stratum])
      u[strat.b == stratum] <- w[strat.b == stratum] - (n.x * (n.x + 1)) / 2
      n1n2[strat.b == stratum] <- n.x * n.y
    }
    # unused? W <- apply(w, 1, sum)
    U <- apply(u, 1, sum)
    N1N2 <- apply(n1n2, 1, sum)
    R <- U / N1N2
    MF <- 2 * R - 1
  }
  q <- c(.5, alpha / 2, 1 - alpha / 2)
  mf.All <-  MFClus(formula, data, compare = compare)$All
  mf.obs <- mf.All$mf
  nboot <- b * B
  cluster.text <- ifelse(boot.cluster, "clusters", "")
  and.text <- ifelse(boot.cluster & boot.unit, " and ", "")
  unit.text <- ifelse(boot.unit, "units in treatment in cluster", "")
  the.text <- paste(nboot, " bootstrap samples of ", cluster.text, and.text,
                    unit.text, sep = "")
  stat <- c(Observed = mf.obs, quantile(MF, prob = q))
  stat <- matrix(stat, 1, 4,
                 dimnames = list(c("Equal Tailed"),
                                 c("observed", "median", "lower", "upper")))
  if (hpd) {
    hpdmf <- emp_hpd(MF, alpha = alpha)
    stat <- rbind(stat, "Highest Density" = c(mf.obs, stat[1, "median"],
                                              hpdmf))
  }
  if (return.boot) {
    sample <- MF
  } else {
    sample <- NULL
  }

  return(mfbootcluster$new(stat = stat, nboot = nboot, alpha = alpha,
                           what = the.text,
                           excludedClusters = excluded.clusters, seed = seed,
                           call = match.call(), compare = compare, rng = rng,
                           sample = sample, All =  mf.All))
}
