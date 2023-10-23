# shared reshaping portion of MFClus and MFClusBoot
#' @importFrom stats model.frame terms
reshape_cluster <- function(data, formula, compare, envir) {
  cluster <- function(x) {
    return(x)
  }
  clus_terms <- terms(formula, specials = "cluster", data = data)
  environment(clus_terms) <- environment()
  clus_frame <- model.frame(formula = clus_terms, data = data)
  # reduce data to the comparison groups - mcv 09/03/13
  clus_frame <-
    clus_frame[clus_frame[, attr(clus_terms, "term.labels")[1]] %in% compare, ]
  dat <- clus_frame[, 1]
  group <- clus_frame[, 2]
  # remove any group levels that aren't present; don't want to evaluate for
  #   empty groups - mcv 08/27/13
  group <- factor(group)
  clusters <- clus_frame[, 3]
  strat <- unique(as.character(clusters))
  assign("dat", dat, envir = envir)
  assign("group", group, envir = envir)
  assign("clusters", clusters, envir = envir)
  assign("strat", strat, envir = envir)
}

#' @description  used in the bootstrapping functions MFClusBoot  MFBoot HLBoot
#' @importFrom stats quantile
#' @noRd
emp_hpd <- function(x, alpha) {
  # empirical hpd by shortest length interval
  x <- sort(x)
  probs <- cbind(low = seq(0, alpha, .001), high = seq(1 - alpha, 1, .001))
  int_len <- quantile(x, prob = probs[, "high"]) -
    quantile(x, prob = probs[, "low"])
  shortest <- min(int_len)
  first <- which(int_len == shortest)[1]
  hpd <- quantile(x, prob = probs[first, ], type = 7)
  # see ?quantile for type
  return(hpd)
}

#' @importFrom plyr rbind.fill
resample_hier <- function(dat, cluster) {
  # exit early for trivial data
  if (nrow(dat) == 1) {
    return(dat)
  }

  # sample the clustering factor
  cls <- sample(unique(dat[[cluster[1]]]), replace = TRUE)

  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, dat[[cluster[1]]] == b))

  # sample lower levels of hierarchy (if any)
  # recursive call to resample
  if (length(cluster) > 1) {
    sub <- lapply(sub, resample_hier, cluster = cluster[-1])
  }

  # join and return samples
  rbind.fill(sub)
}
