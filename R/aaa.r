# shared reshaping portion of MFClus and MFClusBoot
reshapeCluster <- function(data, formula, compare, envir){
  cluster <- function(x){
    return(x)
  }
  this.call <- match.call()
  Terms <- terms(formula, specials = "cluster", data = data)
  environment(Terms) <- environment()
  A <- model.frame(formula = Terms, data = data)
# reduce data to the comparison groups - mcv 09/03/13
  A <- A[A[, attr(Terms, "term.labels")[1]] %in% compare, ]
  dat <- A[, 1]
  group <- A[, 2]
# remove any group levels that aren't present; don't want to evaluate for
#   empty groups - mcv 08/27/13
  group <- factor(group)
  clusters <- A[, 3]
  strat <- unique(as.character(clusters))
  assign("dat", dat, envir = envir)
  assign("group", group, envir = envir)
  assign("clusters", clusters, envir = envir)
  assign("strat", strat, envir = envir)
}

# used in the bootstrapping functions MFClusBoot  MFBoot HLBoot
#' @export
emp.hpd <- function(X, alpha){
    # empirical hpd by shortest length interval
    X <- sort(X)
    probs <- cbind(low = seq(0, alpha, .001), high = seq(1 - alpha, 1, .001))
    int.len <- quantile(X, prob = probs[, "high"]) -
      quantile(X, prob = probs[, "low"])
    shortest <- min(int.len)
    first <- which(int.len == shortest)[1]
    hpd <- quantile(X, prob = probs[first, ], type = 7)
    # see ?quantile for type
    return(hpd)
}

resampleHier <- function(dat, cluster) {

  # exit early for trivial data
  if (nrow(dat) == 1){
    return(dat)
  }

  # sample the clustering factor
  cls <- sample(unique(dat[[cluster[1]]]), replace = TRUE)

  # subset on the sampled clustering factors
  sub <- lapply(cls, function(b) subset(dat, dat[[cluster[1]]] == b))

  # sample lower levels of hierarchy (if any)
  # recursive call to resample
  if (length(cluster) > 1){
    sub <- lapply(sub, resampleHier, cluster = cluster[-1])
  }

  # join and return samples
  rbind.fill(sub)
}
