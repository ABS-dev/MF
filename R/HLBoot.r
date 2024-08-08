#' @description Estimates bootstrap confidence intervals for MF, HL, and Qdif.
#' @details Estimates bootstrap confidence intervals for the mitigated fraction
#'   (MF), Hodge-Lehmann estimator (HL), and the difference of medians and
#'   quartiles (Qdif). Equal tailed intervals are provided for all three,
#'   highest density intervals are optionally provided for MF and HL, and BCa
#'   intervals are optionally provided for MF.  The Hodges-Lehmann estimator is
#'   the median difference; it assumes that the two distributions have the same
#'   shape and differ by a constant shift. Assumes data is single pool (no
#'   nesting).
#' @title Bootstrap CI for MF, HL, and Qdif
#' @param formula Formula of the form `y ~ x + cluster(w)`, where y is a
#'   continuous response, x is a factor with two levels of treatment, and w is a
#'   factor indicating the clusters.
#' @param data Data frame
#' @param vac_grp The name of the vaccinated group.
#' @param con_grp The name of the control group.
#' @param b Number of bootstrap samples to take with each cycle
#' @param B Number of cycles, giving the total number of samples = B * b
#' @param alpha Complement of the confidence level
#' @param hpd Boolean whether to estimate highest density intervals for MF and
#'   HL.
#' @param bca Boolean whether to estimate BCa intervals for MF.
#' @param return.boot Boolean whether to save the bootstrap samples of the
#'   statistics.
#' @param trace.it Boolean whether to display verbose tracking of the cycles.
#' @param seed to initialize random number generator for reproducibility. Passed
#'   to `set.seed`.
#' @param compare `r badge("deprecated")` Text vector stating the factor levels:
#'   `compare[1]` is the control or reference group to which `compare[2]`
#'   (vaccinate) is compared
#' @returns a [mfhlboot-class] data object
#' @seealso [mfhlboot-class]
#' @references Hodges JL, Lehmann EL, (1963). Estimates of location based on
#'   rank tests. *Annals of Mathematical Statistics.* **34:598--611**.
#'
#'
#'   Siev D, (2005). An estimator of intervention effect on disease severity.
#'   *Journal of Modern Applied Statistical Methods.*
#'   **4:500--508**.
#'
#'   Efron B, Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman and
#'   Hall, New York, 1993.
#' @author [MF-package]
#' @examples
#' HLBoot(lesion ~ group, calflung, seed = 12345)
#'
#' @importFrom stats quantile median model.frame pnorm qnorm
#' @importFrom lifecycle badge deprecate_warn is_present deprecated
#' @export
HLBoot <- function(formula,
                   data,
                   vac_grp = "vac",
                   con_grp = "con",
                   b = 100,
                   B = 100,
                   alpha = 0.05,
                   hpd = TRUE,
                   bca = FALSE,
                   return.boot = FALSE,
                   trace.it = FALSE,
                   seed = sample(1:100000, 1),
                   compare = deprecated()) {
  # set seed
  set.seed(seed)

  if (is_present(compare)) {
    deprecate_warn("4.5.0",
                   "HLBoot(compare)",
                   "HLBoot(vac_grp, con_grp)")
    if (length(compare) != 2) {
      stop("`compare` must be a vector of length 2!")
    }
    vac_grp <- compare[2]
    con_grp <- compare[1]
  }

  # Wilcoxon rank sum statistic
  w <- function(xy, n_x) {
    sum(rank(xy)[1:n_x])
  }

  # Hodges-Lehmann estimator
  hl_fn <- function(xy, n_x) {
    x <- xy[1:n_x]
    y <- xy[(n_x + 1):length(xy)]
    n_y <- length(xy) - n_x
    X <- matrix(x, n_x, n_y, byrow = FALSE)
    Y <- matrix(y, n_x, n_y, byrow = TRUE)
    med.dif <- median(Y - X)
    return(med.dif)
  }

  A <- data.frame(model.frame(formula = formula, data = data))
  resp <- A[, 1]
  tx <- A[, 2]
  x <- resp[tx == con_grp]
  y <- resp[tx == vac_grp]

  # shortcircuit if complete separation
  if (range(x)[1] < range(y)[1]) {
    lowtx <- x
    hitx <- y
  } else {
    lowtx <- y
    hitx <- x
  }

  if (max(lowtx) < min(hitx)) {
    message("Complete separation observed. MF is 1.0 with no c.i.")
    return()
  }

  rng <- "Mersenne-Twister"
  RNGkind(rng)

  # takes b bootstrap samples B times, so nboot = B * b
  nboot <- b * B
  n_x <- length(x)
  n_y <- length(y)

  # observed stats
  mf <- (2 * w(c(x, y), n_x) - n_x * (1 + n_x + n_y)) / (n_x * n_y)

  if (round(mf, 1) == 1.0) {
    message("Complete separation observed.")
  }
  hl <- hl_fn(c(x, y), n_x)
  qx <- quantile(x, probs = c(1:3) / 4)
  qy <- quantile(y, probs = c(1:3) / 4)
  qdif <- qy - qx

  W <- H <- rep(NA, b * B)
  Qx <- Qy <- matrix(rep(NA, b * B * 3), b * B, 3,
                     dimnames = list(NULL, c("Q25", "Q50", "Q75")))
  cat("\nBootstrapping\n")
  for (i in 1:B) {
    x_b <- matrix(sample(x, size = b * n_x, replace = TRUE), b, n_x)
    y_b <- matrix(sample(y, size = b * n_y, replace = TRUE), b, n_y)
    Qx[(i - 1) * b + (1:b), ] <- t(apply(x_b, 1, quantile, probs = c(1:3) / 4))
    Qy[(i - 1) * b + (1:b), ] <- t(apply(y_b, 1, quantile, probs = c(1:3) / 4))
    W[(i - 1) * b + (1:b)] <- apply(cbind(x_b, y_b), 1, w, n_x)
    H[(i - 1) * b + (1:b)] <- apply(cbind(x_b, y_b), 1, hl_fn, n_x)
    if (trace.it) {
      cat("bootstrap samples", (i - 1) * b + 1, "to", (i - 1) * b + b, "\n")
    } else {
      cat(". ")
    }
  }
  cat("\n")
  Qdif <- Qy - Qx
  MF <- (2 * W - n_x * (1 + n_x + n_y)) / (n_x * n_y)
  qprob <- c(0.5, alpha / 2, 1 - alpha / 2)
  qmf <- quantile(MF, prob = qprob)
  mfstat <- matrix(c(mf, qmf), 1, 4,
                   dimnames = list(c("Equal Tailed"),
                                   c("observed", "median", "lower", "upper")))
  qhl <- quantile(H, prob = qprob)
  hlstat <- matrix(c(hl, qhl), 1, 4,
                   dimnames = list(c("Equal Tailed"),
                                   c("observed", "median", "lower", "upper")))
  qqd <- t(apply(Qdif, 2, quantile, prob = qprob))
  qdifstat <- cbind(qdif, qqd)
  dimnames(qdifstat) <- list(dimnames(Qdif)[[2]], c("observed", "median",
                                                    "lower", "upper"))
  qqx <- t(apply(Qx, 2, quantile, prob = qprob))
  qxstat <- cbind(qx, qqx)
  dimnames(qxstat) <- list(dimnames(Qx)[[2]], c("observed", "median",
                                                "lower", "upper"))
  qqy <- t(apply(Qy, 2, quantile, prob = qprob))
  qystat <- cbind(qy, qqy)
  dimnames(qystat) <- list(dimnames(Qx)[[2]], c("observed", "median",
                                                "lower", "upper"))
  if (hpd) {
    hpdmf <- emp_hpd(MF, alpha = alpha)
    mfstat <- rbind(mfstat, "Highest Density" = c(mf, median(MF), hpdmf))
    hpdhl <- emp_hpd(H, alpha = alpha)
    hlstat <- rbind(hlstat, "Highest Density" = c(hl, median(H), hpdhl))
  }

  if (bca) {
    z0 <- qnorm(sum(MF < mf) / (b * B))
    xy <- c(x, y)
    nx.i <- rep(n_x - (1:0), c(n_x, n_y))
    ny.i <- rep(n_y - (0:1), c(n_x, n_y))
    theta <- rep(NA, n_x + n_y)
    for (i in 1:(n_x + n_y)) {
      theta[i] <- (2 * w(xy[-i], nx.i[i]) - nx.i[i] *
                     (1 + nx.i[i] + ny.i[i])) / (nx.i[i] * ny.i[i])
    }
    theta_hat <- mean(theta)
    acc <- sum((theta_hat - theta)^3) / (6 * sum((theta_hat - theta)^2)^(3 / 2))
    z1 <- qnorm(alpha / 2)
    z2 <- qnorm(1 - alpha / 2)
    a1 <- pnorm(z0 + (z0 + z1) / (1 - acc * (z0 + z1)))
    a2 <- pnorm(z0 + (z0 + z2) / (1 - acc * (z0 + z2)))
    a5 <- pnorm(z0 + (z0 + 0) / (1 - acc * (z0 + 0)))
    qprob <- c(a5, a1, a2)
    stuff <- c(acc, z0, a1, a2)
    names(stuff) <- c("acc", "z0", "a1", "a2")
    if (trace.it) {
      print(round(stuff, 4))
    }
    qmf <- quantile(MF, prob = qprob)
    mfstat <- rbind(mfstat, "BC.a" = c(mf, qmf))
  }

  if (return.boot) {
    sample  <- list(MF = MF, HL = H, Qx = Qx, Qy = Qy)
  } else {
    sample <- NULL
  }

  return(mfhlboot$new(MFstat = mfstat, HLstat = hlstat, QDIFstat = qdifstat,
                      QXstat = qxstat, QYstat = qystat, nboot = nboot,
                      alpha = alpha, seed = seed, vac_grp = vac_grp,
                      con_grp = con_grp, rng = rng, sample = sample))
}
