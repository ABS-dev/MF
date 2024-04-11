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
#' @param compare Text vector stating the factor levels - `compare[1]` is
#'   the control or reference group to which `compare[2]` is compared
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
#'   to \code{set.seed}.
#' @return a \code{\link{mfhlboot-class}} data object
#' @seealso \code{\link{mfhlboot-class}}
#' @references Hodges JL, Lehmann EL, (1963). Estimates of location based on
#'   rank tests. **Annals of Mathematical Statistics.** *34:598--611*.
#'   \cr \cr Siev D, (2005). An estimator of intervention effect on disease
#'   severity. **Journal of Modern Applied Statistical Methods.**
#'   \bold{4:500--508}. \cr \cr Efron B, Tibshirani RJ. **An Introduction to
#'   the Bootstrap.** Chapman and Hall, New York, 1993.
#' @author \link{MF-package}
#' @examples
#' HLBoot(lesion~group, calflung, seed = 12345)
#'
#' # Bootstrapping
#' # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
#' # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
#' # . . . . . . . . . . . . . . . . . . . . . . . .
#' #
#' # 10000 bootstrap samples
#' # 95% confidence intervals
#' # Comparing vac to con
#' #
#' #
#' # Mitigated Fraction
#' #
#' # observed median lower  upper
#' # Equal Tailed        0.44 0.4496 0.152 0.7088
#' # Highest Density     0.44 0.4496 0.152 0.7088
#' #
#' #
#' # Hodges-Lehmann
#' #
#' # observed   median    lower       upper
#' # Equal Tailed    -0.07335 -0.07615 -0.17220 -0.01565000
#' # Highest Density -0.07335 -0.07615 -0.15635 -0.00850065
#' #
#' #
#' # Quartile Differences (quartiles of vac - quartiles of con)
#' #
#' # observed    median    lower     upper
#' # Q25 -0.041500 -0.041500 -0.10340 -0.000905
#' # Q50 -0.112525 -0.111175 -0.28115  0.019350
#' # Q75 -0.168000 -0.170425 -0.38890  0.005300
#' #
#' #
#' # Quartiles of con
#' # observed   median    lower   upper
#' # Q25 0.054000 0.054000 0.021005 0.11275
#' # Q50 0.139275 0.139275 0.061400 0.31000
#' # Q75 0.315000 0.315000 0.173000 0.44625
#' #
#' #
#' # Quartiles of vac
#' # observed  median   lower    upper
#' # Q25  0.01250 0.01250 0.00125 0.026000
#' # Q50  0.02675 0.02675 0.01665 0.144575
#' # Q75  0.14700 0.14700 0.02810 0.219250
#' @importFrom stats quantile median model.frame pnorm qnorm
#' @export

#--------------------------------------------------------------------
# Bootstrap HL, quartiles, quartile diffs
#--------------------------------------------------------------------
#
HLBoot <- function(formula, data, compare = c("con", "vac"), b = 100, B = 100,
                   alpha = 0.05, hpd = TRUE, bca = FALSE, return.boot = FALSE,
                   trace.it = FALSE, seed = sample(1:100000, 1)) {
  # set seed
  set.seed(seed)

  # Wilcoxon rank sum statistic
  w <- function(xy, n.x) {
    sum(rank(xy)[1:n.x])
  }

  # Hodges-Lehmann estimator
  hl.fn <- function(xy, n.x) {
    x <- xy[1:n.x]
    y <- xy[(n.x + 1):length(xy)]
    n.y <- length(xy) - n.x
    X <- matrix(x, n.x, n.y, byrow = FALSE)
    Y <- matrix(y, n.x, n.y, byrow = TRUE)
    med.dif <- median(Y - X)
    return(med.dif)
  }

  A <- data.frame(model.frame(formula = formula, data = data))
  resp <- A[, 1]
  tx <- A[, 2]
  x <- resp[tx == compare[1]]
  y <- resp[tx == compare[2]]

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
  n.x <- length(x)
  n.y <- length(y)

  # observed stats
  mf <- (2 * w(c(x, y), n.x) - n.x * (1 + n.x + n.y)) / (n.x * n.y)

  if (round(mf, 1) == 1.0) {
    message("Complete separation observed.")
  }
  hl <- hl.fn(c(x, y), n.x)
  qx <- quantile(x, probs = c(1:3) / 4)
  qy <- quantile(y, probs = c(1:3) / 4)
  qdif <- qy - qx

  W <- H <- rep(NA, b * B)
  Qx <- Qy <- matrix(rep(NA, b * B * 3), b * B, 3,
                     dimnames = list(NULL, c("Q25", "Q50", "Q75")))
  cat("\nBootstrapping\n")
  for (i in 1:B) {
    x.b <- matrix(sample(x, size = b * n.x, replace = TRUE), b, n.x)
    y.b <- matrix(sample(y, size = b * n.y, replace = TRUE), b, n.y)
    Qx[(i - 1) * b + (1:b), ] <- t(apply(x.b, 1, quantile, probs = c(1:3) / 4))
    Qy[(i - 1) * b + (1:b), ] <- t(apply(y.b, 1, quantile, probs = c(1:3) / 4))
    W[(i - 1) * b + (1:b)] <- apply(cbind(x.b, y.b), 1, w, n.x)
    H[(i - 1) * b + (1:b)] <- apply(cbind(x.b, y.b), 1, hl.fn, n.x)
    if (trace.it) {
      cat("bootstrap samples", (i - 1) * b + 1, "to", (i - 1) * b + b, "\n")
    } else {
      cat(". ")
    }
  }
  cat("\n")
  Qdif <- Qy - Qx
  MF <- (2 * W - n.x * (1 + n.x + n.y)) / (n.x * n.y)
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
    nx.i <- rep(n.x - (1:0), c(n.x, n.y))
    ny.i <- rep(n.y - (0:1), c(n.x, n.y))
    theta <- rep(NA, n.x + n.y)
    for (i in 1:(n.x + n.y)) {
      theta[i] <- (2 * w(xy[-i], nx.i[i]) - nx.i[i] *
                     (1 + nx.i[i] + ny.i[i])) / (nx.i[i] * ny.i[i])
    }
    theta.hat <- mean(theta)
    acc <- sum((theta.hat - theta)^3) / (6 * sum((theta.hat - theta)^2)^(3 / 2))
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
                      alpha = alpha, seed = seed, compare = compare,
                      rng = rng, sample = sample))
}
