#' @description Estimates bootstrap confidence intervals for the mitigated
#'   fraction.
#' @details Resamples the data and produces bootstrap confidence intervals.
#'   Equal tailed intervals are estimated by the percentile method. Highest
#'   density intervals are estimated by selecting the shortest of all possible
#'   intervals. For BCa intervals, see Efron and Tibshirani section 14.3.
#' @title Bootstrap MF CI
#' @param formula Formula of the form `y ~ x`, where y is a continuous response
#'   and x is a factor with two levels.
#' @param data Data frame
#' @param vac_grp The name of the vaccinated group.
#' @param con_grp The name of the control group.
#' @param b Number of bootstrap samples to take with each cycle
#' @param B Number of cycles, giving the total number of samples = B * b
#' @param alpha Complement of the confidence level
#' @param hpd Estimate highest density intervals?
#' @param bca Estimate BCa intervals?
#' @param return.boot Save the bootstrap sample of the MF statistic?
#' @param trace.it Verbose tracking of the cycles?
#' @param seed to initialize random number generator for reproducibility. Passed
#'   to `set.seed`.
#' @param compare `r badge("deprecated")` Text vector stating the factor levels:
#'   `compare[1]` is the control or reference group to which `compare[2]`
#'   (vaccinate) is compared
#' @returns a [mfboot-class] data object
#' @seealso [mfboot-class]
#' @references Siev D. (2005). An estimator of intervention effect on disease
#'   severity. *Journal of Modern Applied Statistical Methods.*
#'   **4:500--508**
#'
#'   Efron B, Tibshirani RJ. *An Introduction to the Bootstrap.* Chapman and
#'   Hall, New York, 1993.
#' @author [MF-package]
#' @examples
#'
#' MFBoot(lesion ~ group, calflung, seed = 12345)
#'
#' @importFrom stats quantile median model.frame pnorm qnorm
#' @importFrom lifecycle badge deprecate_warn is_present deprecated
#' @export
MFBoot <- function(formula,
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
  if (is_present(compare)) {
    deprecate_warn(
      "4.5.0",
      "MFBoot(compare)",
      details = "Please use the `vac_grp` and `con_grp` argumetns instead.")
    if (length(compare) != 2) {
      stop("`compare` must be a vector of length 2!")
    }
    vac_grp <- compare[2]
    con_grp <- compare[1]
  }

  # set seed
  set.seed(seed)

  A <- data.frame(model.frame(formula = formula, data = data))
  resp <- A[, 1]
  tx <- A[, 2]
  x <- resp[tx %in% con_grp]
  y <- resp[tx %in% vac_grp]
  if (length(y) == 0) {
    stop("HFBoot :: No matches in data for `vac_grp` = '", vac_grp, "'.",
         call. = FALSE)
  }
  if (length(x) == 0) {
    stop("HFBoot :: No matches in data for `con_grp` = '", con_grp, "'.",
         call. = FALSE)
  }

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
  nboot <- b * B
  n_x <- length(x)
  n_y <- length(y)
  w <- function(xy, n_x) {
    sum(rank(xy)[1:n_x])
  }
  W <- rep(NA, b * B)
  for (i in 1:B) {
    x_b <- matrix(sample(x, size = b * n_x, replace = TRUE), b, n_x)
    y_b <- matrix(sample(y, size = b * n_y, replace = TRUE), b, n_y)
    W[(i - 1) * b + (1:b)] <- apply(cbind(x_b, y_b), 1, w, n_x)
    if (trace.it) {
      cat("bootstrap samples", (i - 1) * b + 1, "to",
          (i - 1) * b + b, "\n")
    }
  }
  MF <- (2 * W - n_x * (1 + n_x + n_y)) / (n_x * n_y)
  mf <- (2 * w(c(x, y), n_x) - n_x * (1 + n_x + n_y)) / (n_x * n_y)
  qprob <- c(0.5, alpha / 2, 1 - alpha / 2)
  qmf <- quantile(MF, prob = qprob)
  stat <- matrix(c(mf, qmf), 1, 4,
                 dimnames = list(c("Equal Tailed"),
                                 c("observed", "median", "lower", "upper")))
  if (hpd) {
    hpdmf <- emp_hpd(MF, alpha = alpha)
    stat <- rbind(stat, "Highest Density" = c(mf, median(MF), hpdmf))
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
    stat <- rbind(stat, "BC.a" = c(mf, qmf))
  }
  out <- mfboot$new(stat = stat, nboot = nboot, alpha = alpha, seed = seed,
                    rng = rng, vac_grp = vac_grp, con_grp = con_grp,
                    sample = MF)

  return(out)
}
