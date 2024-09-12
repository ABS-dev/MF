
#' @description Estimates mitigated fraction from matched pairs.
#' @details Estimates *MF* from matched pairs by the difference of multinomial
#'   fractions \eqn{(\Sigma I(x<y) - \Sigma I(x > y)) / N}. The trinomial vector
#'   is \eqn{\{\Sigma I(x<y), \Sigma I(x = y), \Sigma I(x > y)\}}
#' @title Mitigated fraction from matched pairs
#' @param formula Formula of the form `y ~ x + cluster(w)`, where y is a
#'   continuous response, x is a factor with two levels of treatment, and w is a
#'   factor indicating the clusters.
#' @param data Data frame
#' @param compare Text vector stating the factor levels - `compare[1]` is the
#'   control or reference group to which `compare[2]` is compared
#' @param x Trinomial vector \eqn{\{\Sigma I(x < y), \Sigma I(x = y), \Sigma
#'   I(x > y)\}}
#' @param alpha Complement of the confidence level.
#' @param df Degrees of freedom. Default N-2
#' @param tdist Use quantiles of t or Gaussian distribution for confidence
#'   interval? Default t distribution.
#' @export
#' @note upper confidence interval is truncated to 1; lower confidence interval
#'   is truncated to -1. Point estimate of 1.0 indicates complete separation.
#' @returns a [mfmp-class] data object
#' @seealso [mfmp-class]
#' @references Siev D. (2005). An estimator of intervention effect on disease
#'   severity. *Journal of Modern Applied Statistical Methods.*
#'   **4:500--508**
#' @author [MF-package]
#' @examples
#' MFmp(les ~ tx + cluster(cage), mlesions, compare = c("con", "vac"))
#' MFmp(x = c(12, 12, 2))
#' @importFrom stats qnorm qt
MFmp <- function(formula = NULL, data = NULL, compare = c("con", "vac"),
                 x = NULL, alpha = 0.05, df = NA, tdist = TRUE) {
  # asymptotic CI for matched pairs
  # x is a trinomial frequency vector
  # difference of multinomial fractions

  if (!is.null(formula) && !is.null(data)) {
    byCluster <- MFClus(formula = formula, data = data,
                        compare = compare)$byCluster[, "mf"]
    ##
    byCluster <- factor(byCluster)
    levels(byCluster) <- c("-1", "1", "0")
    x <- table(byCluster)[c("1", "0", "-1")]

  } else if (is.null(x)) {
    stop("Need to supply either formula and data or x = vector")
  }

  N <- sum(x)
  p <- x / N
  V <- (diag(p) - t(t(p)) %*% t(p)) / N
  V
  A <- c(1, 0, -1)
  B <- t(A) %*% p
  VB <- t(A) %*% V %*% A

  if (tdist && is.na(df)) {
    df <- N - 2
  }

  if (!is.na(df)) {
    q <- qt(c(0.5, alpha / 2, 1 - alpha / 2), df)
    what <- paste(100 * (1 - alpha), "% t intervals on ", df, " df\n", sep = "")
  } else {
    q <- qnorm(c(0.5, alpha / 2, 1 - alpha / 2))
    what <- paste(100 * (1 - alpha), "% gaussian interval\n", sep = "")
  }

  ci <- as.numeric(B) + q * as.numeric(sqrt(VB))
  names(ci) <- c("point", "lower", "upper")

  if (round(ci[["point"]], digits = 1) == 1.0) {
    message("Complete separation observed")
  }
  # truncate
  ci["upper"] <- min(ci["upper"], 1)
  ci["lower"] <- max(ci["lower"], -1)


  return(mfmp$new(ci = ci, x = x, what = what, alpha = alpha, tdist = tdist,
                  df = df))
}
