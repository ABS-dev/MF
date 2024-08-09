#' Estimates the subject components of the mitigated fraction.
#'
#' The mitigated fraction is an estimator that quantifies an intervention's
#' effect on reducing the severity of a condition. Since its units are on the
#' probability scale, it is often a good idea to accompany it with an estimator
#' on the original scale of measurement.
#'
#' The subject components are the individual contributions of the treated
#' subjects to *MF*, which is the average of the subject components.
#'
#' @title Subject components of mitigated fraction
#' @param formula Formula of the form `y ~ x`, where y is a continuous response
#'   and x is a factor with two levels
#' @param data Data frame
#' @param vac_grp The name of the vaccinated group.
#' @param con_grp The name of the control group.
#' @param compare `r badge("deprecated")` Text vector stating the factor levels:
#'   `compare[1]` is the control or reference group to which `compare[2]`
#'   (vaccinate) is compared
#' @returns a [mfcomponents-class] data object
#' @references Siev D. (2005). An estimator of intervention effect on disease
#'   severity. *Journal of Modern Applied Statistical Methods.*
#'   **4:500--508**
#' @author [MF-package]
#' @examples
#' x <- MFSubj(lesion ~ group, calflung)
#' x
#'
#' mean(x$subj[, "mf_j"])
#'
#' @importFrom stats model.frame
#' @importFrom lifecycle badge deprecate_warn is_present deprecated
#' @export
MFSubj <- function(formula,
                   data,
                   vac_grp = "vac",
                   con_grp = "con",
                   compare = deprecated()) {
  if (is_present(compare)) {
    deprecate_warn(
      "4.5.0",
      "MFSubj(compare)",
      details = "Please use the `vac_grp` and `con_grp` argumetns instead.")
    if (length(compare) != 2) {
      stop("`compare` must be a vector of length 2!")
    }
    vac_grp <- compare[2]
    con_grp <- compare[1]
  }
  # formula of form response ~ treatment
  # x = response for con_grp
  # y = response for vac_grp
  # compare y to x

  df <- data.frame(model.frame(formula = formula, data = data))
  resp <- df[, 1]
  tx <- df[, 2]
  x <- resp[tx == con_grp]
  y <- resp[tx == vac_grp]
  n_x <- length(x)
  n_y <- length(y)
  nn <- n_x + n_y
  x_y <- c(x, y)
  rank_xy <- rank(x_y)
  w <- sum(rank_xy[1:n_x])
  mf <- ((2. * w - n_x * (1. + n_x + n_y)) / (n_x * n_y))
  u_j <- rep(NA, n_y)
  for (j in 1:n_y) {
    u_j[j] <- mean(c(sum(y[j] < x), sum(y[j] <= x)))
  }
  r_j <- u_j / n_x
  mf_j <- 2 * r_j - 1
  subj <- cbind(y, rank = rank_xy[(n_x + 1):nn], u_j, r_j, mf_j)
  subj <- subj[order(subj[, "rank"]), ]
  return(mfcomponents$new(mf = mf, x = sort(x), y = sort(y), subj = subj,
                          vac_grp = vac_grp, con_grp = con_grp))
}
