#' Mitigated fraction comparing treatment to control.
#'
#' The mitigated fraction is an estimator that quantifies an intervention's
#' effect on reducing the severity of a condition. Since its units are on the
#' probability scale, it is often a good idea to accompany it with an estimator
#' on the original scale of measurement.
#'
#' @title Mitigated fraction
#' @param formula Formula of the form \code{y ~ x}, where y is a continuous
#'   response and x is a factor with two levels
#' @param data Data frame
#' @param compare Text vector stating the factor levels -- \code{compare[1]} is
#'   the control or reference group to which \code{compare[2]} is compared
#' @return The estimated mitigated fraction.
#' @export
#' @references Siev D, 2005. An estimator of intervention effect on disease
#'   severity. \emph{Journal of Modern Applied Statistical Methods.} 4:500-508
#' @author \link{MF-package}
#' @examples
#' MFr(lesion~group, calflung)
#' # [1] 0.44
#' @importFrom stats model.frame
MFr <- function(formula, data, compare = c("con", "vac")) {
  # formula of form response~treatment
  # x=response for compare[1]
  # y=response for compare[2]
  # compare y to x
  df <- data.frame(model.frame(formula = formula, data = data))
  resp <- df[, 1]
  tx <- df[, 2]
  x <- resp[tx == compare[1]]
  y <- resp[tx == compare[2]]
  n_x <- length(x)
  n_y <- length(y)
  # unused? N <- n_x + n_y
  x_y <- c(x, y)
  w <- sum(rank(x_y)[seq_len(n_x)])
  return(((2. * w - n_x * (1. + n_x + n_y)) / (n_x * n_y)))
}
