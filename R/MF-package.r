#' @name calflung
#' @title calflung dataset
#' @aliases calflung-data
#' @docType data
#' @description Post-mortem examination of the lungs of groups of calves.
#' @format a data frame with 50 observations of the following 2 variables, no
#'   NAs
#'
#' * `group`: Treatment group. One of con = control or vac = vaccinate
#'
#' * `lesion`: Fraction of lungs with gross lesions.
#'
#' @keywords datasets
NULL


#' @name mlesions
#' @title mlesions dataset
#' @aliases mlesions-data
#' @docType data
#' @description Post-mortem examination of the lungs of dogs housed in cages by
#'   pairs.
#' @format A data frame with 52 observations of the following 3 variables, no
#'   NAs.
#'
#' * `cage`: Cage ID. 1 - 26.
#'
#' * `tx`: Treatment. One of "con" or "vac".
#'
#' * `les`: Percent gross lung lesions.
#'
#' @keywords datasets
NULL

#' @name piglung
#' @title piglung dataset
#' @aliases piglung-data
#' @docType data
#' @description Post-mortem examination of the lungs of pigs in litters.
#' @format A data frame with 102 observations of the following 3 variables, no
#'   NAs.
#'
#' * `lesion`: Percent gross lung lesions.
#'
#' * `group`: Treatment group. One of "con" or "vac".
#'
#' * `litter`: Litter ID.
#'
#' @keywords datasets
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  globalVariables(c("."))
