#' MF Package
#'
#' Includes functions related to mitigated fraction. \cr \cr For internal use only at
#' the USDA Center for Veterinary Biologics. \cr
#'
#' \tabular{ll}{
#' Package: \tab MF-package\cr
#' Type: \tab Package\cr
#' Version: \tab 4.3.5\cr
#' Date: \tab 21-Dec-2018\cr
#' License: \tab MIT \cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @name MF-package
#' @aliases MF
#' @docType package
#' @author David Siev \email{David.Siev@@aphis.usda.gov}
#' @examples
#' #---------------------------------------------
#' # Checking MF package
#' #---------------------------------------------
#' example(MFr)
#' #---------------------------------------------
#' # End examples
#' #---------------------------------------------
#' invisible()
NULL


#' @name calflung
#' @title calflung dataset
#' @aliases calflung-data
#' @docType data
#' @description Post-mortem examination of the lungs of groups of calves.
#' @format a data frame with 50 observations of the following 2 variables, no NAs
#' \describe{
#' \item{group}{Treatment group. One of con = control or vac = vaccinate }
#' \item{lesion}{Fraction of lungs with gross lesions.}
#' }
#' @keywords datasets
NULL


#' @name mlesions
#' @title mlesions dataset
#' @aliases mlesions-data
#' @docType data
#' @description Post-mortem examination of the lungs of dogs housed in cages by pairs. 
#' @format A data frame with 52 observations of the following 3 variables, no NAs.
#' \describe{
#' \item{cage}{Cage ID. 1 - 26.}
#' \item{tx}{Treatment. One of 'con' or 'vac'.}
#' \item{les}{Percent gross lung lesions.}
#' }
#' @keywords datasets
NULL

#' @name piglung
#' @title piglung dataset
#' @aliases piglung-data
#' @docType data
#' @description Post-mortem examination of the lungs of pigs in litters.
#' @format A data frame with 102 observations of the following 3 variables, no NAs.
#' \describe{
#' \item{lesion}{Percent gross lung lesions.}
#' \item{group}{Treatment group. One of 'con' or 'vac'.}
#' \item{litter}{Litter ID.}
#' }
#' @keywords datasets
NULL