#' @importFrom methods new setClassUnion setRefClass


setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))
setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("numericORarray", c("numeric", "array"))
setClassUnion("numericORarrayORtable", c("numeric", "array", "table"))
setClassUnion("numericORinteger", c("numeric", "integer"))

#' @name mf-class
#' @title Class mf
#' @description Parent class for package MF data objects.
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{`nboot`}{numeric value specifying number of samples}
#'   \item{`alpha`}{numeric value specifying complement of confidence interval}
#'   \item{`seed`}{vector of integers specifying seed for pseudo-random
#'   number generator used}
#'   \item{`compare`}{vector of character strings naming groups compared}
#'   \item{`rng`}{character string naming type of random number generator}
#'   }
#' @keywords documentation
#' @family mf
#' @author [MF-package]
mf <- setRefClass("mf", fields = list(nboot = "numeric",
                                      alpha = "numeric",
                                      seed = "numericORinteger",
                                      compare = "character",
                                      rng = "character"))

#' @name mfboot-class
#' @title Class mfboot
#' @description class for data objects produced by MFBoot, contains class mf
#'   with the two additional fields *stat* and *stuff*.
#' @docType class
#' @section Fields:
#'   \describe{
#'   \item{`nboot`}{numeric value specifying number of samples}
#'   \item{`alpha`}{numeric value specifying complement of confidence interval}
#'   \item{`seed`}{vector of integers specifying seed for pseudo-random
#'   number generator used}
#'   \item{`compare`}{vector of character strings naming groups compared}
#'   \item{`rng`}{character string naming type of random number generator}
#'   \item{`sample`}{ what is this?}
#'   \item{`stat`}{matrix of estimates}
#' }
#' @section Contains: [mf-class]
#' @keywords documentation
#' @family mf
#' @seealso [MFBoot]
#' @author [MF-package]
mfboot <- setRefClass("mfboot", contains = "mf",
                      fields = list(stat = "matrix",
                                    sample = "numericORNULL"))

#' @name mfhlboot-class
#' @title Class mfhlboot
#' @description class for data objects produced by HLBoot, contains class mf
#'   with additional fields *MFstat*, *HLstat*, *QDIFstat*, *QXstat*, *QYstat*
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{`nboot`}{Numeric value specifying number of samples.}
#'   \item{`alpha`}{Numeric value specifying complement of confidence interval.}
#'   \item{`seed`}{Vector of integers specifying seed for pseudo-random number
#'   generator used.}
#'   \item{`compare`}{Vector of character strings naming groups compared.}
#'   \item{`rng`}{Character string naming type of random number generator.}
#'   \item{`sample`}{The bootstrapped values.}
#'   \item{`MFstat`}{Matrix with columns *observed*, *median*, *lower*, *upper*
#'   for Equal Tailed and Highest Density estimates of mitigated fraction (MF).}
#'   \item{`HLstat`}{Matrix with columns *observed*, *median*, *lower*, *upper*
#'   for Equal Tailed and Highest Density estimates of Hodge-Lehmann estimator
#'    (HL).}
#'   \item{`QDIFstat`}{Matrix with columns *observed*, *median*, *lower*,
#'   *upper* for estimates of Quartile Differences.}
#'   \item{`QXstat`}{Matrix with columns *observed*, *median*, *lower*,
#'   *upper* for quartiles of treatments, equal tailed.}
#'   \item{`QYstat`}{Matrix with columns *observed*, *median*, *lower*,
#'   *upper* for quartiles of response, equal tailed.}
#' }
#' @section Contains: [mf-class]
#' @keywords documentation
#' @family mf
#' @seealso [HLBoot]
#' @author [MF-package]
mfhlboot <- setRefClass("mfhlboot", contains = "mf",
                        fields = list(MFstat = "matrix",
                                      HLstat = "matrix",
                                      QDIFstat = "matrix",
                                      QXstat = "matrix",
                                      QYstat = "matrix",
                                      sample = "listORNULL"))

#' @name mfmp-class
#' @title Class mfmp
#' @description Class `mfmp` is created from output of function `MFmp`
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{`ci`}{numeric vector of point and interval estimates}
#'   \item{`x` }{numeric vector of length three holding data}
#'   \item{`what`}{text string describing interval type}
#'   \item{`alpha`}{numeric value specifying complement of confidence interval}
#'   \item{`tdist`}{Logical indicating if t distribution(TRUE) or gaussian
#'    (FALSE)}
#'   \item{`df`}{numeric value indicating degrees freedom}
#' }
#' @keywords documentation
#' @family mfmp
#' @author [MF-package]
#' @seealso [MFmp]
mfmp <- setRefClass("mfmp",
                    fields = list(ci = "numeric",
                                  x = "numericORarrayORtable",
                                  what = "character",
                                  alpha = "numeric",
                                  tdist = "logical",
                                  df = "numeric"))

#' @name mfbootcluster-class
#' @title Class mfbootcluster
#' @description Class mfbootcluster is created from output of function
#'   MFClusBoot
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{`nboot`}{numeric value specifying number of samples}
#'   \item{`alpha`}{numeric value specifying complement of confidence interval}
#'   \item{`seed`}{vector of integers specifying seed for pseudo-random number
#'   generator used}
#'   \item{`compare`}{vector of character strings naming groups compared}
#'   \item{`rng`}{character string naming type of random number generator}
#'   \item{`stat`}{matrix matrix with columns *observed*, *median*, *lower*,
#'   *upper* for estimates}
#'   \item{`what`}{character vector naming what was resampled: *clusters*,
#'   *units*, *both*}
#'   \item{`excludedClusters`}{character vector naming clusters excluded
#'   because of missing treatment(s)}
#'   \item{`call`}{the call to `MFClusBoot`}
#'   \item{`sample`}{what is this?}
#'   \item{`All`}{Field "All" from MFClus call.}
#'   }
#' @section Contains: [mf-class]
#' @keywords documentation
#' @family mf
#' @seealso [MFClusBoot]
#' @author [MF-package]
mfbootcluster <- setRefClass("mfbootcluster", contains = "mf",
                             fields = list(stat = "matrix",
                                           what = "character",
                                           excludedClusters = "character",
                                           call = "call",
                                           sample = "numericORNULL",
                                           All = "data.frame"))

#' @name mfcluster-class
#' @title Class mfcluster
#' @description Class mfcluster is created from output of function MFClus
#' @docType class
#' @section Fields:
#'  \describe{
#'   \item{`All`}{vector with elements:
#'   \describe{
#'     \item{`w`}{Wilcoxon statistic}
#'     \item{`u`}{Mann-Whitney statistic}
#'     \item{`r`}{mean ridit}
#'     \item{`n1`}{size of group 1}
#'     \item{`n2`}{size of group 2}
#'     \item{`mf`}{mitigated fraction}
#'   }}
#'   \item{`byCluster`}{As for All, by clusters}
#'   \item{`excludedClusters`}{character vector naming clusters excluded
#'   because of missing treatment}
#'   \item{`call`}{the call to `MFClus`}
#'   \item{`compare`}{character vector naming groups compared}
#' }
#'
#' @keywords documentation
#' @family mfcluster
#' @seealso [MFClus]
#' @author [MF-package]
mfcluster <- setRefClass("mfcluster",
                         fields = list(All = "data.frame",
                                       byCluster = "matrix",
                                       excludedClusters = "characterORNULL",
                                       call = "call",
                                       compare = "character"))

#' @name mfcomponents-class
#' @title Class mfcomponents
#' @description Class mfcomponents is created from output of function MFSubj
#' @docType class
#' @section Fields:
#' \describe{
#' \item{`mf`}{numeric estimator for mitigated fraction}
#' \item{`x`}{numeric vector containing responses of group 1}
#' \item{`y`}{numeric vector containing responses of group 2}
#' \item{`subj`}{matrix where `mf_j`re the subject components}
#' \item{`compare`}{character vector naming groups being compared}
#' }
#' @keywords documentation
#' @family mfcomponents
#' @seealso [MFSubj]
#' @author [MF-package]
mfcomponents <- setRefClass("mfcomponents",
                            fields = list(mf = "numeric",
                                          x = "numeric",
                                          y = "numeric",
                                          subj = "matrix",
                                          compare = "character"))

#' @name mfhierdata-class
#' @title Class mfhierdata
#' @description Class mfhierdata is created from output of function MFh
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{`coreTbl`}{data.frame with one row for each unique core level
#'   showing values for `nx``ny``N``w``u`and
#'   median observed response.}
#'   \item{`data`}{data.frame is the restructured input data used for
#'   calculations in MFh and MFnest.}
#'   \item{`compare`}{character vector naming groups being compared.}
#'   \item{`formula`}{formula that was called by user.}
#' }
#' @keywords documentation
#' @family mfhierdata
#' @seealso [MFh]
#' @author [MF-package]
mfhierdata <- setRefClass("mfhierdata",
                          fields = list(coreTbl = "tbl",
                                        data = "tbl",
                                        compare = "character",
                                        formula = "formula"))


#' @name mfclushier-class
#' @title Class mfclushier
#' @description Class mfclushier is created from output of function MFClusHier
#' @docType class
#' @section Fields:
#' \describe{
#' \item{`MFh`}{output from MFh. A [mfhierdata] object.}
#' \item{`MFnest`}{output from MFnest. A tibble.}
#' }
#' @keywords documentation
#' @family mfclushier
#' @seealso [MFh], [MFnest]
#' @author [MF-package]
mfclushier <- setRefClass("mfclushier", fields = list(MFh = "mfhierdata",
                                                      MFnest = "tbl"))

#' @name mfclusboothier-class
#' @title Class mfclusboothier
#' @description Class mfclusboothier is created from output of function
#' MFClusBootHier.
#' @docType class
#' @section Fields:
#' \describe{
#' \item{`MFhBoot`}{output from MFhBoot. A list.}
#' \item{`MFnestBoot`}{output from MFnestBoot. A list.}
#' }
#' @keywords documentation
#' @family mfclusboothier
#' @seealso [MFhBoot], [MFnestBoot]
#' @author [MF-package]
mfclusboothier <- setRefClass("mfclusboothier",
                              fields = list(MFhBoot = "list",
                                            MFnestBoot = "list"))
