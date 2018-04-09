#' @title Clustered mitigated fraction for nested experimental design.
#' @param formula Formula of the form \code{y ~ x + cluster(w1/w2/..)}, where y is a 
#' continuous response, x is a factor with two levels of treatment, and w1/w2/... are  
#' variables indicating the nested clusters.
#' @param data Data frame.  
#' @export
#' @references Siev D. (2005). An estimator of intervention effect on disease severity.
#' \emph{Journal of Modern Applied Statistical Methods.} \bold{4:500--508}
#' @author David Siev \email{david.siev@@aphis.usda.gov}
#' @seealso \code{\link{mfcluster-class}}, \code{\link{MFClus}}
MFClusNested <- function(formula, data, compare = c("con", "vac"), trace.it = FALSE){
  termslabs <- attr(terms(formula), 'term.labels')
  allvars <- all.vars(formula)
  
  ## data suitability
  if(length(termslabs) != 2){
    stop("Formula not structured appropriately. Must be of format: response ~ treatmentgroup + cluster(level1/level2/...)")
  } 
  
  # if(length(strsplit(termslabs[2], "/")[[1]]) == 1){
  #   stop("No nesting specified.")
  # }
  
  ## Recursive call to MFClusNested
  if(length(allvars) == 3){
    MFClus(formula, data, compare, trace.it)
  } else {
    resp <- with(attributes(terms(formula)), as.character(variables[response+1]))
    thisvar <- allvars[!allvars %in% c(resp, termslabs[1])][1]

    ## rewrite formula without top-level variable
    newform <- gsub(deparse(formula, width.cutoff = 500), pattern = paste(thisvar, '/', sep = ""), replacement = '')
    ## For each factor of the highest level, run MFClusNested on the subset data
    dlply(data, thisvar, .fun = function(x){
      message("Calling MFClusNested for ", thisvar, " ", unique(subset(x, select = thisvar)),
              ".\n", newform)
      ## remove the top-level variable from data
      out <- x[,thisvar != names(x)]
      MFClusNested(formula = formula(newform), data = out, compare = compare, 
                   trace.it = trace.it)
      
    })
  }
}