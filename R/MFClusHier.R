#' @name MFh
#' @title Identify ranks for use when evaluating MF for nested hierarchy.
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous 
#' response, x is a factor with two levels of treatment, and a/b/c are variables 
#' corresponding to the clusters. It is expected that levels of "c" are nested within 
#' levels of "b". Nesting is assumed to be in order, left to right, highest to lowest.
#' @param data a data.frame with the variables specified in formula. Additional variables will
#' be ignored.
#' @param compare Text vector stating the factor levels - compare[1] is the control or 
#' reference group to which compare[2] is compared.
#' @return a data.frame with one row for each unique core level showing values for
#' \code{nx}, \code{ny}, \code{N}, \code{w}, and \code{u}
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core.
#' @seealso \code{\link{MFnest}} for calculation of MF for nest, core and all variables.
#' @examples 
#' a <- data.frame(
#'  room = paste('Room',rep(c('W','Z'),each=24)),
#'  pen = paste('Pen',rep(LETTERS[1:6],each=8)),
#'  litter = paste('Litter',rep(11:22,each=4)),
#'  tx = rep(rep(c('vac','con'),each=2),12)
#'  )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24,5,1.3)
#' a$lung[a$tx=='con'] <- rnorm(24,7,1.3)
#' 
#' aCore <- MFh(lung ~ tx + room/pen/litter,a)
# function - now it just returns the core table - all it needs are the ranks
#' @export
MFh <- function(formula, data, compare = c("con", "vac")){
  ## get all variables from formula & identify role
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  core <- nests[length(nests)]
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]
  
  
  ## groups for comparison
  xname <- compare[1]
  yname <- compare[2]
  
  ## create new data.frame with all nest variables to be unique across entire dataset.
  ##    these are ID forms for the nest variables.
  ##
  ## combine with orig data.frame excluding response
  ## row order is preserved.
  ##
  ## left-to-right variable order of final data.frame (newdat):
  ##    nest variables in ID form (unique), same order as formula
  ##    nest variables in original data form (not necessarily unique), same order as formula
  ##    compare variable 
  ##    response variable in last position
  newdat <- data[, nests]
  nestID <- paste(nests, "ID", sep = "")
  names(newdat) <- nestID
  for (i in ncol(newdat):2) {
    newdat[, i] <- apply(newdat[, 1:i], 1, paste, collapse = " ")
  }
  newdat <- cbind(newdat, data[, nests])
  newdat$tgroup <- data[, tgroup]
  newdat$resp <- data[, resp]
  
  ## for navigating core variable
  coreID <- newdat[, length(nests)]
  coreLevels <- unique(coreID)
  coreIDname <- names(newdat)[length(nests)]
  
  ## rank response for each unique core level
  for (cID in coreLevels) {
    newdat[newdat[, coreIDname] == cID, "rank"] <- 
      rank(newdat[newdat[, coreIDname] == cID, "resp"])
  }
  
  ## sum ranks for MF for each unique coreID for internal summaries
  coreTbl <- ddply(newdat, coreIDname, .fun = function(x) {
    data.frame(nx = length(x[x$tgroup == xname, "rank"]), 
               ny = length(x[x$tgroup == yname, "rank"]), 
               w = sum(x[x$tgroup == xname, "rank"]))
  })
  coreTbl$N <- coreTbl$nx * coreTbl$ny
  coreTbl$u <- coreTbl$w - (coreTbl$nx * (coreTbl$nx + 1))/2

  coreTbl <- merge(unique(newdat[, c(nests, coreIDname)]), coreTbl, by = coreIDname)
  names(coreTbl)[1] <- paste("Core:", nests[length(nests)])
  
  return(coreTbl)
}

#' @name MFnest
#' @title Summations to calculate the MF for nested data from a rank table.
#' @param Y rank table, as output from \code{\link{MFh}}.
#' @param which.factor one or more variable(s) of interest. This can be any of the core or nest variables
#' from the data set. If none or \code{NULL} is specified, MF will be calculated for the whole 
#' tree.
#' @return data.frame with each unique level of a variable as a row. Values \code{N}, 
#' \code{U}, and \code{MF} returned.
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core. All refers to a summary of the entire tree.
#' @seealso \code{\link{MFh}}
#' @examples
#' MFnest(aCore)
#' #   variable level  N  U    MF
#' # 1      All   All 48 45 0.875
#' 
#' MFnest(aCore, 'room')
#' #  variable  level  N  U        MF
#' # 1     room Room W 24 22 0.8333333
#' # 2     room Room Z 24 23 0.9166667
#' 
#' MFnest(aCore, 'pen')
#' #   variable level N U   MF
#' # 1      pen Pen A 8 6 0.50
#' # 2      pen Pen B 8 8 1.00
#' # 3      pen Pen C 8 8 1.00
#' # 4      pen Pen D 8 7 0.75
#' # 5      pen Pen E 8 8 1.00
#' # 6      pen Pen F 8 8 1.00
#'  
#' MFnest(aCore, 'litter')
#' #    variable     level N U  MF
#' # 1    litter Litter 11 4 4 1.0
#' # 2    litter Litter 12 4 2 0.0
#' # 3    litter Litter 13 4 4 1.0
#' # 4    litter Litter 14 4 4 1.0
#' # 5    litter Litter 15 4 4 1.0
#' # 6    litter Litter 16 4 4 1.0
#' # 7    litter Litter 17 4 4 1.0
#' # 8    litter Litter 18 4 3 0.5
#' # 9    litter Litter 19 4 4 1.0
#' # 10   litter Litter 20 4 4 1.0
#' # 11   litter Litter 21 4 4 1.0
#' # 12   litter Litter 22 4 4 1.0
#' 
#' MFnest(aCore, c('room', 'pen', 'litter'))
#' #    variable     level  N  U        MF
#' # 1      room    Room W 24 22 0.8333333
#' # 2      room    Room Z 24 23 0.9166667
#' # 3       pen     Pen A  8  6 0.5000000
#' # 4       pen     Pen B  8  8 1.0000000
#' # 5       pen     Pen C  8  8 1.0000000
#' # 6       pen     Pen D  8  7 0.7500000
#' # 7       pen     Pen E  8  8 1.0000000
#' # 8       pen     Pen F  8  8 1.0000000
#' # 9    litter Litter 11  4  4 1.0000000
#' # 10   litter Litter 12  4  2 0.0000000
#' # 11   litter Litter 13  4  4 1.0000000
#' # 12   litter Litter 14  4  4 1.0000000
#' # 13   litter Litter 15  4  4 1.0000000
#' # 14   litter Litter 16  4  4 1.0000000
#' # 15   litter Litter 17  4  4 1.0000000
#' # 16   litter Litter 18  4  3 0.5000000
#' # 17   litter Litter 19  4  4 1.0000000
#' # 18   litter Litter 20  4  4 1.0000000
#' # 19   litter Litter 21  4  4 1.0000000
#' # 20   litter Litter 22  4  4 1.0000000
#' @export
MFnest <- function(Y, which.factor = NULL) {
  ## if no factor specified, look at "All"
  if(is.null(which.factor)){
    which.factor <- 'All'
  }
  if ("all" %in% tolower(which.factor)) {
    Y <- cbind(All = rep("All", nrow(Y)), Y)
  }
  
  ## evaluate N, U and MF for each variable specified in which.factor
  rbind.fill(lapply(which.factor, FUN = function(x){
    ## get the design matrix
    X <- sapply(as.character(unique(Y[, x])), FUN = function(a){
      as.numeric(Y[, x] == a)
    })
    out <- data.frame(variable = x, level = unique(Y[, x]))
    out$N <- t(X) %*% Y$N
    out$U <- t(X) %*% Y$u
    R <- out$U/out$N
    out$MF <- 2 * R - 1
    return(out)
  }))

}
