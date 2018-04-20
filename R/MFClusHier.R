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
  
  cat("\n", nests[length(nests)], "\n")
  coreTbl <- merge(unique(newdat[, c(nests, coreIDname)]), coreTbl, by = coreIDname)
  
  return(coreTbl)
}
# end function


#-------------------------------
dMat <- function(x){
  # simple design matrix from a vector
  a <- NULL
  u <- unique(x)
  for(i in 1:length(u))
    a <- cbind(a, as.numeric(x == u[i]))
  dimnames(a) <- list(NULL, as.character(u))
  return(a)
}

#' @name MFnest
#' @title Calculate the MF for nested data from a rank table.
#' @param Y rank table, as output from \code{\link{MFh}}.
#' @param which.factor variable name of interest. This can be any of the core or nest variables
#' from the data set. If none or \code{NULL} is specified, MF will be calculated for the whole 
#' tree.
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core. All refers to a summary of the entire tree.
#' @examples
#' 
#' MFnest(aCore)
#' MFnest(aCore, 'room')
#' MFnest(aCore, 'pen')
#' MFnest(aCore, 'litter')
#' MFnest(aCore)
#' #   level  N  U    MF
#' # 1   All 48 45 0.875
#' 
#' MFnest(aCore, 'room')
#' #    level  N  U        MF
#' # 1 Room W 24 22 0.8333333
#' # 2 Room Z 24 23 0.9166667
#' 
#' MFnest(aCore, 'pen')
#' #   level N U   MF
#' # 1 Pen A 8 6 0.50
#' # 2 Pen B 8 8 1.00
#' # 3 Pen C 8 8 1.00
#' # 4 Pen D 8 7 0.75
#' # 5 Pen E 8 8 1.00
#' # 6 Pen F 8 8 1.00
#' 
#' MFnest(aCore, 'litter')
#' #        level N U  MF
#' # 1  Litter 11 4 4 1.0
#' # 2  Litter 12 4 2 0.0
#' # 3  Litter 13 4 4 1.0
#' # 4  Litter 14 4 4 1.0
#' # 5  Litter 15 4 4 1.0
#' # 6  Litter 16 4 4 1.0
#' # 7  Litter 17 4 4 1.0
#' # 8  Litter 18 4 3 0.5
#' # 9  Litter 19 4 4 1.0
#' # 10 Litter 20 4 4 1.0
#' # 11 Litter 21 4 4 1.0
#' # 12 Litter 22 4 4 1.0
#' @export
# this does the summations on the core table
MFnest <- function(Y, which.factor=NULL){
  if(is.null(which.factor)){
    Y <- cbind(All=rep('All', nrow(Y)), Y)
    which.factor <- 'All'
  }
  X <- dMat(Y[,which.factor])
  out <- data.frame(level=unique(Y[,which.factor]))
  out$N <- t(X) %*% Y$N
  out$U <- t(X) %*% Y$u
  R <- out$U / out$N
  out$MF <- 2 * R - 1
  return(out)
}
