#' @title MFnestBoot 
#' @description Bootstrapping MF estimate for nested heirarchy
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous 
#' response, x is a factor with two levels of treatment, and a/b/c are variables 
#' corresponding to the clusters. It is expected that levels of "c" are nested within 
#' levels of "b". Nesting is assumed to be in order, left to right, highest to lowest.
#' @param data a data.frame or tibble with the variables specified in formula. 
#' Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - \code{compare[1]} is the control 
#' or reference group to which \code{compare[2]} is compared.
#' @param boot.cluster Boolean whether to resample the clusters
#' @param boot.unit Boolean whether to resample the units within each cluster
#' @param b Number of bootstrap samples to take with each cycle
#' @param B Number of cycles, giving the total number of samples = B * b
#' @param compare Text vector stating the factor levels - compare[1] is the control or 
#' reference group to which compare[2] is compared.
#' @param alpha complement of the confidence level
#' @param hpd boolean whether to estimate highest density intervals
#' @note B * b notation is used for consistency with MFClus
#' @author CVB Statistics \email{CVB.Data.Help@@aphis.usda.gov}
#' @seealso \code{\link{MFClus}}
#' @export
#' @examples 
#' a <- as_tibble(
#'  room = paste('Room',rep(c('W','Z'),each=24)),
#'  pen = paste('Pen',rep(LETTERS[1:6],each=8)),
#'  litter = paste('Litter',rep(11:22,each=4)),
#'  tx = rep(rep(c('vac','con'),each=2),12)
#'  )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24,5,1.3)
#' a$lung[a$tx=='con'] <- rnorm(24,7,1.3)
#' MFnestBoot(lung ~ tx + room/pen/litter, a)
#' MFnestBoot(lung ~ tx + room/pen/litter, a, boot.unit = FALSE)
#' MFnestBoot(lung ~ tx + room/pen/litter, a, which.factor = 'litter')
#' MFnestBoot(lung ~ tx + room/pen/litter, a, n.boot = 2)
#' MFnestBoot(lung ~ tx + room/pen/litter, a, n.boot = 144, boot.cluster = FALSE)
#'
MFnestBoot <- function(formula, data, compare = c("con", "vac"),
                       B = 100, b = 100, alpha = 0.05, hpd = TRUE,
                       boot.unit = TRUE, boot.cluster = TRUE){
  
  ## get all variables from formula & identify role
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  core <- nests[length(nests)]
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]
  uniquelev <-   unique(data[, nests])
  rownames(uniquelev) <- 1:nrow(uniquelev)
  uniquelevID <- data.table(uniquelev)
  uniquelevID$clusterID <- 1:nrow(uniquelevID)
  newdata <- merge(data.table(data), uniquelevID, by = nests, all = TRUE)

  # ## check that boot.levels are reasonable.
  # ## adjust for default of "All"
  # if(tolower(boot.levels) == 'all'){
  #   boot.levels <- c(nests, termlab[1])
  # } else if(!termlab[1] %in% boot.levels){
  #   message('Must bootstrap treatment groups. Adding ', termlab[1], ' to boot.levels.')
  #   boot.levels <- c(boot.levels, termlab[1])
  # }
  # ## cluster variables not included in bootstrap
  # notboot.levels <- allclusvar[!allclusvar %in% boot.levels]
  
  # 
  # ## abandon this approach
  # lapply(1:(b * B), FUN = function(x){
  #   if(boot.cluster){
  #     thisboot <- sample(1:nrow(uniquelev), size = nrow(data), replace = TRUE)
  #     newout <- uniquelev[thisboot,]
  #     rownames(newout) <- 1:nrow(newout)
  #     newout
  #   } else {
  #     newout <- data[,which(names(data) != resp)]
  #     newout
  #   }
  #   
  # })
  
  
  ##### 
  
  
  ################ helper functions
  
  ## calculate w, u, n1n2 values when boot.unit == FALSE
  get_w_noBootUnit <- function(dt){
    thisdata <- newdata[clusterID %in% unique(dt$clusterID),]
    x <- thisdata[get(tgroup) == compare[1], get(resp)]
    n.x <- length(x)
    y <- thisdata[get(tgroup) == compare[2], get(resp)]
    n.y <- length(y)
    x.y <- c(x, y)
    w <- sum(rank(x.y)[1:n.x])
    u <- w - (n.x * (n.x + 1))/2
    n1n2 <- n.x * n.y
    return(list(w = w, u = u, n1n2 = n1n2))
  }
  
  ## calculate stat summary when boot.unit == FALSE
  get_stat_noBootUnit <- function(dt){
    W = sum(dt$w)
    U = sum(dt$u)
    N1N2 = sum(dt$n1n2)
    R = U/N1N2
    return(list(W = W, U = U, N1N2 = N1N2, R = R, MF = 2 * R - 1))
  }
  
  ########################### end helper functions
  
  
  ## bootstrapping of clusters, if needed.
  if(boot.cluster){
    clusterID <- unlist(lapply(1:B, FUN = function(x){
      sample(1:nrow(uniquelev), size = b * nrow(uniquelev), replace = T)}))
  } else {
    clusterID <- 1:nrow(uniquelev)
  }


  strat.b <- merge(data.table(B = rep(1:B, each = b),
                    b = rep(1:b, B),
                    colID = rep(1:nrow(uniquelev), each = b * B),
                    clusterID = clusterID),
               uniquelevID, 
               by = 'clusterID', all = TRUE)[order(B, b)]

  
  ## bootstrapping of observations, if needed
  if(!boot.unit){
    intstats <- strat.b[, get_w_noBootUnit(.SD), by = nests, .SDcols = names(strat.b)]
    newstrat.b <- merge(strat.b, intstats, by = nests)
    detailedstats <- newstrat.b[,get_stat_noBootUnit(.SD), by = .(B, b), .SDcols = names(newstrat.b)]
  }
  
  
  mfnest_call <- MFnest(MFh(formula = formula, data = data, compare = compare), 
                        which.factor = "All")
  q <- c(.5,alpha/2, 1 - alpha/2)
  stat <- matrix(c(Observed = mfnest_call$MF, 
                   quantile(detailedstats$MF, prob = q)), 1, 4, 
                 dimnames = list(c('Equal Tailed'), 
                                 c('observed', 'median', 'lower', 'upper')))
  if(hpd){
    hpdmf <- MF:::emp.hpd(detailedstats$MF, alpha = alpha)
    stat <- rbind(stat, 'Highest Density' = c(mfnest_call$MF, stat[1, 'median'], hpdmf))
  }
  return(list(MFnest = mfnest_call,  n.boot = B * b, call = match.call(), 
              compare = compare, stat = stat, strat.b = newstrat.b, intstats = intstats))
}
