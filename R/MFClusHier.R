require(MF)
require(CVBmisc)

# data
a <- data.frame(
  room = paste('Room',rep(c('W','Z'),each=24)),
  pen = paste('Pen',rep(LETTERS[1:6],each=8)),
  litter = paste('Litter',rep(11:22,each=4)),
  tx = rep(rep(c('vac','con'),each=2),12)
)
set.seed(76153)
a$lung[a$tx=='vac'] <- rnorm(24,5,1.3)
a$lung[a$tx=='con'] <- rnorm(24,7,1.3)

# function - now it just returns the core table - all it needs are the ranks
MFh <- function(formula, data, compare = c("con", "vac"), trace.it = FALSE){
  termlab <- attr(terms(formula), 'term.labels')
  tgroup <- termlab[1]
  xname <- compare[1]
  yname <- compare[2]
  nests <- unlist(strsplit(termlab[[length(termlab)]],split=':'))
  core <- nests[length(nests)]
  # Error checking?
  # need unique id at all levels
  # one way of doing that is concatenation
  newdat <- data[,nests]
  names(newdat) <- paste(names(newdat),'ID',sep='')
  for(i in ncol(newdat):2){
    newdat[,i]   <- apply(newdat[,1:i], 1, paste, collapse=' ')
  }
  # rank within core level
  coreID <- newdat[,ncol(newdat)]
  newdat <- cbind(newdat, data[,nests])
  newdat$tgroup <- data[,tgroup]
  newdat$rank <- rep(NA, nrow(newdat))
  for(cID in unique(coreID))
    newdat$rank[coreID==cID] <- rank(data$lung[coreID==cID])
  # sum ranks for MF at each level
  nestID <- paste(nests, 'ID', sep='')
  
  coreLevels <- unique(coreID)
  allLevels <- UniqueRows(cbind(newdat[,nests],coreID))
  coreTbl <- data.frame(
    matrix(NA, nrow=length(coreLevels), ncol=5, dimnames=list(coreLevels, c('nx','ny','N','w','u')))
  )
  for(lev in coreLevels){
    coreTbl[lev,'nx'] <- length(newdat$rank[coreID==lev & newdat$tgroup==xname])
    coreTbl[lev, 'ny'] <- length(newdat$rank[coreID==lev & newdat$tgroup==yname])
    coreTbl[lev, 'w'] <- sum(newdat$rank[coreID==lev & newdat$tgroup==xname])
  }
  coreTbl$N <-coreTbl$nx * coreTbl$ny
  coreTbl$u <- coreTbl$w - (coreTbl$nx * (coreTbl$nx + 1))/2
  
  cat('\n', nests[length(nests)],'\n')
  coreTbl <- cbind(allLevels, coreTbl)
  
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

aCore <- MFh(lung ~ tx + room/pen/litter,a)

MFnest(aCore)
MFnest(aCore, 'room')
MFnest(aCore, 'pen')
MFnest(aCore, 'litter')

# > MFnest(aCore)
#   level  N  U    MF
# 1   All 48 45 0.875

# > MFnest(aCore, 'room')
#    level  N  U        MF
# 1 Room W 24 22 0.8333333
# 2 Room Z 24 23 0.9166667

# > MFnest(aCore, 'pen')
#   level N U   MF
# 1 Pen A 8 6 0.50
# 2 Pen B 8 8 1.00
# 3 Pen C 8 8 1.00
# 4 Pen D 8 7 0.75
# 5 Pen E 8 8 1.00
# 6 Pen F 8 8 1.00

# > MFnest(aCore, 'litter')
#        level N U  MF
# 1  Litter 11 4 4 1.0
# 2  Litter 12 4 2 0.0
# 3  Litter 13 4 4 1.0
# 4  Litter 14 4 4 1.0
# 5  Litter 15 4 4 1.0
# 6  Litter 16 4 4 1.0
# 7  Litter 17 4 4 1.0
# 8  Litter 18 4 3 0.5
# 9  Litter 19 4 4 1.0
# 10 Litter 20 4 4 1.0
# 11 Litter 21 4 4 1.0
# 12 Litter 22 4 4 1.0
