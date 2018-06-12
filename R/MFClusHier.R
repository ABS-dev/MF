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
#' @return a list of three items. \emph{coreTbl} is a data.frame with one row for each unique core level showing values for
#' \code{nx}, \code{ny}, \code{N}, \code{w}, \code{u}, and median observed response. \emph{data} is the restructured input data used for calculations.
#' \emph{compare} is the compare variable as input by user.
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core.
#' @seealso \code{\link{MFnest}} for calculation of MF for nest, core and all variables.
#' @examples 
#' a <- data.frame(
#'  room = paste('Room',rep(c('W','Z'),each=24)),
#'  pen = paste('Pen',rep(LETTERS[1:6],each=8)),
#'  litter = paste('Litter',rep(11:22,each=4)),
#'  tx = rep(rep(c('vac','con'),each=2),12),
#'  stringsAsFactors = FALSE
#'  )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24,5,1.3)
#' a$lung[a$tx=='con'] <- rnorm(24,7,1.3)
#' 
#' aCore <- MFh(lung ~ tx + room/pen/litter,a)
#' aCore
# function - now it just returns the core table - all it needs are the ranks
#' @export
MFh <- function(formula, data, compare = c("con", "vac")){
  ## get all variables from formula & identify role
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  if(length(nests) == 1){
    stop("This is not nested hierarchy. See MFClus.")
  }
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
  # newdat <- data[, nests]
  # nestID <- paste(nests, "ID", sep = "")
  # names(newdat) <- nestID
  # for (i in ncol(newdat):2) {
  #   newdat[, i] <- apply(newdat[, 1:i], 1, paste, collapse = " ")
  # }
  # newdat <- cbind(newdat, data[, nests])
  # newdat$tgroup <- data[, tgroup]
  # newdat$resp <- data[, resp]
  
  ## for navigating core variable
  # coreID <- newdat[, length(nests)]
  # coreLevels <- unique(coreID)
  # coreIDname <- names(newdat)[length(nests)]
  
    
  ## remove clusters missing a treatment
  # excluded.clusters <- unlist(sapply(coreLevels, FUN = function(x){
  #   if(length(unique(newdat[newdat[, coreIDname] == x, "tgroup"])) == 1){
  #     return(x)
  #   }}))
  
  # if(length(excluded.clusters > 0)){
  #   message(paste("Excluded clusters:", paste(excluded.clusters, collapse = ',')))
  #   newdat <- newdat[newdat[, coreIDname] != excluded.clusters,]
  # }
  
  ## rank response for each unique core level
  # for (cID in coreLevels) {
  #   newdat[newdat[, coreIDname] == cID, "rank"] <- 
  #     rank(newdat[newdat[, coreIDname] == cID, "resp"])
  # }
  ## sum ranks for MF for each unique coreID for internal summaries
  
  # coreTbl <- ddply(newdat, coreIDname, .fun = function(x) {
  #   out <- data.frame(nx = length(x[x$tgroup == xname, "rank"]),
  #                     ny = length(x[x$tgroup == yname, "rank"]),
  #                     w = sum(x[x$tgroup == xname, "rank"]),
  #                     median(x[x$tgroup == xname, 'resp'], na.rm = TRUE),
  #                     median(x[x$tgroup == yname, 'resp'], na.rm = TRUE))
  #   names(out)[4:5] <- c(paste("median_resp:", xname, sep = ''),
  #                        paste("median_resp:", yname, sep = ''))
  #   out
  # })
  # 
  # coreTbl$N <- coreTbl$nx * coreTbl$ny
  # coreTbl$u <- coreTbl$w - (coreTbl$nx * (coreTbl$nx + 1))/2
  # 
  # coreTbl <- merge(unique(newdat[, c(nests, coreIDname)]), coreTbl, by = coreIDname)
  # names(coreTbl)[1] <- paste("Core:", nests[length(nests)], sep = "")
  nx <- sym(str_c(xname, "n", sep = "_"))
  ny <- sym(str_c(yname, "n", sep = '_'))
  wy <- sym(str_c(yname, "w", sep = "_"))
  wx <- sym(str_c(xname, "w", sep = "_"))
  
  newdat <- as_tibble(data) %>%
    select(everything(), tgroup = tgroup, resp = resp) 
  
  thiscoreTbl <- newdat %>%
    group_by_at(nests) %>%
    add_count(tgroup) %>%
    filter(n > 1) %>%
    select(-n) %>%
    mutate(rank = rank(resp)) %>%
    group_by_at(vars(nests, tgroup)) %>%
    summarize(n = length(resp),
              medResp = median(resp, na.rm = TRUE),
              w = sum(rank)) %>%
    gather(variable, value, -c(tgroup, nests)) %>%
    unite(temp, tgroup, variable) %>%
    spread(temp, value) %>%    
    select(-!!wy) %>%
    rename(w = !!wx) %>%
    mutate(N = !!nx * !!ny,
           u = w - (!!nx * (!!nx + 1))/2) %>%
    select(everything(), w, N, u) %>%
    ungroup()

  return(mfhierdata$new(coreTbl = thiscoreTbl, data = newdat, compare = compare))
}

#' @name MFnest
#' @title Summations to calculate the MF for nested data from a rank table.
#' @param Y rank table, structured as \code{$coreTbl} output from \code{\link{MFh}} or
#'  output list from MFh
#' @param which.factor one or more variable(s) of interest. This can be any of the core or nest variables
#' from the data set. If none or \code{NULL} is specified, MF will be calculated for the whole 
#' tree.
#' @return data.frame with each unique level of a variable as a row. Values \code{N}, 
#' \code{U}, \code{MF} and median of responses returned.
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core. All refers to a summary of the entire tree.
#' @seealso \code{\link{MFh}}
#' @examples
#' MFnest(aCore)
#' #   variable level  N  U    MF median_resp:con median_resp:vac
#' # 1      All   All 48 45 0.875        7.244466        4.909263
#' 
#' MFnest(aCore$coreTbl)
#' # Skipping median summary, no response data provided.
#' #   variable level  N  U    MF
#' # 1      All   All 48 45 0.875
#' 
#' MFnest(aCore, 'room')
#' #  variable  level  N  U        MF median_resp:con median_resp:vac
#' # 1     room Room W 24 22 0.8333333        7.792846        4.846574
#' # 2     room Room Z 24 23 0.9166667        6.708765        4.978941
#' 
#' MFnest(aCore, 'pen')
#' #   variable level N U   MF median_resp:con median_resp:vac
#' # 1      pen Pen A 8 6 0.50        6.792419        4.237541
#' # 2      pen Pen B 8 8 1.00        8.113648        5.594649
#' # 3      pen Pen C 8 8 1.00        7.688790        4.846574
#' # 4      pen Pen D 8 7 0.75        6.097081        4.978941
#' # 5      pen Pen E 8 8 1.00        6.858526        4.858916
#' # 6      pen Pen F 8 8 1.00        6.884332        5.134238
#'  
#' MFnest(aCore, 'litter')
#' #    variable     level N U  MF median_resp:con median_resp:vac
#' # 1    litter Litter 11 4 4 1.0        8.237194        5.125583
#' # 2    litter Litter 12 4 2 0.0        4.914294        3.808685
#' # 3    litter Litter 13 4 4 1.0        8.103441        5.227177
#' # 4    litter Litter 14 4 4 1.0        8.113648        5.594649
#' # 5    litter Litter 15 4 4 1.0        8.087914        5.256611
#' # 6    litter Litter 16 4 4 1.0        6.770457        4.503342
#' # 7    litter Litter 17 4 4 1.0        5.575649        4.258613
#' # 8    litter Litter 18 4 3 0.5        7.442926        6.329360
#' # 9    litter Litter 19 4 4 1.0        7.980120        4.584041
#' # 10   litter Litter 20 4 4 1.0        6.781480        4.858916
#' # 11   litter Litter 21 4 4 1.0        6.819483        5.363069
#' # 12   litter Litter 22 4 4 1.0        7.272879        5.134238
#' 
#' MFnest(aCore, c('room', 'pen', 'litter'))
#' #    variable     level  N  U        MF median_resp:con median_resp:vac
#' # 1      room    Room W 24 22 0.8333333        7.792846        4.846574
#' # 2      room    Room Z 24 23 0.9166667        6.708765        4.978941
#' # 3       pen     Pen A  8  6 0.5000000        6.792419        4.237541
#' # 4       pen     Pen B  8  8 1.0000000        8.113648        5.594649
#' # 5       pen     Pen C  8  8 1.0000000        7.688790        4.846574
#' # 6       pen     Pen D  8  7 0.7500000        6.097081        4.978941
#' # 7       pen     Pen E  8  8 1.0000000        6.858526        4.858916
#' # 8       pen     Pen F  8  8 1.0000000        6.884332        5.134238
#' # 9    litter Litter 11  4  4 1.0000000        8.237194        5.125583
#' # 10   litter Litter 12  4  2 0.0000000        4.914294        3.808685
#' # 11   litter Litter 13  4  4 1.0000000        8.103441        5.227177
#' # 12   litter Litter 14  4  4 1.0000000        8.113648        5.594649
#' # 13   litter Litter 15  4  4 1.0000000        8.087914        5.256611
#' # 14   litter Litter 16  4  4 1.0000000        6.770457        4.503342
#' # 15   litter Litter 17  4  4 1.0000000        5.575649        4.258613
#' # 16   litter Litter 18  4  3 0.5000000        7.442926        6.329360
#' # 17   litter Litter 19  4  4 1.0000000        7.980120        4.584041
#' # 18   litter Litter 20  4  4 1.0000000        6.781480        4.858916
#' # 19   litter Litter 21  4  4 1.0000000        6.819483        5.363069
#' # 20   litter Litter 22  4  4 1.0000000        7.272879        5.134238
#' @export
MFnest <- function(Y, which.factor = 'All') {
  ## restructure if using output from MFh
  if(class(Y) == 'mfhierdata'){
    input <- Y
    Y <- input$coreTbl
  } else if(class(Y) != 'tbl'){
    Y <- as_tibble(Y)
  }
  
  ## if no factor specified, look at "All"
  # if(is.null(which.factor)){
  #   which.factor <- 'All'
  # }
  ## create the All variable if it is a variable to be calculated
  # if ("all" %in% tolower(which.factor)) {
  #   Y <- cbind(All = rep("All", nrow(Y)), Y)
  # }
  stat.names <- c(str_subset(names(Y), "_medResp"), 
                  str_subset(names(Y), "_n"), 'N', 'u', 'w')
  out <- Y %>%
    gather(variable, level, -stat.names) %>%
    bind_rows(., 
              select(Y, stat.names) %>%
              mutate(variable = 'All', level = 'All')) %>%
    group_by(variable, level) %>%
    summarize(N = sum(N), U = sum(u)) %>%
    mutate(R = U/N, MF = 2 * R - 1) %>%
    select(-R) %>%
    filter(tolower(variable) %in% tolower(which.factor)) %>%
    ungroup()

  ## inform user of complete separation
  ## TODO: this message might need to go away for bootstrap calls to this function.
  if(1.0 %in% round(out$MF, digits = 1) ){
    out %>%
    filter(round(MF, digits = 1) == 1.0) %>%
    distinct(variable) %>%
    pull() %>%
    paste0(collapse = ', ') %>%
    message("Complete separation observed for variable(s): ", ., collapse = "")
  }
  
  ## inform user why medians are not available
  ## TODO: this message might need to go away for bootstrap calls to this function.
  if(!exists('input')){
    message('Skipping median summary, no response data provided.') 
  } else{
    thisdata <- input$data
    compare <- input$compare
    names(input$compare) <- paste0("median_resp:", input$compare, sep = '')

    out <- thisdata %>%
      gather(variable, level, -c(tgroup, resp)) %>%
      bind_rows(.,
                select(thisdata, c(tgroup, resp)) %>%
                mutate(variable = "All", level = "All")) %>%
      group_by(variable, level, tgroup) %>%
      summarize(median_resp = median(resp, na.rm = TRUE)) %>%
      spread(tgroup, median_resp) %>%
      rename(!!compare) %>%
      filter(tolower(variable) %in% tolower(which.factor)) %>%
      left_join(out,.) %>%
      ungroup()
  }    
 
  ## evaluate N, U and MF for each variable specified in which.factor
  # plyr::rbind.fill(lapply(which.factor, FUN = function(x){

    ## get the design matrix
    # Y <- as.data.frame(Y)
    # X <- sapply(as.character(unique(Y[, x])), FUN = function(a){
    #   as.numeric(Y[, x] == a)
    # })
    
    ## calculations for MF, N, U
    # out <- data.frame(variable = x, level = unique(Y[, x]))
    # out$N <- t(X) %*% Y$N
    # out$U <- t(X) %*% Y$u
    # R <- out$U/out$N
    # out$MF <- 2 * R - 1
    # if(exists('input')){
    #   comparex <- input$compare[1]
    #   comparey <- input$compare[2]
    #   if(x == 'All'){
    #     out$medianx <- median(input$data[input$data$tgroup == comparex, 'resp'],
    #                           na.rm = TRUE)
    #     out$mediany <- median(input$data[input$data$tgroup == comparey, 'resp'],
    #                           na.rm = TRUE)
    #   } else{
    #     # thismedians <- plyr::ddply(input$data, x, .fun = function(y){
    #     #   return(data.frame(medianx = median(y[y$tgroup == comparex, 'resp'], na.rm = TRUE),
    #     #                     mediany = median(y[y$tgroup == comparey, 'resp'], na.rm = TRUE)))})
    #     # names(thismedians)[1] <- 'level'
    #     # thismedians$variable <- x
    #     # out <- merge(out, thismedians, by = c('variable', 'level'))
    #   }
    # 
    #   # names(out)[6] <- paste("median_resp:", as.character(comparex), sep = '')
    #   # names(out)[7] <- paste("median_resp:", as.character(comparey), sep = '')
    #   
    # }
# 
    # if(1.0 %in% round(out$MF, digits = 1) ){
    #   message("Complete separation for variable ", x, " observed.")
    # }
    return(out)
#   }))

}
