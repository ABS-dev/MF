#' @title  MFhBoot
#' @description MFh using bootstrapping
#' @export
#' @examples 
#' set.seed(76153)
#' a <- data_frame(room = paste('Room', rep(c('W','Z'), each = 24)),
#'                 pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
#'                 litter = paste('Litter', rep(11:22, each = 4)),
#'                 tx = rep(rep(c('vac', 'con'), each = 2), 12)) %>%
#'   mutate(lung = ifelse(tx == 'vac', rnorm(24, 5, 1.3), rnorm(24, 7, 1.3)))
#' a
#' 
#' formula <- lung ~ tx + room/pen/litter
#' nboot <- 10000
#' boot.cluster <- TRUE
#' boot.unit <- TRUE
#' which.factors <- c('All', 'room', 'pen', 'litter')
#' set.seed(12345)
#'
#' system.time(test1 <- MFhBoot(formula, a, 
#'                             nboot = 10000,
#'                              boot.cluster = TRUE, boot.unit = TRUE))
#' test1$bootdat
MFhBoot <- function(formula, data,
                    compare = c("con", "vac"),
                    nboot = 10000, alpha = 0.05, 
                    boot.unit = TRUE, boot.cluster = TRUE){
  
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]

  
  symresp <- sym(resp)
  symtgroup <- sym(tgroup)
  wx <- sym(paste0(c(compare[1], "w"), collapse = '_'))
  wy <- sym(paste0(c(compare[2], "w"), collapse = '_'))
  nx <- sym(str_c(compare[1], "n", sep = "_"))
  ny <- sym(str_c(compare[2], "n", sep = '_'))
  mednm <- compare
  names(mednm) <- paste0("median_resp:", compare, sep = '')
  
  
  ## assign an ID to each unique core node
  indivclus <- data %>%
    mutate_if(is.factor, as.character) %>%
    select(nests) %>%
    distinct() %>% 
    mutate(clusterID = 1:n())
  nclus <- nrow(indivclus)
  
  datID <- data %>% 
    full_join(indivclus, by = nests)
  
  
  ## **Sample to create the new trees**
  ## 
  ## Bootstrapping means that some trees have all the cores
  ##     while others only have a subset. 
  ## We assume that the input data has the max possible number
  ##     of unique cores.     
  newdf <- tibble(bootID = rep(1:nboot, each = nclus),
                  newClus = case_when(isTRUE(boot.cluster) ~ 
                                        sample(indivclus$clusterID,
                                               nboot * nclus, replace = TRUE),
                                      !isTRUE(boot.cluster) ~ rep(indivclus$clusterID,
                                                               nboot))) %>%
    arrange(bootID)
  
  ## ** use new tree structure to calculate summary statistics **
  ## For each possible possible node figure out the set of w, u, & n1n2 statistics.
  ## 
  ## nx is the number of observations in the control or reference group for a  node. 
  ## ny is the number of observations in the comparison group for a node.
  ##
  ## w is the sum of the rankings of observations from the control 
  ##    or reference group where observations are ranked within the entire node. This will
  ##    change with the sampling that a occurs when isTRUE(boot.unit), although the interval of possible values does not change.
  ##    Note that depending on which bootstrap incidence, this maynot be a complete w for a unique core node.
  ##    
  ## u = w - nx(nx + 1)/2. The value on the rhs of minus is constant regardless of boot.unit. As the 
  ##    value of "w" changes due to sampling when isTRUE(boot.unit), so will "u" by the same amount.
  ##    Note that for bootstrap cases where a unique core node is included > 1x, the value
  ##    of u is being calculated for each instance, separately (as above for w). 
  ## 
  ## 
  ## 
  if(boot.unit){
    strat.b <- matrix(newdf$newClus, nboot)
    w <- u <- n1n2 <- medResp1 <- medResp2 <- matrix(NA, nboot, nclus)
    n.each <- rep(NA, nclus)
    names(n.each) <- indivclus$clusterID
    w.boot <- function(x, y, n.b){
      n.x <- length(x)
      n.y <- length(y)
      out <- rep(NA, n.b)
      x.b <- matrix(switch(as.character(n.x == 1),
                           'TRUE' = rep(x, n.b),
                           'FALSE' = sample(x, size = n.b * n.x, replace = T)), n.b, n.x)
      y.b <- matrix(switch(as.character(n.y == 1),
                           'TRUE' = rep(y, n.b),
                           'FALSE' = sample(y, size = n.b * n.y, replace = T)), n.b, n.y)
      w <- apply(cbind(x.b, y.b), 1, function(x, n.x)
        sum(rank(x)[1:n.x]), n.x)
      return(list(w, x.b, y.b))
    }
    
    lapply(1:nclus, FUN = function(a){
      x <- datID %>% 
        filter(!!symtgroup == compare[1] & clusterID == a) %>%
        select(!!symresp) %>% 
        as_vector() %>%
        unname()
    
      y <- datID %>% 
        filter(tx == compare[2] & clusterID == a) %>%
        select(lung) %>%
        as_vector() %>%
        unname()
      
      n.x <- length(x)
      n.y <- length(y)
      
      n.each[a] <<- sum(strat.b == a)
      thiswboot <- w.boot(x, y, n.each[a])
      w[strat.b == a] <<- thiswboot[[1]]
      u[strat.b == a] <<- w[strat.b == a] - (n.x * (n.x + 1))/2
      n1n2[strat.b == a] <<- n.x * n.y
      medResp1[strat.b == a] <<- median(x, na.rm = TRUE)
      medResp2[strat.b == a] <<- median(y, na.rm = TRUE)
      return(NULL)
    
    })
    
    newnames <- c('medResp1', 'medResp2')
    names(newnames) <- paste(compare, 'medResp', sep = '_')
    budat <- newdf %>% 
      mutate(w = as.vector(t(w)),
             u = as.vector(t(u)),
             n1n2 = as.vector((t(n1n2))),
             medResp1 = as.vector(t(medResp1)),
             medResp2 = as.vector(t(medResp2))) %>%
      rename(!!newnames) %>%
      full_join(indivclus, by = c('newClus' = 'clusterID')) %>%
      ungroup() %>%
      select(-newClus)
    

    
  } else {

    budat <- full_join(data, indivclus, by = nests) %>%
      group_by(clusterID) %>%
      mutate(rank = rank(!!symresp)) %>%
      group_by_at(vars('clusterID', tgroup)) %>%
      summarize(w = sum(rank),
                n = length(lung),
                medResp = median(lung, na.rm = TRUE)) %>%
      gather(variable, value, -c(clusterID, !!symtgroup)) %>%
      unite(tmp, tgroup, variable) %>%
      spread(tmp, value) %>%
      rename(w = !!wx) %>%
      mutate(u = w - (!!nx * (!!nx + 1))/2,
             n1n2 = !!nx * !!ny) %>%
      select(-!!wy) %>%
      full_join(newdf, by = c('clusterID' = 'newClus')) %>%
      arrange(bootID) %>%
      select(bootID, everything()) %>%
      full_join(indivclus, by = 'clusterID') %>%
      ungroup() %>%
      select(-clusterID)
  }
  return(list(bootdat = budat, clusters = indivclus, compare = compare, mfh = MFh(formula, data, compare)))
}

#' @title MFnestBoot
#' @description MFnest using bootstrapping
#' @export
#' @examples 
#' set.seed(76153)
#' a <- data_frame(room = paste('Room', rep(c('W','Z'), each = 24)),
#'                 pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
#'                 litter = paste('Litter', rep(11:22, each = 4)),
#'                 tx = rep(rep(c('vac', 'con'), each = 2), 12)) %>%
#'   mutate(lung = ifelse(tx == 'vac', rnorm(24, 5, 1.3), rnorm(24, 7, 1.3)))
#' a
#' 
#' formula <- lung ~ tx + room/pen/litter
#' nboot <- 10000
#' boot.cluster <- TRUE
#' boot.unit <- TRUE
#' which.factors <- c('All', 'room', 'pen', 'litter')
#' 
#' #################
#' set.seed(12345)
#' 
#' test1 <- MFhBoot(formula, a, 
#'                  nboot = 10000,
#'                  boot.cluster = TRUE, boot.unit = TRUE)
#' MFnestBoot(test1, c('All', 'litter'))
#' 
#' \dontrun{
#' system.time(test2 <- MFnestBoot(test1, which.factors))
#' test2
#' system.time(test3 <- MFnestBoot(test1, which.factors[1]))
#' test3
#' system.time(test4 <- MFnestBoot(test1, which.factors[2]))
#' test4
#' system.time(test5 <- MFnestBoot(test1, which.factors[2:3]))
#' test5
#' system.time(test6 <- MFnestBoot(test1, which.factors[2:4]))
#' test6
#' }
MFnestBoot <- function(x, which.factor = 'All', alpha = 0.05){
  
  quant <- c(.5, alpha / 2, 1 - alpha / 2)
  
  tmpall <- x$bootdat %>%
    select(-ends_with('_medResp'))

  mfnest <- bind_rows(tmpall %>%
                        gather(variable, level, -c('bootID', 'w', 'u', 'n1n2')) ,
                      tmpall %>%
                        select(bootID, w, u, n1n2) %>%
                        mutate(variable = 'All', level = 'All')) %>%
    filter(variable %in% which.factor) %>%
    group_by(variable, level, bootID) %>%
    summarize(U = sum(u),
              N = sum(n1n2),
              MF = 2 * (U/N) - 1) %>%
    group_by(variable, level) %>%
    summarize(etmedian = quantile(MF, prob = quant[1]),
              etlower = quantile(MF, prob = quant[2]),
              etupper = quantile(MF, prob = quant[3]),
              htlower = MF:::emp.hpd(MF, alpha = alpha)[1],
              htupper = MF:::emp.hpd(MF, alpha = alpha)[2]) %>%
    full_join(MFnest(x$mfh, which.factor = which.factor) %>%
                select(variable, level, MF) %>%
                rename(mf.obs = 'MF') %>%
                mutate_if(is.factor, as.character), by = c('variable', 'level')) %>%
    ungroup() %>%
    mutate(variable = fct_relevel(variable, which.factor)) %>%
    arrange(variable)

  mfnest
}
