#' @title  MFhBoot
#' @name MFhBoot
#' @description Calculate rank tables for MF using bootstrapping.
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous response,
#' x is a factor with two levels of treatment, and a/b/c are grouping variables
#' corresponding to the clusters. Nesting is assumed to be in order, left to right,
#' highest to lowest. So a single level of "a" will contain multiple levels of
#' "b" and a single level of "b" will contain multiple levels of "c".
#' @param data a data.frame or tibble with the variables specified in formula.
#' Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - compare[1] is the control
#' or reference group to which compare[2] is compared.
#' @param nboot number of bootstrapping events
#' @param boot.unit Boolean whether to sample observations from within those of the same core.
#' @param boot.cluster Boolean whether to sample which cores are present. If TRUE,
#' some trees have all the cores while others only have a subset.
#' @param seed to initialize random number generator for reproducibility. Passed to \code{set.seed}.
#' @return A list with the following elements: \cr \cr
#' \describe{
#'   \item{bootmfh}{Rank table for the bootstrapped values as output from
#'   \code{\link{MFh}}. Includes a new \code{bootID} variable to distinguish
#'   each bootstrapped incidence.}
#'   \item{clusters}{Table of unique nodes with an ID.}
#'   \item{compare}{Compare vector as specified by user.}
#'   \item{mfh}{MFh run on original data input.}
#' }
#' @seealso \code{\link{MFClusBootHier}}, \code{\link{MFnestBoot}}
#' @author \link{MF-package}
#' @export
#' @examples
#' set.seed(76153)
#' a <- data.frame(room = paste('Room', rep(c('W','Z'), each = 24)),
#'                 pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
#'                 litter = paste('Litter', rep(11:22, each = 4)),
#'                 tx = rep(rep(c('vac', 'con'), each = 2), 12),
#'                 stringsAsFactors = FALSE)
#' a[a$tx == 'vac', 'lung'] <-  rnorm(24, 5, 1.3)
#' a[a$tx == 'con', 'lung'] <- rnorm(24, 7, 1.3)
#' a
#'
#' formula <- lung ~ tx + room/pen/litter
#' nboot <- 10000
#' boot.cluster <- TRUE
#' boot.unit <- TRUE
#' which.factors <- c('All', 'room', 'pen', 'litter')
#'
#' system.time(test1 <- MFhBoot(formula, a,
#'                             nboot = 10000,
#'                              boot.cluster = TRUE, boot.unit = TRUE, seed = 12345))
#' test1$bootmfh
MFhBoot <- function(formula, data,
                    compare = c("con", "vac"),
                    nboot = 10000,
                    boot.unit = TRUE, boot.cluster = TRUE,
                    seed = sample(1:100000, 1)){
  ## set seed
  set.seed(seed)

  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]

  # create symbols for later access
  symresp <- sym(resp)
  symtgroup <- sym(tgroup)
  wx <- sym(paste0(c(compare[1], "w"), collapse = "_"))
  wy <- sym(paste0(c(compare[2], "w"), collapse = "_"))
  nx <- sym(str_c(compare[1], "n", sep = "_"))
  ny <- sym(str_c(compare[2], "n", sep = "_"))
  mednm <- compare
  names(mednm) <- paste0("median_resp:", compare, sep = "")


  # assign an ID to each unique core node
  indivclus <- data %>%
    mutate_if(is.factor, as.character) %>%
    select(nests) %>%
    distinct() %>%
    mutate(clusterID = 1:n())
  nclus <- nrow(indivclus)

  datID <- data %>%
    full_join(indivclus, by = nests)


  # **Sample to create the new trees**
  #
  # Bootstrapping means that some trees have all the cores
  #     while others only have a subset.
  # We assume that the input data has the max possible number
  #     of unique cores.
  newdf <- tibble(bootID = rep(1:nboot, each = nclus),
                  newClus = case_when(isTRUE(boot.cluster) ~
                    sample(indivclus$clusterID, nboot * nclus, replace = TRUE),
                    !isTRUE(boot.cluster) ~ rep(indivclus$clusterID,
                                                       nboot))) %>%
    arrange(bootID)

  # ** use new tree structure to calculate summary statistics **
  # For each possible possible node figure out the set of w, u, & n1n2
  #    statistics.
  #
  # nx is the number of observations in the control or reference group for a
  #     node.
  # ny is the number of observations in the comparison group for a node.
  #
  # w is the sum of the rankings of observations from the control
  #    or reference group where observations are ranked within the entire node.
  #    This will change with the sampling that a occurs when
  #    isTRUE(boot.unit), although the interval of possible values does not
  #    change.
  #
  #    Note that depending on which bootstrap incidence, this may not be a
  #    complete w for a unique core node.
  #
  # u = w - nx(nx + 1)/2. The value on the rhs of minus is constant regardless
  #    of boot.unit. As the value of "w" changes due to sampling when
  #    isTRUE(boot.unit), so will "u" by the same amount. Note that for
  #    bootstrap cases where a unique core node is included > 1x, the value
  #    of u is being calculated for each instance, separately
  #    (as above for w).
  if (boot.unit) {
    strat.b <- matrix(newdf$newClus, nboot)
    w <- u <- n1n2 <- medResp1 <- medResp2 <- con_n <- vac_n <-
      matrix(NA, nboot, nclus)
    n.each <- rep(NA, nclus)
    names(n.each) <- indivclus$clusterID
    w.boot <- function(x, y, n.b){
      n.x <- length(x)
      n.y <- length(y)
      x.b <- matrix(switch(as.character(n.x == 1),
                           'TRUE' = rep(x, n.b),
                           'FALSE' = sample(x, size = n.b * n.x,
                                            replace = TRUE)),
                    n.b, n.x)
      y.b <- matrix(switch(as.character(n.y == 1),
                           'TRUE' = rep(y, n.b),
                           'FALSE' = sample(y, size = n.b * n.y,
                                            replace = TRUE)),
                    n.b, n.y)
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
        filter(!!symtgroup == compare[2] & clusterID == a) %>%
        select(!!symresp) %>%
        as_vector() %>%
        unname()

      n.x <- length(x)
      n.y <- length(y)

      n.each[a] <<- sum(strat.b == a)
      thiswboot <- w.boot(x, y, n.each[a])
      w[strat.b == a] <<- thiswboot[[1]]
      u[strat.b == a] <<- w[strat.b == a] - (n.x * (n.x + 1))/2
      n1n2[strat.b == a] <<- n.x * n.y
      con_n[strat.b == a] <<- n.x
      vac_n[strat.b == a] <<- n.y
      medResp1[strat.b == a] <<- median(x, na.rm = TRUE)
      medResp2[strat.b == a] <<- median(y, na.rm = TRUE)
      return(NULL)

    })

    newnames <- c('medResp1', 'medResp2', 'n1', 'n2')
    names(newnames) <- c(paste(compare, 'medResp', sep = '_'),
                         paste(compare, 'n', sep = '_'))
    budat <- newdf %>%
      mutate(w = as.vector(t(w)),
             u = as.vector(t(u)),
             n1n2 = as.vector((t(n1n2))),
             n1 = as.vector((t(con_n))),
             n2 = as.vector((t(vac_n))),
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
                n = length(!!symresp),
                medResp = median(!!symresp, na.rm = TRUE)) %>%
      gather(variable, value, -c(clusterID, !!symtgroup)) %>%
      unite(tmp, tgroup, variable) %>%
      spread(tmp, value) %>%
      rename(w = !!wx) %>%
      mutate(u = w - (!!nx * (!!nx + 1))/2,
             n1n2 = !!nx * !!ny,
             !!quo_name(nx) := !!nx,
             !!quo_name(ny) := !!ny) %>%
      select(-!!wy) %>%
      full_join(newdf, by = c('clusterID' = 'newClus')) %>%
      arrange(bootID) %>%
      select(bootID, everything()) %>%
      full_join(indivclus, by = 'clusterID') %>%
      ungroup() %>%
      select(-clusterID)
  }

  return(list(bootmfh = budat, clusters = indivclus, compare = compare, mfh = MFh(formula, data, compare), seed = seed))

}
# to keep R CMD happy
utils::globalVariables(c('clusterID', 'newClus', 'variable', 'value', 'tmp'))

#' @title MFnestBoot
#' @name MFnestBoot
#' @description MFnest using bootstrapping
#' @param x output from \code{\link{MFhBoot}}
#' @param which.factor one or more grouping variable(s) of interest. This can be any of
#' the core or nest variables from the data set. A MF value will be calculated for
#' each level of the variable(s) specified. Default is 'All', to sum over entire tree.
#' @param alpha Passed to \code{emp.hpd} to calculate eq tailed upper
#' and high lower
#' of mitigated fraction
#' @return A list with the following elements: \cr
#' \describe{
#'
#' \item{mfnest_details}{The MF and summary statistics as calculated for each
#' bootstrap event. Variables as in \code{\link{MFnest}} output.}
#' \item{mfnest_summary}{Statistical summary of bootstrapped MF with each unique
#' level of a core or nest variable passed to \code{which.factor} as a row.
#' Other variables include: \cr
#' \itemize{
#' \item \code{median} Median of MFs from all of the bootstrap events.
#' \item \code{etlower} Lower value of equal tailed range.
#' \item \code{etupper} Upper value of equal tailed range.
#' \item \code{hdlower} Lower value of the highest posterior density range.
#' \item \code{hdupper} Upper value of the highest posterior density range.
#' \item \code{mf.obs} MF calculated from data using \code{\link{MFh}}.
#' }}
#' }
#' @seealso \code{\link{MFClusBootHier}}, \code{\link{MFhBoot}}
#' @export
#' @author \link{MF-package}
#' @examples
#' set.seed(76153)
#' a <- data.frame(room = paste('Room', rep(c('W','Z'), each = 24)),
#'                 pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
#'                 litter = paste('Litter', rep(11:22, each = 4)),
#'                 tx = rep(rep(c('vac', 'con'), each = 2), 12),
#'                 stringsAsFactors = FALSE)
#' a[a$tx == 'vac', 'lung'] <- rnorm(24, 5, 1.3)
#' a[a$tx == 'con', 'lung'] <- rnorm(24, 7, 1.3)
#' a
#'
#' formula <- lung ~ tx + room/pen/litter
#' nboot <- 10000
#' boot.cluster <- TRUE
#' boot.unit <- TRUE
#' which.factors <- c('All', 'room', 'pen', 'litter')
#'
#' #################
#'
#' test1 <- MFhBoot(formula, a,
#'                  nboot = 10000,
#'                  boot.cluster = TRUE, boot.unit = TRUE, seed = 12345)
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

  tmpall <- x$bootmfh %>%
    select(-ends_with('_medResp'))

  stat.names <- paste(x$compare, "n", sep = "_")
  comp1 <- sym(stat.names[1])
  comp2 <- sym(stat.names[2])

  comp3 <- sym(gsub(stat.names[1], pattern = "_n", replacement = "_N"))
  comp4 <- sym(gsub(stat.names[2], pattern = "_n", replacement = "_N"))

  mfnest_all <- bind_rows(tmpall %>%
                        gather(variable, level, -c('bootID', 'w', 'u', 'n1n2',
                                                   stat.names)) %>%
                        mutate(level = as.character(level)),
                      tmpall %>%
                        select(bootID, w, u, n1n2, !!comp1, !!comp2) %>%
                        mutate(variable = 'All', level = 'All')) %>%
    filter(variable %in% which.factor) %>%
    group_by(variable, level, bootID) %>%
    summarize(U = sum(u),
              N1N2 = sum(n1n2),
              !!quo_name(comp3) := sum(!!comp1),
              !!quo_name(comp4) := sum(!!comp2),
              MF = 2 * (U/N1N2) - 1)

   mfnest_summary <- mfnest_all %>%
    group_by(variable, level) %>%
    summarize(median = quantile(MF, prob = quant[1]),
              etlower = quantile(MF, prob = quant[2]),
              etupper = quantile(MF, prob = quant[3]),
              hdlower = emp.hpd(MF, alpha = alpha)[1],
              hdupper = emp.hpd(MF, alpha = alpha)[2]) %>%
    full_join(MFnest(x$mfh, which.factor = which.factor) %>%
                select(variable, level, MF) %>%
                rename(mf.obs = 'MF') %>%
                mutate_if(is.factor, as.character),
              by = c('variable', 'level')) %>%
    ungroup() %>%
    mutate(variable = fct_relevel(variable, which.factor)) %>%
    arrange(variable)

  return(list(mfnest_details = mfnest_all, mfnest_summary = mfnest_summary, seed = x$seed))
}

# to keep R CMD happy
utils::globalVariables(c('variable', 'level', 'bootID', 'w', 'u', 'n1n2',
  'U', 'N1N2', 'MF'))
