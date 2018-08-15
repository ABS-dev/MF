#' @name MFh
#' @title Identify ranks for use when evaluating MF for nested hierarchy.
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous 
#' response, x is a factor with two levels of treatment, and a/b/c are variables 
#' corresponding to the clusters. It is expected that levels of "c" are nested within 
#' levels of "b". Nesting is assumed to be in order, left to right, highest to lowest.
#' @param data a data.frame or tibble with the variables specified in formula. 
#' Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - compare[1] is the control or 
#' reference group to which compare[2] is compared.
#' @return A \code{\link{mfhierdata}} object, which is a list of three items. \cr
#' \describe{
#' 
#' \item{coreTbl}{A \code{\link[dplyr]{tibble}} with one row for each unique core level showing values for:
#' \itemize{
#'   \item \emph{con}\code{_n} & \emph{vac}\code{_n} - counts of observations for each treatment 
#' level in the core level.
#'   \item \code{N} - total number of observations across all treatement levels at that core level.
#'   \item \code{w} - Wilcoxon statistic
#'   \item \code{u} - Mann-Whitney statistic
#' }
#' }
#' \item{data}{A \code{\link[dplyr]{tibble}} of the restructured input data used for calculations.}
#' \item{compare}{The compare variables as input by user.}
#' \item{formula}{The formula as input by user.}
#' }
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core.
#' @seealso \code{\link{MFnest}} for calculation of MF for nest, core and all variables. \code{\link{mfhierdata}} for returned object.
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
#' # A tibble: 12 x 10
#' #     room   pen   litter    con_medResp con_n     w vac_medResp vac_n     N     u
#' #     <fct>  <fct> <fct>           <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl> <dbl>
#' #   1 Room W Pen A Litter 11        8.24     2     7        5.13     2     4     4
#' #   2 Room W Pen A Litter 12        4.91     2     5        3.81     2     4     2
#' #   3 Room W Pen B Litter 13        8.10     2     7        5.23     2     4     4
#' #   4 Room W Pen B Litter 14        8.11     2     7        5.59     2     4     4
#' #   5 Room W Pen C Litter 15        8.09     2     7        5.26     2     4     4
#' #   6 Room W Pen C Litter 16        6.77     2     7        4.50     2     4     4
#' #   7 Room Z Pen D Litter 17        5.58     2     7        4.26     2     4     4
#' #   8 Room Z Pen D Litter 18        7.44     2     6        6.33     2     4     3
#' #   9 Room Z Pen E Litter 19        7.98     2     7        4.58     2     4     4
#' #  10 Room Z Pen E Litter 20        6.78     2     7        4.86     2     4     4
#' #  11 Room Z Pen F Litter 21        6.82     2     7        5.36     2     4     4
#' #  12 Room Z Pen F Litter 22        7.27     2     7        5.13     2     4     4
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
  
  nx <- sym(str_c(xname, "n", sep = "_"))
  ny <- sym(str_c(yname, "n", sep = '_'))
  wy <- sym(str_c(yname, "w", sep = "_"))
  wx <- sym(str_c(xname, "w", sep = "_"))
  
  newdat <- as_tibble(data) %>%
    ungroup() %>%
    select(nests, tgroup = tgroup, resp = resp) 
  
  thiscoreTbl <- newdat %>%
    group_by_at(nests) %>%
    mutate(ntgroups = length(unique(tgroup))) %>%
    filter(ntgroups > 1) %>%
    select(-ntgroups) %>%
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

  return(mfhierdata$new(coreTbl = thiscoreTbl, data = newdat, 
                        compare = compare, formula = formula))
}

#' @name MFnest
#' @title Summations to calculate the MF for nested data from a rank table.
#' @param Y rank table (tibble or data.frame), structured as \code{$coreTbl} output from \code{\link{MFh}} or
#'  output list from MFh.
#' @param which.factor one or more variable(s) of interest. This can be any of the core or nest variables
#' from the data set. If none or \code{NULL} is specified, MF will be calculated for the whole 
#' tree.
#' @return tibble with each unique level of a variable as a row. Values \code{N}, 
#' \code{U}, \code{MF} and median of responses returned.
#' @note Core variable is the variable corresponding to the lowest nodes of the hierarchial 
#' tree. Nest variables are those above the core. All refers to a summary of the entire tree.
#' @seealso \code{\link{MFh}}
#' @examples
#' MFnest(aCore)
#' #   # A tibble: 1 x 7
#' #     variable level     N     U    MF `median_resp:con` `median_resp:vac`
#' #     <fct>    <chr> <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' #   1 All      All      48    45 0.875              7.24              4.91
#' 
#' MFnest(aCore$coreTbl)
#' # Skipping median summary, no response data provided.
#' # # A tibble: 1 x 5
#' #   variable level     N     U    MF
#' #   <fct>    <chr> <dbl> <dbl> <dbl>
#' # 1 All      All      48    45 0.875
#' 
#' MFnest(aCore, 'room')
#' # # A tibble: 2 x 7
#' #   variable level      N     U    MF `median_resp:con` `median_resp:vac`
#' #   <fct>    <chr>  <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' # 1 room     Room W    24    22 0.833              7.79              4.85
#' # 2 room     Room Z    24    23 0.917              6.71              4.98
#' 
#' MFnest(aCore, 'pen')
#' # Complete separation observed for variable(s): pen
#' # # A tibble: 6 x 7
#' #   variable level     N     U    MF `median_resp:con` `median_resp:vac`
#' #   <fct>    <chr> <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' # 1 pen      Pen A     8     6  0.5               6.79              4.24
#' # 2 pen      Pen B     8     8  1                 8.11              5.59
#' # 3 pen      Pen C     8     8  1                 7.69              4.85
#' # 4 pen      Pen D     8     7  0.75              6.10              4.98
#' # 5 pen      Pen E     8     8  1                 6.86              4.86
#' # 6 pen      Pen F     8     8  1                 6.88              5.13
#' 
#' MFnest(aCore, c('All', 'litter'))
#' # Complete separation observed for variable(s): litter
#' # # A tibble: 13 x 7
#' #  variable level         N     U    MF `median_resp:con` `median_resp:vac`
#' #  <fct>    <chr>     <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' #  1 All      All          48    45 0.875              7.24              4.91
#' #  2 litter   Litter 11     4     4 1                  8.24              5.13
#' #  3 litter   Litter 12     4     2 0                  4.91              3.81
#' #  4 litter   Litter 13     4     4 1                  8.10              5.23
#' #  5 litter   Litter 14     4     4 1                  8.11              5.59
#' #  6 litter   Litter 15     4     4 1                  8.09              5.26
#' #  7 litter   Litter 16     4     4 1                  6.77              4.50
#' #  8 litter   Litter 17     4     4 1                  5.58              4.26
#' #  9 litter   Litter 18     4     3 0.5                7.44              6.33
#' # 10 litter   Litter 19     4     4 1                  7.98              4.58
#' # 11 litter   Litter 20     4     4 1                  6.78              4.86
#' # 12 litter   Litter 21     4     4 1                  6.82              5.36
#' # 13 litter   Litter 22     4     4 1                  7.27              5.13
#'
#' MFnest(aCore, 'litter')
#' # Complete separation observed for variable(s): litter
#' # # A tibble: 12 x 7
#' #    variable level         N     U    MF `median_resp:con` `median_resp:vac`
#' #    <fct>    <chr>     <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' #  1 litter   Litter 11     4     4   1                8.24              5.13
#' #  2 litter   Litter 12     4     2   0                4.91              3.81
#' #  3 litter   Litter 13     4     4   1                8.10              5.23
#' #  4 litter   Litter 14     4     4   1                8.11              5.59
#' #  5 litter   Litter 15     4     4   1                8.09              5.26
#' #  6 litter   Litter 16     4     4   1                6.77              4.50
#' #  7 litter   Litter 17     4     4   1                5.58              4.26
#' #  8 litter   Litter 18     4     3   0.5              7.44              6.33
#' #  9 litter   Litter 19     4     4   1                7.98              4.58
#' # 10 litter   Litter 20     4     4   1                6.78              4.86
#' # 11 litter   Litter 21     4     4   1                6.82              5.36
#' # 12 litter   Litter 22     4     4   1                7.27              5.13
#' 
#' MFnest(aCore, c('room', 'pen', 'litter'))
#' # Complete separation observed for variable(s): litter, pen
#' # # A tibble: 20 x 7
#' #    variable level         N     U    MF `median_resp:con` `median_resp:vac`
#' #    <fct>    <chr>     <dbl> <dbl> <dbl>             <dbl>             <dbl>
#' #  1 room     Room W       24    22 0.833              7.79              4.85
#' #  2 room     Room Z       24    23 0.917              6.71              4.98
#' #  3 pen      Pen A         8     6 0.5                6.79              4.24
#' #  4 pen      Pen B         8     8 1                  8.11              5.59
#' #  5 pen      Pen C         8     8 1                  7.69              4.85
#' #  6 pen      Pen D         8     7 0.75               6.10              4.98
#' #  7 pen      Pen E         8     8 1                  6.86              4.86
#' #  8 pen      Pen F         8     8 1                  6.88              5.13
#' #  9 litter   Litter 11     4     4 1                  8.24              5.13
#' # 10 litter   Litter 12     4     2 0                  4.91              3.81
#' # 11 litter   Litter 13     4     4 1                  8.10              5.23
#' # 12 litter   Litter 14     4     4 1                  8.11              5.59
#' # 13 litter   Litter 15     4     4 1                  8.09              5.26
#' # 14 litter   Litter 16     4     4 1                  6.77              4.50
#' # 15 litter   Litter 17     4     4 1                  5.58              4.26
#' # 16 litter   Litter 18     4     3 0.5                7.44              6.33
#' # 17 litter   Litter 19     4     4 1                  7.98              4.58
#' # 18 litter   Litter 20     4     4 1                  6.78              4.86
#' # 19 litter   Litter 21     4     4 1                  6.82              5.36
#' # 20 litter   Litter 22     4     4 1                  7.27              5.13
#' @export
MFnest <- function(Y, which.factor = 'All') {
  ## restructure if using output from MFh
  if(class(Y)[1] == 'mfhierdata'){
    input <- Y
    Y <- input$coreTbl
  } else if(class(Y)[1] != 'tbl_df'){
    Y <- as_tibble(Y)
  }
  
  stat.names <- c(str_subset(names(Y), "_medResp"), 
                  str_subset(names(Y), "_n"), 'N', 'u', 'w')
  out <- Y %>%
    mutate_if(is.factor, as.character) %>%
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
    
  ## TODO: this needs to go in a helper function
    thisdata <- input$data
    compare <- input$compare
    names(compare) <- paste0("median_resp:", input$compare, sep = '')

    out <- thisdata %>%
      mutate_if(is.factor, as.character) %>%
      gather(variable, level, -c(tgroup, resp)) %>%
      bind_rows(.,
                select(thisdata, c(tgroup, resp)) %>%
                mutate(variable = "All", level = "All") %>%
                mutate_if(is.factor, as.character)) %>%
      group_by(variable, level, tgroup) %>%
      summarize(median_resp = median(resp, na.rm = TRUE)) %>%
      spread(tgroup, median_resp) %>%
      ungroup() %>%
      rename(!!compare) %>%
      filter(tolower(variable) %in% tolower(which.factor)) %>%
      left_join(out, ., by = c('variable', 'level')) 
      
  }    
  
  out <- out %>%
    mutate(variable = fct_relevel(variable, which.factor)) %>%
    arrange(variable)
 
  return(out)
}


