#' @name MFh
#' @title Identify ranks for use when evaluating MF for nested hierarchy.
#' @param formula Formula of the form y ~ x + a/b/c, where y is a continuous
#'   response, x is a factor with two levels of treatment, and a/b/c are
#'   vgrouping variables corresponding to the clusters. Nesting is assumed to be
#'   in order, left to right, highest to lowest. So a single level of "a" will
#'   contain multiple levels of "b" and a single level of "b" will contain
#'   multiple levels of "c".
#' @param data a data.frame or tibble with the variables specified in formula.
#'   Additional variables will be ignored.
#' @param compare Text vector stating the factor levels - compare[1] is the
#'   control or reference group to which compare[2] is compared.
#' @return A \code{\link{mfhierdata}} object, which is a list of three items.
#'   \cr \describe{
#'
#'
#'   \item{coreTbl}{A \code{\link[dplyr]{tibble}} with one row for each unique
#'   core level showing values for:
#'
#'   \itemize{
#'
#'   \item \emph{con}\code{_n} & \emph{vac}\code{_n} - counts of observations
#'   for each treatment level in the core level.
#'
#'   \item \emph{con}\code{_medResp} & \emph{vac}\code{_medResp} - median of the
#'   y continuous response for each treatment level.
#'
#'   \item \code{n1n2} - product of the counts, \emph{con}\code{_n} *
#'   \emph{vac}\code{_n}.
#'
#'   \item \code{w} - Wilcoxon statistic
#'
#'   \item \code{u} - Mann-Whitney statistic
#'
#'   }
#'
#'   }
#'
#'   \item{data}{A \code{\link[dplyr]{tibble}} of the restructured input data
#'   used for calculations.}
#'
#'   \item{compare}{The compare variables as input by user.}
#'
#'   \item{formula}{The formula as input by user.}
#'
#'   }
#' @note Core variable is the variable corresponding to the lowest nodes of the
#'   hierarchial tree. Nest variables are those above the core.
#' @seealso \code{\link{MFnest}} for calculation of MF for nest, core and all
#'   variables. \code{\link{mfhierdata}} for returned
#'   object.\code{\link{MFClusHier}} for a wrapper.
#' @examples
#' a <- data.frame(
#'  room = paste('Room', rep(c('W','Z'), each=24)),
#'  pen = paste('Pen', rep(LETTERS[1:6], each=8)),
#'  litter = paste('Litter', rep(11:22, each=4)),
#'  tx = rep(rep(c('vac','con'), each=2), 12),
#'  stringsAsFactors = FALSE
#'  )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx=='con'] <- rnorm(24, 7, 1.3)
#'
#' aCore <- MFh(lung ~ tx + room / pen / litter, a)
#' aCore
#' #  A tibble: 12 x 10
#' #     room   pen   litter    con_medResp con_n     w vac_medResp vac_n  n1n2
#' #     <chr>  <chr> <chr>           <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl>
#' #   1 Room W Pen A Litter 11        8.24     2     7        5.13     2     4
#' #   2 Room W Pen A Litter 12        4.91     2     5        3.81     2     4
#' #   3 Room W Pen B Litter 13        8.10     2     7        5.23     2     4
#' #   4 Room W Pen B Litter 14        8.11     2     7        5.59     2     4
#' #   5 Room W Pen C Litter 15        8.09     2     7        5.26     2     4
#' #   6 Room W Pen C Litter 16        6.77     2     7        4.50     2     4
#' #   7 Room Z Pen D Litter 17        5.58     2     7        4.26     2     4
#' #   8 Room Z Pen D Litter 18        7.44     2     6        6.33     2     4
#' #   9 Room Z Pen E Litter 19        7.98     2     7        4.58     2     4
#' #  10 Room Z Pen E Litter 20        6.78     2     7        4.86     2     4
#' #  11 Room Z Pen F Litter 21        6.82     2     7        5.36     2     4
#' #  12 Room Z Pen F Litter 22        7.27     2     7        5.13     2     4
#' @export
#' @author \link{MF-package}
#' @importFrom stringr str_c
#' @importFrom tidyr gather unite spread all_of
#' @importFrom stats median terms
#' @importFrom dplyr select sym "%>%" as_tibble ungroup group_by_at mutate
#'   filter vars summarize rename everything
MFh <- function(formula, data, compare = c("con", "vac")) {
  ## get all variables from formula & identify role
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  if (length(nests) == 1) {
    stop("This is not nested hierarchy. See MFClus.")
  }
  # unused? core <- nests[length(nests)]
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]

  ## groups for comparison
  xname <- compare[1]
  yname <- compare[2]

  nx <- sym(str_c(xname, "n", sep = "_"))
  ny <- sym(str_c(yname, "n", sep = "_"))
  wy <- sym(str_c(yname, "w", sep = "_"))
  wx <- sym(str_c(xname, "w", sep = "_"))

  newdat <- as_tibble(data) %>%
    ungroup() %>%
    select(all_of(nests), tgroup = all_of(tgroup), resp = all_of(resp))

  thiscoreTbl <- newdat %>%
    group_by_at(nests) %>%
    mutate(ntgroups = length(unique(tgroup))) %>%
    filter(ntgroups > 1) %>%
    select(-ntgroups) %>%
    mutate(rank = rank(resp)) %>%
    group_by_at(vars(all_of(nests), tgroup)) %>%
    summarize(n = length(resp),
              medResp = median(resp, na.rm = TRUE),
              w = sum(rank)) %>%
    gather(variable, value, -c(tgroup, all_of(nests))) %>%
    unite(temp, tgroup, variable) %>%
    spread(temp, value) %>%
    select(-!!wy) %>%
    rename(w = !!wx) %>%
    mutate(n1n2 = !!nx * !!ny,
           u = w - (!!nx * (!!nx + 1)) / 2) %>%
    select(everything(), w, n1n2, u) %>%
    ungroup()

  return(mfhierdata$new(coreTbl = thiscoreTbl, data = newdat,
                        compare = compare, formula = formula))
}
# to keep R CMD happy
utils::globalVariables(c("u", "bootID", "n1n2", "w", "variable", "value", "tmp",
                         "ntgroups", "temp"))

#' @name MFnest
#' @title Summations to calculate the MF for nested data from a rank table.
#' @param Y rank table (tibble or data.frame), structured as \code{$coreTbl}
#'   output from \code{\link{MFh}} or returned object from \code{\link{MFh}()}.
#' @param which.factor one or more grouping variable(s) of interest. This can be
#'   any of the core or nest variables from the data set. If none or \code{All}
#'   is specified, a summary MF will be calculated for the whole tree.
#' @return A tibble with each unique level of a variable as a row. Other values
#'   include: \cr
#'
#'   \describe{
#'
#'   \item{\code{MF}}{Mitigated fraction for the particular level of the
#'   variable in this row.}
#'
#'   \item{\code{N1N2}}{Sum of the \code{n1n2} variable in \code{$coreTbl} field
#'   of \code{\link{mfhierdata}} object output by \code{\link{MFh}} for this
#'   particular variable-level combination.}
#'
#'   \item{\code{U}}{Sum of u variable in \code{$coreTbl} field of
#'   \code{\link{mfhierdata}} object output by \code{\link{MFh}} for this
#'   particular variable-level combination.}
#'
#'   \item{\code{_N}}{Sum of the \code{_n} variable in \code{$coreTbl} field of
#'   \code{\link{mfhierdata}} object output by \code{\link{MFh}} for this
#'   particular variable-level combination.}
#'
#'   \item{\code{_medResp}}{Median of observed responses for each comparison
#'   group for this particular variable-level combination.}
#'
#'   }
#' @note Core variable is the variable corresponding to the lowest nodes of the
#'   hierarchial tree. Nest variables are those above the core. All refers to a
#'   summary of the entire tree.
#' @seealso \code{\link{MFh}}
#' @examples
#' a <- data.frame(
#'  room = paste('Room', rep(c('W','Z'), each=24)),
#'  pen = paste('Pen', rep(LETTERS[1:6], each=8)),
#'  litter = paste('Litter', rep(11:22, each=4)),
#'  tx = rep(rep(c('vac','con'), each=2), 12),
#'  stringsAsFactors = FALSE
#'  )
#' set.seed(76153)
#' a$lung[a$tx=='vac'] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx=='con'] <- rnorm(24, 7, 1.3)
#'
#' aCore <- MFh(lung ~ tx + room / pen / litter, a)
#' MFnest(aCore)
#' # # A tibble: 1 x 9
#' #   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' # 1 All      All   0.875    48    45    24    24        7.24        4.91
#'
#' MFnest(aCore$coreTbl)
#' # Skipping median summary, no response data provided.
#' # # A tibble: 1 x 7
#' #   variable level    MF  N1N2     U con_N vac_N
#' #   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#' # 1 All      All   0.875    48    45    24    24
#'
#' MFnest(aCore, 'room')
#' # # A tibble: 2 x 9
#' #   variable level     MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #   <fct>    <chr>  <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' # 1 room     Room W 0.833    24    22    12    12        7.79        4.85
#' # 2 room     Room Z 0.917    24    23    12    12        6.71        4.98
#'
#' MFnest(aCore, 'pen')
#' # Complete separation observed for variable(s): pen
#' # # A tibble: 6 x 9
#' #   variable level    MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #   <fct>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' # 1 pen      Pen A  0.5      8     6     4     4        6.79        4.24
#' # 2 pen      Pen B  1        8     8     4     4        8.11        5.59
#' # 3 pen      Pen C  1        8     8     4     4        7.69        4.85
#' # 4 pen      Pen D  0.75     8     7     4     4        6.10        4.98
#' # 5 pen      Pen E  1        8     8     4     4        6.86        4.86
#' # 6 pen      Pen F  1        8     8     4     4        6.88        5.13
#'
#' MFnest(aCore, c('All', 'litter'))
#' # Complete separation observed for variable(s): litter
#' # # A tibble: 13 x 9
#' #    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' #  1 All      All       0.875    48    45    24    24        7.24        4.91
#' #  2 litter   Litter 11 1         4     4     2     2        8.24        5.13
#' #  3 litter   Litter 12 0         4     2     2     2        4.91        3.81
#' #  4 litter   Litter 13 1         4     4     2     2        8.10        5.23
#' #  5 litter   Litter 14 1         4     4     2     2        8.11        5.59
#' #  6 litter   Litter 15 1         4     4     2     2        8.09        5.26
#' #  7 litter   Litter 16 1         4     4     2     2        6.77        4.50
#' #  8 litter   Litter 17 1         4     4     2     2        5.58        4.26
#' #  9 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
#' # 10 litter   Litter 19 1         4     4     2     2        7.98        4.58
#' # 11 litter   Litter 20 1         4     4     2     2        6.78        4.86
#' # 12 litter   Litter 21 1         4     4     2     2        6.82        5.36
#' # 13 litter   Litter 22 1         4     4     2     2        7.27        5.13
#'
#' MFnest(aCore, 'litter')
#' # Complete separation observed for variable(s): litter
#' # # A tibble: 12 x 9
#' #    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' #  1 litter   Litter 11   1       4     4     2     2        8.24        5.13
#' #  2 litter   Litter 12   0       4     2     2     2        4.91        3.81
#' #  3 litter   Litter 13   1       4     4     2     2        8.10        5.23
#' #  4 litter   Litter 14   1       4     4     2     2        8.11        5.59
#' #  5 litter   Litter 15   1       4     4     2     2        8.09        5.26
#' #  6 litter   Litter 16   1       4     4     2     2        6.77        4.50
#' #  7 litter   Litter 17   1       4     4     2     2        5.58        4.26
#' #  8 litter   Litter 18   0.5     4     3     2     2        7.44        6.33
#' #  9 litter   Litter 19   1       4     4     2     2        7.98        4.58
#' # 10 litter   Litter 20   1       4     4     2     2        6.78        4.86
#' # 11 litter   Litter 21   1       4     4     2     2        6.82        5.36
#' # 12 litter   Litter 22   1       4     4     2     2        7.27        5.13
#'
#' MFnest(aCore, c('room', 'pen', 'litter'))
#' # # A tibble: 20 x 9
#' #    variable level        MF  N1N2     U con_N vac_N con_medResp vac_medResp
#' #    <fct>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>       <dbl>
#' #  1 room     Room W    0.833    24    22    12    12        7.79        4.85
#' #  2 room     Room Z    0.917    24    23    12    12        6.71        4.98
#' #  3 pen      Pen A     0.5       8     6     4     4        6.79        4.24
#' #  4 pen      Pen B     1         8     8     4     4        8.11        5.59
#' #  5 pen      Pen C     1         8     8     4     4        7.69        4.85
#' #  6 pen      Pen D     0.75      8     7     4     4        6.10        4.98
#' #  7 pen      Pen E     1         8     8     4     4        6.86        4.86
#' #  8 pen      Pen F     1         8     8     4     4        6.88        5.13
#' #  9 litter   Litter 11 1         4     4     2     2        8.24        5.13
#' # 10 litter   Litter 12 0         4     2     2     2        4.91        3.81
#' # 11 litter   Litter 13 1         4     4     2     2        8.10        5.23
#' # 12 litter   Litter 14 1         4     4     2     2        8.11        5.59
#' # 13 litter   Litter 15 1         4     4     2     2        8.09        5.26
#' # 14 litter   Litter 16 1         4     4     2     2        6.77        4.50
#' # 15 litter   Litter 17 1         4     4     2     2        5.58        4.26
#' # 16 litter   Litter 18 0.5       4     3     2     2        7.44        6.33
#' # 17 litter   Litter 19 1         4     4     2     2        7.98        4.58
#' # 18 litter   Litter 20 1         4     4     2     2        6.78        4.86
#' # 19 litter   Litter 21 1         4     4     2     2        6.82        5.36
#' # 20 litter   Litter 22 1         4     4     2     2        7.27        5.13
#' @export
#' @author \link{MF-package}
#' @importFrom stringr str_subset
#' @importFrom tidyr gather spread
#' @importFrom forcats fct_relevel
#' @importFrom stats median
#' @importFrom dplyr select as_tibble sym "%>%" mutate_if mutate bind_rows
#'   group_by summarize filter ungroup distinct pull rename left_join arrange
#'   everything
#' @importFrom rlang ":=" quo_name
MFnest <- function(Y, which.factor = "All") {
  ## restructure if using output from MFh
  if (class(Y)[1] == "mfhierdata") {
    input <- Y
    Y <- input$coreTbl
  } else if (class(Y)[1] != "tbl_df") {
    Y <- as_tibble(Y)
  }

  stat.names <- c(str_subset(names(Y), "_medResp"),
                  str_subset(names(Y), "_n"), "n1n2", "u", "w")
  comp1 <- sym(stat.names[3])
  comp2 <- sym(stat.names[4])

  comp3 <- sym(gsub(stat.names[3], pattern = "_n", replacement = "_N"))
  comp4 <- sym(gsub(stat.names[4], pattern = "_n", replacement = "_N"))

  out <- Y %>%
    mutate_if(is.factor, as.character) %>%
    gather(variable, level, -all_of(stat.names)) %>%
    mutate(level = as.character(level)) %>%
    bind_rows(.,
              select(Y, all_of(stat.names)) %>%
                mutate(variable = "All", level = "All")) %>%
    group_by(variable, level) %>%
    summarize(N1N2 = sum(n1n2), U = sum(u), con_N = sum(!!comp1),
              vac_N = sum(!!comp2)) %>%
    mutate(R = U / N1N2, MF = 2 * R - 1) %>%
    select(-R, !!quo_name(comp3) := con_N, !!quo_name(comp4) := vac_N) %>%
    filter(tolower(variable) %in% tolower(which.factor)) %>%
    ungroup()

  ## inform user of complete separation
  if (1.0 %in% round(out$MF, digits = 1)) {
    out %>%
      filter(round(MF, digits = 1) == 1.0) %>%
      distinct(variable) %>%
      pull() %>%
      paste0(collapse = ", ") %>%
      message("Complete separation observed for variable(s): ", .,
              collapse = "")
  }

  ## inform user why medians are not available
  if (!exists("input")) {
    message("Skipping median summary, no response data provided.")
  } else {

    thisdata <- input$data
    compare <- input$compare
    names(compare) <- paste0(input$compare, "_medResp", sep = "")

    out <- thisdata %>%
      mutate_if(is.factor, as.character) %>%
      gather(variable, level, -c(tgroup, resp)) %>%
      mutate(level = as.character(level)) %>%
      bind_rows(.,
                select(thisdata, c(tgroup, resp)) %>%
                  mutate(variable = "All", level = "All") %>%
                  mutate_if(is.factor, as.character)) %>%
      group_by(variable, level, tgroup) %>%
      summarize(medResp = median(resp, na.rm = TRUE)) %>%
      spread(tgroup, medResp) %>%
      ungroup() %>%
      rename(!!compare) %>%
      filter(tolower(variable) %in% tolower(which.factor)) %>%
      left_join(out, ., by = c("variable", "level"))

  }

  out <- out %>%
    mutate(variable = fct_relevel(variable, which.factor)) %>%
    arrange(variable) %>%
    select(variable, level, MF, everything())

  return(out)
}

# to keep R CMD happy
utils::globalVariables(c("R", "con_N", "vac_N", "tgroup", "resp", "medResp"))
