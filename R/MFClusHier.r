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
#' @param vac_grp The name of the vaccinated group.
#' @param con_grp The name of the control group.
#' @param compare `r badge("deprecated")` Text vector stating the factor levels:
#'   `compare[1]` is the control or reference group to which `compare[2]`
#'   (vaccinate) is compared.
#' @returns A [mfhierdata] object, which is a list of three items.
#' * `coreTbl` A [tibble] with one row for each unique core level showing
#'   values for:
#'
#'   * `con_n` & `vac_n`: counts of observations for each treatment level
#'   in the core level.
#'
#'   * `con_medResp` & `vac_medResp`: median of the `y` continuous
#'   response for each treatment level.
#'
#'   * `n1n2`: product of the counts, `con_n` \eqn{\times} `vac_n`.
#'
#'   * `w`: Wilcoxon statistic
#'
#'   * `u`: Mann-Whitney statistic
#'
#' * `data`: A [tibble] of the restructured input data used for
#'   calculations.
#'
#' * `compare`: The compare variables as input by user.
#'
#' * `formula`: The formula as input by user.
#' @note Core variable is the variable corresponding to the lowest nodes of the
#'   hierarchial tree. Nest variables are those above the core.
#' @seealso [MFnest] for calculation of MF for nest, core and all variables.
#'   [mfhierdata] for returned object. [MFClusHier] for a wrapper.
#' @examples
#' a <- data.frame(
#'  room   = paste("Room", rep(c("W", "Z"), each = 24)),
#'  pen    = paste("Pen", rep(LETTERS[1:6], each = 8)),
#'  litter = paste("Litter", rep(11:22, each = 4)),
#'  tx     = rep(rep(c("vac", "con"), each = 2), 12))
#' set.seed(76153)
#' a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
#'
#' aCore <- MFh(lung ~ tx + room / pen / litter, a)
#' aCore
#' @author [MF-package]
#' @importFrom stringr str_c
#' @importFrom tidyr gather unite spread all_of
#' @importFrom stats median terms
#' @importFrom dplyr select sym as_tibble ungroup group_by_at mutate filter vars
#'   summarize rename everything
#' @importFrom lifecycle badge deprecate_warn is_present deprecated
#' @export
MFh <- function(formula,
                data,
                vac_grp = "vac",
                con_grp = "con",
                compare = deprecated()) {
  if (is_present(compare)) {
    deprecate_warn("4.5.0",
                   "Mfh(compare)",
                   "Mfh(vac_grp, con_grp)")
    if (length(compare) != 2) {
      stop("`compare` must be a vector of length 2!")
    }
    vac_grp <- compare[2]
    con_grp <- compare[1]
  }

  ## get all variables from formula & identify role
  termlab <- attr(terms(formula), "term.labels")
  nests <- unlist(strsplit(termlab[[length(termlab)]], split = ":"))
  if (length(nests) == 1) {
    stop("This is not nested hierarchy. See MFClus.")
  }
  tgroup <- termlab[1]
  resp <- all.vars(formula)[1]

  ## groups for comparison
  xname <- con_grp
  yname <- vac_grp

  nx <- sym(str_c(xname, "n", sep = "_"))
  ny <- sym(str_c(yname, "n", sep = "_"))
  wy <- sym(str_c(yname, "w", sep = "_"))
  wx <- sym(str_c(xname, "w", sep = "_"))

  newdat <- as_tibble(data) |>
    ungroup() |>
    select(all_of(nests), tgroup = all_of(tgroup), resp = all_of(resp))

  this_core_tbl <- newdat |>
    group_by_at(nests) |>
    mutate(ntgroups = length(unique(tgroup))) |>
    filter(ntgroups > 1) |>
    select(-ntgroups) |>
    mutate(rank = rank(resp)) |>
    group_by_at(vars(all_of(nests), tgroup)) |>
    summarize(n = length(resp),
             medResp = median(resp, na.rm = TRUE),
             w = sum(rank)) |>
    gather(variable, value, -c(tgroup, all_of(nests))) |>
    unite(temp, tgroup, variable) |>
    spread(temp, value) |>
    select(-!!wy) |>
    rename(w = !!wx) |>
    mutate(n1n2 = !!nx * !!ny,
          u = w - (!!nx * (!!nx + 1)) / 2) |>
    select(everything(), w, n1n2, u) |>
    ungroup()

  return(mfhierdata$new(coreTbl = this_core_tbl, data = newdat,
                        vac_grp = vac_grp, con_grp = con_grp,
                        formula = formula))
}
# to keep R CMD happy
globalVariables(c("u", "bootID", "n1n2", "w", "variable", "value", "tmp",
                  "ntgroups", "temp"))

#' @name MFnest
#' @title Summations to calculate the MF for nested data from a rank table.
#' @param Y rank table (tibble or data.frame), structured as `$coreTbl` output
#'   from [MFh] or returned object from [MFh]().
#' @param which.factor one or more grouping variable(s) of interest. This can be
#'   any of the core or nest variables from the data set. If none or `All` is
#'   specified, a summary MF will be calculated for the whole tree.
#' @returns A tibble with each unique level of a variable as a row. Other values
#'   include:
#'
#' * `MF`: Mitigated fraction for the particular level of the
#'   variable in this row.
#'
#' * `N1N2`: Sum of the `n1n2` variable in `$coreTbl` field of
#'   [mfhierdata] object output by [MFh] for this particular variable-level
#'   combination.
#'
#' * `U`: Sum of u variable in `$coreTbl` field of [mfhierdata] object
#'   output by [MFh] for this particular variable-level combination.
#'
#' * `_N`: Sum of the `_n` variable in `$coreTbl` field of [mfhierdata]
#'   object output by [MFh] for this particular variable-level combination.
#'
#' * `_medResp`: Median of observed responses for each comparison group
#'   for this particular variable-level combination.
#'
#' @note Core variable is the variable corresponding to the lowest nodes of the
#'   hierarchial tree. Nest variables are those above the core. All refers to a
#'   summary of the entire tree.
#' @seealso [MFh]
#' @examples
#' a <- data.frame(
#'  room = paste("Room", rep(c("W", "Z"), each = 24)),
#'  pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
#'  litter = paste("Litter", rep(11:22, each = 4)),
#'  tx = rep(rep(c("vac", "con"), each = 2), 12))
#' set.seed(76153)
#' a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
#' a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
#'
#' aCore <- MFh(lung ~ tx + room / pen / litter, a)
#' MFnest(aCore)
#'
#' MFnest(aCore$coreTbl)
#'
#' MFnest(aCore, "room")
#'
#' MFnest(aCore, "pen")
#'
#' MFnest(aCore, c("All", "litter"))
#'
#' MFnest(aCore, "litter")
#'
#' MFnest(aCore, c("room", "pen", "litter"))
#' @author [MF-package]
#' @importFrom stringr str_subset
#' @importFrom tidyr gather spread
#' @importFrom forcats fct_relevel
#' @importFrom stats median
#' @importFrom dplyr select as_tibble sym mutate_if mutate bind_rows group_by
#'   summarize filter ungroup distinct pull rename left_join arrange everything
#' @importFrom rlang ":=" quo_name
#' @export
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

  out <- Y |>
    mutate_if(is.factor, as.character) |>
    gather(variable, level, -all_of(stat.names)) |>
    mutate(level = as.character(level)) |>
    bind_rows(select(Y, all_of(stat.names)) |>
                mutate(variable = "All", level = "All")) |>
    group_by(variable, level) |>
    summarize(N1N2 = sum(n1n2), U = sum(u), con_N = sum(!!comp1),
              vac_N = sum(!!comp2)) |>
    mutate(R = U / N1N2, MF = 2 * R - 1) |>
    select(-R, !!quo_name(comp3) := con_N, !!quo_name(comp4) := vac_N) |>
    filter(tolower(variable) %in% tolower(which.factor)) |>
    ungroup()

  ## inform user of complete separation
  if (1.0 %in% round(out$MF, digits = 1)) {
    out |>
      filter(round(MF, digits = 1) == 1.0) |>
      distinct(variable) |>
      pull() |>
      paste0(collapse = ", ") |>
      message("Complete separation observed for variable(s): ", x = _,
              collapse = "")
  }

  ## inform user why medians are not available
  if (!exists("input")) {
    message("Skipping median summary, no response data provided.")
  } else {
    thisdata <- input$data
    compare <- c(input$con_grp, input$vac_grp)
    names(compare) <- paste0(compare, "_medResp", sep = "")

    out <- thisdata |>
      mutate_if(is.factor, as.character) |>
      gather(variable, level, -c(tgroup, resp)) |>
      mutate(level = as.character(level)) |>
      bind_rows(select(thisdata, c(tgroup, resp)) |>
                  mutate(variable = "All", level = "All") |>
                  mutate_if(is.factor, as.character)) |>
      group_by(variable, level, tgroup) |>
      summarize(medResp = median(resp, na.rm = TRUE)) |>
      spread(tgroup, medResp) |>
      ungroup() |>
      rename(!!compare) |>
      filter(tolower(variable) %in% tolower(which.factor)) |>
      left_join(out, y = _, by = c("variable", "level"))
  }

  out <- out |>
    mutate(variable = fct_relevel(variable, which.factor)) |>
    arrange(variable) |>
    select(variable, level, MF, everything())

  return(out)
}

# to keep R CMD happy
globalVariables(c("R", "con_N", "vac_N", "tgroup", "resp", "medResp"))
