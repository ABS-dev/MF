################################################################################
############ Utilities to check the code for style and other issues ############
################################################################################

library(data.table)
library(stringr)

# Lint the package to check for formatting errors
# Path should be the root of the package directory.
path <- getwd()
linter <- N <- NULL

# Variables that don't follow stylistic guidelines, which we are not changing.
okay_vars <- c("SnR", "SpR", "Prev", "SnE", "SpE",
               "X", "Xpos", "Xsus", "Xneg", "cellP",
               "cellN", "cloppearSnSp", "est.Sn",
               "H", "estimateSnSp", "Sn.ref", "Sp.ref", "prev.pop",
               "n.states", "N", "detailOut", "calcVal",
               "Sn.distn", "Sn.spread", "Sp.distn", "Sp.spread", "prev.distn",
               "prev.spread", "step.size", "rep.iter", "test.names",
               "pop.names", "prev.current", "current.fit",
               "current.ests", "current.con", "message.current",
               "sus.perc", "updateAlpha", "newAlpha",
               "blood2_a_SnSp", "Sn.fig", "Sp.fig",
               "return.boot", "trace.it", "n.x", "hl.fn", "n.y", "Y",
               "A", "x.b", "y.b", "Qx", "Qy", "W", "H", "Qdif", "MF",
               "nx.i", "ny.i", "theta.hat", "boot.cluster", "boot.unit",
               "excluded.clusters", "n.strat", "strat.b", "x.y", "U",
               "n.each", "strat.b", "cluster.text", "and.text", "unit.text",
               "the.text", "exluded.clusters", "thiscoreTbl", "byCluster",
               "byCluster", "datID", "medResp1", "medResp2", "u.j", "mf.j")

## Change This Value ##
# Determine whose files should be linted.  Change the number in the [.]

excluded_files <- included_files <- NULL

idx <- 1
excluded_linters <- c("indentation_linter",
                      "cyclocomp_linter",
                      "commented_code_linter",
                      "object_usage_linter"
                      )[idx]

# Run lintr on the selected files.
tmp <- codeDiagnostics::lint_package_extended(
  path = path,
  okay_vars = okay_vars,
  excluded_files = excluded_files,
  included_files = included_files,
  excluded_linters = excluded_linters)

# Send results to Markers screen
if (length(tmp) == 0) {
  message("Sucess!  No lintr issues!")
} else {
  tmp
}

## Run to this point to see lint results.
# CTRL-SHIFT-B

# Format results as a data.table for further exploration
dt <- data.table::as.data.table(tmp)
dt[, .N, linter][order(N)]
dt[, .N, filename]
dt[, .N, keyby = .(filename, linter)]


tmp[dt[, which(grepl("name", linter))]]



lf <- paste0("(", paste0(lindsay_files, collapse = "|"), ")")
dt[!str_detect(filename, lf), .N, filename]
tmp[!dt[, str_detect(filename, lf)]]


?assign


tmp <- dt[str_detect(linter, "name"),
          str_extract(trimws(line), "^[^ ]*"), line][, .N, keyby = V1]
cat(tmp[, paste0("\"", V1, "\"", collapse = ", ")])
