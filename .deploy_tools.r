## Create Static Documentation ####

# 1. DESCRIPTION should contain this line: `Config/build/clean-inst-doc: FALSE`
# 2. Should be run from within root directory of project.
# 3. Roll version forward, install, but do not push
# 4. Install
# 5. Run this script
# 6. Update tags and push
# 7. Push new version
CVBdevtools::create_static_documentation()


## Create pkgdown Content locally ####

CVBdevtools::deploy_pkgdown_local()

# I ran git update-index --assume-unchanged <file> in the terminal for each file
# in pkgdown and public to remove them from being followed.  Hopefully it
# sticks.
#
# I finally just started deploying into docs/ since that has been in gitignore
# since the beginning.

## Deploy pkgdown Content to remote git repository ####

# 1. Roll Version forward, install and then run this script.
CVBdevtools::deploy_pkgdown_remote()

# Lint Package with Extended Version

{
  library(data.table)

  # Lint the package to check for formatting errors
  # Path should be the root of the package directory.
  path <- getwd()
  linter <- N <- NULL

  # Variables that don't follow stylistic guidelines, which we are not changing.

  okay_vars <- c("")

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
}

data.table(tmp)
