# 1. DESCRIPTION should contain this line: `Config/build/clean-inst-doc: FALSE`
# 2. Should be run from within root directory of project.
# 3. Roll version forward, install, but do not push
# 4. Install
# 5. Run this script
# 6. Update tags and push
# 7. Push new version

library(data.table)
library(stringr)

create_rsp_contents <- function(entry) {
  text <- "%%\\VignetteIndexEntry{%s}\n%%\\VignetteEngine{R.rsp::asis}"
  sprintf(text, entry)
}

path_0 <- "vignettes"
path_1 <- "inst"

if (!dir.exists(path_0)) dir.create(path_0)
if (!dir.exists(path_1)) dir.create(path_1)

# remove old documentation

project <- basename(rstudioapi::getActiveProject())
ver <- packageVersion(project)
pat <- paste0("^", project, "_[0-9]+\\.[0-9]+.*|asis$")
file.remove(file.path(path_0, dir(path_0, pat)))

# Render vignettes

files <- dir(path_0, pattern = "(rmd|rnw)$", ignore.case = TRUE)

for (ff in files) {
  print(ff)
  fp <- file.path(path_0, ff)
  rmarkdown::render(fp,
                    "all",
                    output_dir = path_0)
  dt <- fread(fp, sep = NULL, header = FALSE)
  entry <- dt[str_detect(V1, "VignetteIndexEntry")]
  entry <- str_replace(entry, "^[^\\{]*\\{", "")
  entry <- str_replace(entry, "\\}[^\\}]*$", "")
  text  <- create_rsp_contents(entry)
  asis <- str_replace(fp, "...$", "pdf\\.asis")
  fwrite(data.table(text),
         file = asis,
         quote = FALSE,
         col.names = FALSE)
}

# Render manual

devtools::build_manual(path = path_0)

entry <- sprintf("Package '%s'", project)
text  <- create_rsp_contents(entry)
asis  <- paste0(project, "_", ver, ".pdf.asis")
asis  <- file.path(path_0, asis)

fwrite(data.table(text),
  file = asis,
  quote = FALSE,
  col.names = FALSE
)

# Compact Files

pdf <- c(file.path(path_0, dir(path_0, pattern = "pdf$")))

for (fp in pdf) {
  tools::compactPDF(pdf)
}
