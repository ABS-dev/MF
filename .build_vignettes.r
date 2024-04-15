# 1. DESCRIPTION should contain this line: `Config/build/clean-inst-doc: FALSE`
# 2. Should be run from within root directory of project.
# 3. Most recent version of package should be installed.
# 4. Install
# 5. Run this script
# 6. Update tags and push
# 7. Push new version

path_0 <- "vignettes"
path_1 <- "inst"
path_2 <- "inst/doc"

if (!dir.exists(path_0)) dir.create(path_0)
if (!dir.exists(path_1)) dir.create(path_1)
if (!dir.exists(path_2)) dir.create(path_2)

files <- dir(path_0, pattern = "rmd")

for (ff in files) {
  print(ff)
  rmarkdown::render(file.path(path_0, ff),
                    "all",
                    output_dir = path_2)
}
project <- basename(rstudioapi::getActiveProject())
ver <- packageVersion(project)

pat_pdf <- paste0("^", project, "_[0-9]+\\.[0-9]+.*\\.pdf$")
pat_asis <- paste0("^", project, "_[0-9]+\\.[0-9]+.*\\.asis$")

manuals <- c(dir(path_0, pattern = pat_pdf), dir(path_0, pattern = pat_asis))
manuals <- file.path(path_0, manuals)
manuals
file.remove(manuals)

manuals <- c(dir(path_2, pattern = pat_pdf), dir(path_2, pattern = pat_asis))
manuals <- file.path(path_2, manuals)
manuals
file.remove(manuals)

asis <- paste0(project, "_", ver, ".pdf.asis")
asis

text <- 
  "%%\\VignetteIndexEntry{Package '%s'}
%%\\VignetteEngine{R.rsp::asis}"
text <- sprintf(text, project)

data.table::fwrite(
  data.table::data.table(text),
  file = file.path(path_0, asis),
  quote = FALSE,
  col.names = FALSE
)


devtools::build_manual(path = path_0)

manual <- dir(path_0, pattern = pat_pdf)
file.copy(file.path(path_0, manual),
          file.path(path_2, manual))

pdf <- c(file.path(path_0, dir(path_0, pattern = "*.pdf$")),
         file.path(path_2, dir(path_2, pattern = "*.pdf$")))

for (fp in pdf) {
  tools::compactPDF(pdf)
}
