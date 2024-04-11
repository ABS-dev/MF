# 1. DESCRIPTION should contain this line: `Config/build/clean-inst-doc: FALSE`
# 2. Should be run from within root directory of project.
# 3. Most recent version of package should be installed.

files <- dir("vignettes", pattern = "rmd")

if (!dir.exists("inst")) dir.create("inst")
if (!dir.exists("inst/doc")) dir.create("inst/doc")

for (ff in files) {
  print(ff)
  rmarkdown::render(file.path("vignettes", ff),
                    "all",
                    output_dir = "inst/doc")
}
