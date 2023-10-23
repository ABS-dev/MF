## ----hiddensetup, echo = FALSE, include = FALSE--------------------------
require(knitr)
require(kableExtra)
require(tidyverse)
require(MF)

## example data
a <- data.frame(
  room = paste("Room", rep(c("W", "Z"), each = 24)),
  pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
  litter = paste("Litter", rep(11:22, each = 4)),
  tx = rep(rep(c("vac", "con"), each = 2), 12),
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
a <- a[-48, ]
a$lung <- round(a$lung, 2)
a


## ---- expdesign, echo = FALSE--------------------------------------------
knitr::kable(a, align = "c", row.names = FALSE,
             caption = "Nested hierarchical data structure.",
             format = "latex") %>%
  kable_styling("bordered", full_width = FALSE, font_size = 8) %>%
  collapse_rows(columns = 1:4, valign = "middle")


## ---- echo = FALSE-------------------------------------------------------
thismfh <- MFh(formula = lung ~ tx + room / pen / litter, data = a)
thismfh


## ---- message=FALSE, echo=FALSE------------------------------------------
MFnest(thismfh, which.factor = "pen")


## ---- message=FALSE, echo=FALSE------------------------------------------
MFnest(thismfh, which.factor = "All")


## ---- echo = FALSE, message = FALSE--------------------------------------
mfboot_multiple <- MFClusBootHier(formula = lung ~ tx + room / pen / litter,
                                  data = a, boot.unit = FALSE,
                                  boot.cluster = TRUE, alpha = 0.1,
                                  which.factor = c("room", "litter", "All"))
mfboot_multiple$MFhBoot$bootmfh %>%
  filter(bootID == 1) %>%
  select(c("bootID", "room", "pen", "litter"))


## ---- eval = FALSE-------------------------------------------------------
## a <- data.frame(
##  room = paste("Room", rep(c("W", "Z"), each = 24)),
##  pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
##  litter = paste("Litter", rep(11:22, each = 4)),
##  tx = rep(rep(c("vac", "con"), each = 2), 12),
##  stringsAsFactors = FALSE
##  )
## set.seed(76153)
## a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
## a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
## a <- a[-48, ]
##
## # DETERMINING MITIGATED FRACTION
## thismfh <- MFh(formula = lung ~ tx + room / pen / litter, data = a)
## MFnest(thismfh, which.factor = "pen")
## MFnest(thismfh, which.factor = "All")
##
## # BOOTSTRAPPING
## MFClusBootHier(formula = lung ~ tx + room / pen / litter,
##                                   data = a, boot.unit = FALSE,
##                                   boot.cluster = TRUE, alpha = 0.1,
##                                   which.factor = c("room", "litter", "All"))
## mfboot_multiple$MFhBoot$bootmfh %>%
##   filter(bootID == 1) %>%
##   select(c("bootID", "room", "pen", "litter"))


## ------------------------------------------------------------------------
sessionInfo()
