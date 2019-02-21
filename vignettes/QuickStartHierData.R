## ----hiddensetup, echo = FALSE, include = FALSE--------------------------
require(kableExtra)
require(tidyverse)
require(MF)

## example data
a <- data.frame(
 room = paste('Room', rep(c('W', 'Z'), each = 24)),
 pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
 litter = paste('Litter', rep(11:22, each = 4)),
 tx = rep(rep(c('vac', 'con'), each = 2), 12),
 stringsAsFactors = FALSE
 )
set.seed(76153)
a$lung[a$tx == 'vac'] <- round(rnorm(24, 5, 1.3), 2)
a$lung[a$tx == 'con'] <- round(rnorm(24, 7, 1.3), 2)
a <- a[-48,]

a

## ---- expdesign, echo = FALSE--------------------------------------------
kable(a, align = "c", row.names = FALSE, 
  caption = "Nested hierarchical data structure.",
  format = 'latex') %>%
  kable_styling('bordered', full_width = FALSE, font_size = 8) %>%  
  collapse_rows(columns = 1:4, valign = "middle")

## ---- results = 'hide'---------------------------------------------------
MFClusHier(formula = lung ~ tx + room/pen/litter, data = a)

## ---- results = 'hide'---------------------------------------------------
MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = 'room')

## ---- results = 'hide', message=FALSE------------------------------------
MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = c('room', 'litter', 'All'))

## ---- message = FALSE, results = 'hide'----------------------------------
mf_multiple <- MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = c('room', 'litter', 'All'))
mf_multiple

## ---- echo=FALSE---------------------------------------------------------
mf_multiple$MFnest %>% 
  mutate(MF = round(MF, 2)) %>% 
  as.data.frame

## ---- message = FALSE, results='hide'------------------------------------
thisMFh <- mf_multiple$MFh
thisMFh

## ---- echo = FALSE-------------------------------------------------------
thisMFh$coreTbl %>%
  as.data.frame

## ---- message=FALSE, eval=FALSE------------------------------------------
#  MFh(formula = lung ~ tx + room/pen/litter, data = a)

## ---- message=FALSE, echo=FALSE------------------------------------------
MFh(formula = lung ~ tx + room/pen/litter, data = a)$coreTbl %>% 
  as.data.frame(.)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  MFnest(thisMFh, which.factor = "pen")

## ---- message=FALSE, echo=FALSE------------------------------------------
MFnest(thisMFh, which.factor = "pen") %>% as.data.frame

## ---- message=FALSE, eval=FALSE------------------------------------------
#  MFClusHier(formula = lung ~ tx + room/pen/litter, data = a,
#                        which.factor = "pen")

## ---- message=FALSE, echo=FALSE------------------------------------------
MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = "pen")$MFnest %>% 
  as.data.frame

## ---- results='hide'-----------------------------------------------------
MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                             data = a)

## ---- results = 'hide'---------------------------------------------------
MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                              data = a, boot.unit = TRUE, 
                              boot.cluster = FALSE, which.factor = 'room')

## ---- results = 'hide', message=FALSE------------------------------------
MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                                  data = a, boot.unit = FALSE, 
                                  boot.cluster = TRUE,  alpha = 0.1,
                                  which.factor = c('room', 'litter', 'All'))

## ---- message = FALSE, results='hide'------------------------------------
mfboot_multiple <- MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                                  data = a, boot.unit = FALSE, 
                                  boot.cluster = TRUE,  alpha = 0.1,
                                  which.factor = c('room', 'litter', 'All'),
                                  seed = 150)
mfboot_multiple

## ---- message=FALSE, echo=FALSE------------------------------------------
mfboot_multiple$MFnestBoot$mfnest_summary %>%
  mutate(median = round(median, 2),
    etlower = round(etlower, 2),
    mf.obs = round(mf.obs, 2)) %>%
  as.data.frame

## ---- message=FALSE------------------------------------------------------
thisBootMFh <- mfboot_multiple$MFhBoot
thisBootMFh

## ---- message=FALSE, results=FALSE---------------------------------------
MFhBoot(formula = lung ~ tx + room/pen/litter, 
                                    data = a, boot.unit = FALSE, 
                                    boot.cluster = TRUE,  
                                    seed = 150)

## ---- results='hide'-----------------------------------------------------
MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                              data = a, seed = 150)

## ------------------------------------------------------------------------
MFnestBoot(thisBootMFh, which.factor = c('pen', 'All'), alpha = 0.1)

## ---- eval=FALSE---------------------------------------------------------
#  a <- data.frame(
#   room = paste('Room', rep(c('W', 'Z'), each = 24)),
#   pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
#   litter = paste('Litter', rep(11:22, each = 4)),
#   tx = rep(rep(c('vac', 'con'), each = 2), 12),
#   stringsAsFactors = FALSE
#   )
#  set.seed(76153)
#  a$lung[a$tx == 'vac'] <- round(rnorm(24, 5, 1.3), 2)
#  a$lung[a$tx == 'con'] <- round(rnorm(24, 7, 1.3), 2)
#  a <- a[-48,]

## ------------------------------------------------------------------------
sessionInfo()

