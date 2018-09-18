## ----setup, message=FALSE------------------------------------------------
require(tidyverse)
require(MF)

## example data
a <- data.frame(
 room = paste('Room',rep(c('W','Z'),each=24)),
 pen = paste('Pen',rep(LETTERS[1:6],each=8)),
 litter = paste('Litter',rep(11:22,each=4)),
 tx = rep(rep(c('vac','con'),each=2),12),
 stringsAsFactors = FALSE
 )
set.seed(76153)
a$lung[a$tx=='vac'] <- rnorm(24,5,1.3)
a$lung[a$tx=='con'] <- rnorm(24,7,1.3)
a <- a[-48,]
a

## ---- results = 'hide'---------------------------------------------------
mf_all <- MFClusHier(formula = lung ~ tx + room/pen/litter, data = a)

## ---- results = 'hide'---------------------------------------------------
mf_room <- MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = 'room')

## ---- results = 'hide', message=FALSE------------------------------------
mf_multiple <- MFClusHier(formula = lung ~ tx + room/pen/litter, data = a, 
                      which.factor = c('room', 'litter', 'All'))

## ------------------------------------------------------------------------
mf_all

## ---- results='hide'-----------------------------------------------------
mfboot_all <- MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                             data = a)

## ---- results = 'hide'---------------------------------------------------
mfboot_room <- MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                              data = a, boot.unit = TRUE, 
                              boot.cluster = FALSE, which.factor = 'room')

## ---- results = 'hide', message=FALSE------------------------------------
mfboot_multiple <- MFClusBootHier(formula = lung ~ tx + room/pen/litter, 
                                  data = a, boot.unit = FALSE, 
                                  boot.cluster = TRUE,  alpha = 0.1,
                                  which.factor = c('room', 'litter', 'All'))

## ------------------------------------------------------------------------
mfboot_all

## ------------------------------------------------------------------------
mf_all$MFh

## ------------------------------------------------------------------------
MFnest(mf_all$MFh)

## ------------------------------------------------------------------------
MFnest(mf_all$MFh, which.factor = 'room')
MFnest(mf_all$MFh, which.factor = c('room', 'litter', 'All'))

## ------------------------------------------------------------------------
mfboot_all$MFhBoot

## ------------------------------------------------------------------------
MFnestBoot(mfboot_all$MFhBoot)

## ------------------------------------------------------------------------
MFnestBoot(mfboot_all$MFh, which.factor = 'room')
MFnestBoot(mfboot_all$MFh, which.factor = c('room', 'litter', 'All'), 
           alpha = 0.1)

## ------------------------------------------------------------------------
MFh(formula = lung ~ tx + room/pen/litter, data = a)

## ------------------------------------------------------------------------
thismf <- MFh(formula = lung ~ tx + room/pen/litter, data = a)
MFnest(thismf)

## ------------------------------------------------------------------------
MFhBoot(formula = lung ~ tx + room/pen/litter, data = a, nboot = 10000,
  boot.unit = TRUE, boot.cluster = TRUE)

## ------------------------------------------------------------------------
thismfboot <- MFhBoot(formula = lung ~ tx + room/pen/litter, data = a)
MFnestBoot(thismfboot)

## ------------------------------------------------------------------------
sessionInfo()

