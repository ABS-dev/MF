context("MFClusHier")

a <- data.frame(
  room = paste('Room',rep(c('W','Z'), each = 24)),
  pen = paste('Pen',rep(LETTERS[1:6], each = 8)),
  litter = paste('Litter',rep(11:22, each = 4)),
  tx = rep(rep(c('vac','con'), each = 2),12),
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24,5,1.3)
a$lung[a$tx == 'con'] <- rnorm(24,7,1.3)
thismf <- MFClusHier(lung ~ tx + room/pen/litter,a)

test_that("output", {
  
  ##MFh
  expect_s4_class(thismf$MFh, 'mfhierdata')
  expect_equal(dim(thismf$MFh$coreTbl), c(12, 10))
  expect_identical(names(thismf$MFh$coreTbl), c('room', 'pen', 'litter', 
              'con_medResp', 'con_n', 'w', 'vac_medResp', 'vac_n', 'n1n2', 'u'))
  expect_equivalent(sapply(thismf$MFh$coreTbl, class), c('character', 'character',
    'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
    'numeric'))
  expect_equal(dim(thismf$MFh$data), c(48, 5))
  expect_identical(thismf$MFh$compare, c('con', 'vac'))
  expect_identical(all.vars(thismf$MFh$formula), c('lung', 'tx', 'room', 'pen', 
                                                   'litter'))
  
  ## MFnest
  expect_identical(names(thismf$MFnest), c('variable' ,'level' ,'MF' ,'N1N2' ,
      'U' ,'con_N' ,'vac_N' ,'con_medResp' ,'vac_medResp'))
  expect_equal(as.data.frame(thismf$MFnest)[,1:7], data.frame(variable = factor("All"),
    level = "All", MF = 0.875, N1N2 = 48, U = 45, con_N = 24, vac_N = 24, 
    stringsAsFactors = FALSE),
    tolerance = 0.001)
  
})