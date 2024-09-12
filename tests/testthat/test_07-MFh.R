context('MFh')
a <- data.frame(
  room = paste('Room', rep(c('W','Z'), each = 24)),
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
  litter = paste('Litter', rep(11:22, each = 4)),
  tx = rep(rep(c('vac','con'), each = 2),12),
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24,5,1.3)
a$lung[a$tx == 'con'] <- rnorm(24,7,1.3)

aCore <- MFh(lung ~ tx + room/pen/litter,a)

test_that("output", {
  
  expect_is(aCore, 'mfhierdata')
  
  ##coreTbl
  expect_equal(dim(aCore$coreTbl), c(12, 10))
  expect_equivalent(sapply(aCore$coreTbl, class), c('character', 'character', 
    'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
    'numeric', 'numeric'))
  expect_identical(aCore$coreTbl$w, c(7, 5, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7))
  
  expect_identical(aCore$compare, c('con', 'vac'))
  
  expect_identical(all.vars(aCore$formula), c('lung', 'tx', 'room', 'pen', 'litter'))
  
})