context("MFClus")

require(tidyverse)

test3 <- MFClus(lesion ~ group + cluster(litter), piglung)


test_that("output", {
  expect_s4_class(test2, "mfcluster")
  ## All
  expect_is(test3$All, 'data.frame')
  all3 <- data.frame(w = 181, u = 90, r = 0.6766917, n1 = 50, n2 = 52, 
                     mf = 0.3533835)
  rownames(all3) <- 'All'
  expect_equal(round(test3$All, 5), round(all3, 5))
  
  ##byCluster
  by3 <- test3$byCluster %>% as.data.frame() %>% drop_na()
  expect_equal(nrow(by3), 18)
  expect_identical(names(by3), c('w', 'u', 'r', 'n1', 'n2', 'mf'))
  expect_identical(rownames(by3), c('U', 'K', 'Z', 'D', 'N', 'T', 'P', 'L', 'G', 
'J', 'W', 'A', 'X', 'F', 'S', 'H', 'Y', 'E'))
  
  ## excludedClusters
  expect_identical(test3$excludedClusters, c('M', 'Q', 'R', 'B', 'O', 
                                             'V', 'I', 'C'))
  ## call
  expect_identical(all.names(test3$call), c("MFClus", "~", "lesion", "+", "group",
                                            "cluster", "litter", "piglung"))
  
  ## compare
  expect_identical(test3$compare, c('con', 'vac'))
}  )
  