context("MFClusBoot")

set.seed(12345)
test4 <- MFClusBoot(lesion ~ group + cluster(litter), piglung)

test_that("output", {
  expect_s4_class(test4, 'mfbootcluster')
  expect_equal(test4$nboot, 10000)
  expect_equal(test4$alpha, 0.05)
  expect_identical(test4$compare, c('con', 'vac'))
  expect_identical(test4$rng, "Mersenne-Twister")
  expect_match(test4$what, "10000 bootstrap samples of clusters and units in treatment in cluster")
  expect_identical(test4$excludedClusters, c('M', 'Q', 'R', 'B', 'O', 'V', 'I', 'C'))
  expect_identical(all.names(test4$call), c("MFClusBoot", "~", "lesion", "+", "group",
                                            "cluster", "litter", "piglung"))
  expect_null(test4$sample)
  
  ## stat
  expect_equal(test4$stat["Equal Tailed", ], c(observed = 0.35338346,
                                                   median = 0.36486486,
                                                   lower = -0.01409471,
                                                   upper = 0.71099655),
               tolerance = 0.01)
  expect_equal(test4$stat["Highest Density", ], c(observed = 0.35338346,
                                               median = 0.3648649,
                                               lower = 0.00000000,
                                               upper = 0.7236842),
               tolerance = 0.01)
  ## All
  all4 <- data.frame(w = 181, u = 90, r = 0.6766917, n1 = 50, n2 = 52, 
                     mf = 0.3533835)
  rownames(all4) <- 'All'
  expect_equal(test4$All, all4, tolerance = 0.01)
})