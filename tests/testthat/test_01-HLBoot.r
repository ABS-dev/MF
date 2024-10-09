test_that("output", {
  set.seed(12345)
  test1 <- HLBoot(lesion ~ group, calflung)

  expect_s4_class(test1, "mfhlboot")
  expect_equal(test1$nboot, expected = 10000, tolerance = 0)
  expect_equal(test1$alpha, expected = 0.05, tolerance = 0)
  expect_identical(test1$compare, expected = c("con", "vac"))
  expect_identical(test1$rng, expected = "Mersenne-Twister")

  #MFstat
  expect_is(test1$MFstat, "matrix")
  expect_identical(colnames(test1$MFstat),
                   expected = c("observed", "median",
                                "lower", "upper"))
  expect_identical(rownames(test1$MFstat),
                   expected = c("Equal Tailed",
                                "Highest Density"))
  expect_equal(round(test1$MFstat[1, ], 4),
               expected = round(c(observed = 0.4400,
                                  median = 0.4496,
                                  lower = 0.1520,
                                  upper = 0.7088), 4),
               tolerance = 0.01)
  expect_equal(round(test1$MFstat[2, ], 4),
               expected = round(c(observed = 0.4400,
                                  median = 0.4496,
                                  lower = 0.1650,
                                  upper = 0.7220), 4),
               tolerance = 0.01)

  #HLstat
  expect_is(test1$HLstat, "matrix")
  expect_identical(colnames(test1$HLstat),
                   expected = c("observed", "median",
                                "lower", "upper"))
  expect_identical(rownames(test1$HLstat),
                   expected = c("Equal Tailed",
                                "Highest Density"))
  expect_equal(round(test1$HLstat[1, ], 4),
               expected = round(c(observed = -0.07335,
                                  median = -0.07615,
                                  lower = -0.17220,
                                  upper = -0.01565), 4),
               tolerance = 0.01)
  expect_equal(round(test1$HLstat[2, ], 4),
               expected = round(c(observed = -0.07335000,
                                  median = -0.07615000,
                                  lower = -0.15635000,
                                  upper = -0.00850065), 4),
               tolerance = 0.01)

  #QDIFstat
  expect_is(test1$QDIFstat, "matrix")
  expect_identical(colnames(test1$QDIFstat),
                   expected = c("observed", "median",
                                "lower", "upper"))
  expect_identical(rownames(test1$QDIFstat),
                   expected = c("Q25", "Q50", "Q75"))
  expect_equal(round(test1$QDIFstat[1, ], 4),
               expected = round(c(observed = -0.041500,
                                  median = -0.041500,
                                  lower = -0.103400,
                                  upper = -0.000905), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QDIFstat[2, ], 4),
               expected = round(c(observed = -0.112525,
                                  median = -0.111175,
                                  lower = -0.281150,
                                  upper = 0.019350), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QDIFstat[3, ], 4),
               expected = round(c(observed = -0.168000,
                                  median = -0.170425,
                                  lower = -0.388900,
                                  upper = 0.005300), 4),
               tolerance = 0.01)

  #QXstat
  expect_is(test1$QXstat, "matrix")
  expect_identical(colnames(test1$QXstat),
                   expected = c("observed", "median",
                                "lower", "upper"))
  expect_identical(rownames(test1$QXstat),
                   expected = c("Q25", "Q50", "Q75"))
  expect_equal(round(test1$QXstat[1, ], 4),
               expected = round(c(observed = 0.054000,
                                  median = 0.054000,
                                  lower = 0.021005,
                                  upper = 0.112750), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QXstat[2, ], 4),
               expected = round(c(observed = 0.139275,
                                  median = 0.139275,
                                  lower = 0.061400,
                                  upper = 0.310000), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QXstat[3, ], 4),
               expected = round(c(observed = 0.31500,
                                  median = 0.31500,
                                  lower = 0.17300,
                                  upper = 0.44625), 4),
               tolerance = 0.01)

  #QYstat
  expect_is(test1$QYstat, "matrix")
  expect_identical(colnames(test1$QYstat),
                   expected = c("observed", "median",
                                "lower", "upper"))
  expect_identical(rownames(test1$QYstat),
                   expected = c("Q25", "Q50", "Q75"))
  expect_equal(round(test1$QYstat[1, ], 4),
               expected = round(c(observed = 0.01250,
                                  median = 0.01250,
                                  lower = 0.00125,
                                  upper = 0.02600), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QYstat[2, ], 4),
               expected = round(c(observed = 0.026750,
                                  median = 0.026750,
                                  lower = 0.016650,
                                  upper = 0.144575), 4),
               tolerance = 0.01)
  expect_equal(round(test1$QYstat[3, ], 4),
               expected = round(c(observed = 0.14700,
                                  median = 0.14700,
                                  lower = 0.02810,
                                  upper = 0.21925), 4),
               tolerance = 0.01)

  #sample
  expect_null(test1$sample)

})
