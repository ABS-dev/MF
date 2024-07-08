context("MFBoot")

test_that("output", {
  set.seed(12345)
  test2 <- MFBoot(lesion ~ group, calflung)

  expect_s4_class(test2, "mfboot")
  expect_equal(test2$nboot, expected = 10000, tolerance = 0)
  expect_equal(test2$alpha, expected = 0.05, tolerance = 0)
  expect_identical(test2$compare, expected = c("con", "vac"))
  expect_identical(test2$rng, expected = "Mersenne-Twister")

  # stat
  expect_is(test2$stat, "matrix")
  expect_identical(rownames(test2$stat),
                   expected = c("Equal Tailed", "Highest Density"))
  expect_identical(colnames(test2$stat),
                   expected = c("observed", "median", "lower", "upper"))
  expect_equal(round(test2$stat[1, ], 4),
               expected = round(c(observed = 0.4400,
                                  median = 0.4496,
                                  lower = 0.1520,
                                  upper = 0.7088), 4),
               tolerance = 0.01)

  # sample
  expect_is(test2$sample, "numeric")
  expect_equal(length(test2$sample), expected = 10000, tolerance = 0.01)

})
