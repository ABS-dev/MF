context("MFr")

test12 <- MFr(lesion ~ group, calflung)

test_that("output", {
  expect_equal(test12, 0.44)
  
})