test_that("output", {
  test12 <- MFr(lesion ~ group, calflung)

  expect_equal(test12, 0.44)
})
