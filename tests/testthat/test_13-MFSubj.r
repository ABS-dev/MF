test_that("output", {
  test13 <- MFSubj(lesion ~ group, calflung)

  expect_is(test13, "mfcomponents")
  expect_equal(test13$mf, 0.44)
  expect_identical(test13$compare, c("con", "vac"))
})
