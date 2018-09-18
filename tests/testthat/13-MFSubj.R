context("MFSubj")

test13 <- MFSubj(lesion ~ group, calflung)

test_that("output", {
  expect_is(test13, 'mfcomponents')
  expect_equal(test13$mf, 0.44)
  expect_identical(test13$compare, c('con', 'vac'))
})