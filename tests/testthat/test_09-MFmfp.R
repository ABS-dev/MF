context("MFmp")

mfmp1 <- MFmp(les ~ tx + cluster(cage), mlesions, compare = c('con', 'vac'))
mfmp2 <- MFmp(x = c(12, 12, 2))

test_that("output", {
  
  ## mfmp1
  expect_is(mfmp1, 'mfmp')
  expect_equal(mfmp1$ci, c(point = 0.3846154, lower = 0.1316679, upper = 0.6375628),
               tolerance = 0.00001)
  expect_equivalent(as.table(c('1' = 12, '0' = 12, '-1' = 2)), mfmp1$x)
  expect_identical(mfmp1$alpha, 0.05)
  expect_true(mfmp1$tdist)
  expect_equal(mfmp1$df, 24)
  expect_equal(mfmp1$what, "95% t intervals on 24 df\n")
  
  ## mfmp2
  expect_is(mfmp2, 'mfmp')
  expect_equal(mfmp2$ci, c(point = 0.3846154, lower = 0.1316679, upper = 0.6375628),
               tolerance = 0.00001)
  expect_equivalent(c(12, 12, 2), mfmp2$x)
  expect_identical(mfmp2$alpha, 0.05)
  expect_true(mfmp2$tdist)
  expect_equal(mfmp2$df, 24)
  expect_equal(mfmp2$what, "95% t intervals on 24 df\n")
})