test_that("output", {
  a <- data.frame(
    room = paste("Room", rep(c("W", "Z"), each = 24)),
    pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
    litter = paste("Litter", rep(11:22, each = 4)),
    tx = rep(rep(c("vac", "con"), each = 2), 12)
  )
  set.seed(76153)
  a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
  a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)

  formula <- lung ~ tx + room / pen / litter
  set.seed(12345)
  test8 <- MFhBoot(formula, a,
                   nboot = 10000,
                   boot.cluster = TRUE,
                   boot.unit = TRUE)

  expect_identical(names(test8),
                   c("bootmfh", "clusters", "compare", "mfh", "seed"))

  ## bootmfh
  expect_equal(dim(test8$bootmfh), c(120000, 11))
  expect_equivalent(sapply(test8$bootmfh, class),
                    c("integer", "numeric", "numeric", "integer",
                      "integer", "integer", "numeric", "numeric",
                      "character", "character", "character"))

  ## clusters
  expect_equal(nrow(test8$clusters), 12)
  expect_identical(names(test8$clusters),
                   c("room", "pen", "litter", "clusterID"))

  ## compare
  expect_identical(test8$compare,
                   c("con", "vac"))

  #' @note aCore table not found?
  #' @note expect_equal(test8$mfh$coreTbl, aCore$coreTbl)
  #' @noRd


})
