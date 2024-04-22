context("MFh")


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

  a_core <- MFh(lung ~ tx + room / pen / litter, a)
  expect_is(a_core, "mfhierdata")

  ##coreTbl
  expect_equal(dim(a_core$coreTbl), c(12, 10))
  expect_equivalent(sapply(a_core$coreTbl, class),
                    c("character", "character", "character",
                      "numeric", "numeric", "numeric",
                      "numeric", "numeric", "numeric", "numeric"))
  expect_identical(a_core$coreTbl$w,
                   c(7, 5, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7))

  expect_identical(a_core$compare,
                   c("con", "vac"))

  expect_identical(all.vars(a_core$formula),
                   c("lung", "tx", "room", "pen", "litter"))

})
