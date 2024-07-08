context("MFnest")


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
  test10 <- MFnest(a_core)

  expect_equal(as.data.frame(test10)[, 1:7],
               data.frame(variable = factor("All"),
                          level = "All",
                          MF = 0.875, N1N2 = 48,
                          U = 45, con_N = 24, vac_N = 24))
})
