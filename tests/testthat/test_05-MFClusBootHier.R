context("MFClusBootHier")

test_that("output", {

  a <- data.frame(
    room = paste("Room", rep(c("W", "Z"), each = 24)),
    pen = paste("Pen", rep(LETTERS[1:6], each = 8)),
    litter = paste("Litter", rep(11:22, each = 4)),
    tx = rep(rep(c("vac", "con"), each = 2), 12),
    stringsAsFactors = FALSE
  )
  set.seed(76153)
  a$lung[a$tx == "vac"] <- rnorm(24, 5, 1.3)
  a$lung[a$tx == "con"] <- rnorm(24, 7, 1.3)
  set.seed(12345)
  thismf1 <- MFClusBootHier(lung ~ tx + room / pen / litter, a, nboot = 10000,
                            boot.cluster = TRUE, boot.unit = TRUE)

  expect_is(thismf1, "mfclusboothier")

  ## MFhBoot
  expect_identical(names(thismf1$MFhBoot),
                   c("bootmfh", "clusters", "compare", "mfh", "seed"))
  expect_equal(dim(thismf1$MFhBoot$bootmfh), c(120000, 11))
  expect_identical(names(thismf1$MFhBoot$bootmfh),
                   c("bootID", "w", "u", "n1n2", "con_n", "vac_n",
                     "con_medResp", "vac_medResp", "room", "pen", "litter"))
  expect_equivalent(sapply(thismf1$MFhBoot$bootmfh, class),
                    c("integer", "numeric", "numeric", "integer",
                      "integer", "integer", "numeric", "numeric",
                      "character", "character", "character"))
  expect_equal(nrow(thismf1$MFhBoot$clusters), 12)
  expect_identical(names(thismf1$MFhBoot$clusters),
                   c("room", "pen", "litter", "clusterID"))
  expect_identical(thismf1$MFhBoot$compare,
                   c("con", "vac"))

  ## MFnestBoot
  expect_identical(names(thismf1$MFnestBoot),
                   c("mfnest_details", "mfnest_summary", "seed"))
  expect_equal(dim(thismf1$MFnestBoot$mfnest_details), c(10000, 8))
  expect_identical(names(thismf1$MFnestBoot$mfnest_details),
                   c("variable", "level", "bootID", "U",
                     "N1N2", "con_N", "vac_N", "MF"))
  expect_identical(names(thismf1$MFnestBoot$mfnest_summary),
                   c("variable", "level", "median", "etlower",
                     "etupper", "hdlower", "hdupper", "mf.obs"))
  expect_equivalent(sapply(thismf1$MFnestBoot$mfnest_summary, class),
                    c("factor", "character", "numeric", "numeric",
                      "numeric", "numeric", "numeric", "numeric"))

  expect_equal(as.data.frame(thismf1$MFnestBoot$mfnest_summary),
               data.frame(variable = factor("All"),
                          level = "All",
                          median = 0.917,
                          etlower = 0.583,
                          etupper = 1,
                          hdlower = 0.667,
                          hdupper = 1,
                          mf.obs = 0.875,
                          stringsAsFactors = FALSE),
               tolerance = 0.001)
})

test_that("level_types", {
  thisdata <- data.frame(location = rep(1:2, each = 4),
                         group = factor(rep(c("vacc", "con"), each = 2),
                                        levels = c("vacc", "con"),
                                        ordered = TRUE),
                         pen = c(rep(1:2, 2), rep(3:4, 2)),
                         obs = rnorm(n = 8, mean = 23.5, sd = 0.53))
  ex1 <- thisdata |>
    MFClusBootHier(formula = obs ~ group + location / pen,
                   compare = c("con", "vacc"),
                   which.factor = c("location", "All"), seed = 61889)
  expect_is(ex1, "mfclusboothier")

  ex2 <- thisdata |>
    mutate_if(is.factor, as.character) |>
    MFClusBootHier(formula = obs ~ group + location / pen,
                   compare = c("con", "vacc"),
                   which.factor = c("location", "All"), seed = 61889)
  expect_is(ex2, "mfclusboothier")

})
