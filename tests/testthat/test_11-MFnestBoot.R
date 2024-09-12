context("MFnestBoot")

set.seed(76153)
a <- data_frame(room = paste('Room', rep(c('W','Z'), each = 24)),
                pen = paste('Pen', rep(LETTERS[1:6], each = 8)),
                litter = paste('Litter', rep(11:22, each = 4)),
                tx = rep(rep(c('vac', 'con'), each = 2), 12)) %>%
  mutate(lung = ifelse(tx == 'vac', rnorm(24, 5, 1.3), rnorm(24, 7, 1.3)))
a

formula <- lung ~ tx + room/pen/litter
nboot <- 10000
boot.cluster <- TRUE
boot.unit <- TRUE
which.factors <- c('All', 'room', 'pen', 'litter')

#################
set.seed(12345)

thismfhboot <- MFhBoot(formula, a, 
                 nboot = 10000,
                 boot.cluster = TRUE, boot.unit = TRUE)
test11 <- MFnestBoot(thismfhboot, c('All', 'litter'))

test_that("output", {
  
  expect_identical(names(test11), c('mfnest_details', 'mfnest_summary', 'seed'))
  ##mfnest_details
  expect_equal(dim(test11$mfnest_details), c(87753, 8))
  expect_identical(names(test11$mfnest_details), c('variable', 'level', 
                  'bootID', 'U', 'N1N2', 'con_N', 'vac_N', 'MF'))
  ##mfnest_summary
  # expect_equal(dim(test11$mfnest_summary), c(13, 8))
  # expect_equal(test11$mfnest_summary$median, c(0.83333, rep(1, 12)), 
  #   tolerance = 0.1)
  # expect_equal(test11$mfnest_summary$etlower, c(0.5, -.5, 
          # rep(-1, 6), -0.308333, -0.5, -1, -0.5, -1), tolerance = 0.1)
})
  