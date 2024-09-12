context("examples")

###################
## HLBoot
###################

hlboot_ex <- HLBoot(lesion~group,calflung)
complete_sep <- data.frame(lesion = c(rnorm(25, mean = .2, sd = 0.1), 
                                      rnorm(25, mean = 1, sd = 0.1)),
                           group = rep(c('con', 'vac'), each = 25))
test_that("HLBoot", {
  expect_equal(hlboot_ex$nboot, expected = 10000, tolerance = 0)
  expect_equal(hlboot_ex$alpha, expected = 0.05, tolerance = 0)
  expect_identical(hlboot_ex$compare, expected = c('con', 'vac'))
  expect_identical(hlboot_ex$rng, expected = "Mersenne-Twister")
  # Mitigated Fraction
  expect_equal(hlboot_ex$MFstat["Equal Tailed", "observed"], expected = 0.44)
  expect_equal(hlboot_ex$MFstat["Highest Density", "observed"], expected = 0.44)

  # Hodges-Lehmann
  expect_equal(hlboot_ex$HLstat["Equal Tailed", "observed"], expected = -0.07335)
  expect_equal(hlboot_ex$HLstat["Highest Density", "observed"], expected =-0.07335)

  # Quartile Differences
  expect_equal(hlboot_ex$QDIFstat["Q25", "observed"], expected = -0.041500)
  expect_equal(hlboot_ex$QDIFstat["Q50", "observed"], expected = -0.112525)
  expect_equal(hlboot_ex$QDIFstat["Q75", "observed"], expected = -0.168000)
  # Quartiles of con
  expect_equal(hlboot_ex$QXstat["Q25", "observed"], expected = 0.054000)
  expect_equal(hlboot_ex$QXstat["Q50", "observed"], expected = 0.139275)
  expect_equal(hlboot_ex$QXstat["Q75", "observed"], expected = 0.315000)

  # Quartiles of vac
  expect_equal(hlboot_ex$QYstat["Q25", "observed"], expected = 0.01250)
  expect_equal(hlboot_ex$QYstat["Q50", "observed"], expected = 0.02675)
  expect_equal(hlboot_ex$QYstat["Q75", "observed"], expected = 0.14700)

  ## TODO: if seed is made reproducible, check median, lower and upper
  
  # complete separation
  expect_message(HLBoot(lesion~group, complete_sep))
})

###################
## MFBoot
###################
mfboot_ex1 <- MFBoot(lesion~group, calflung)
complete_sep <- data.frame(lesion = c(rnorm(25, mean = .2, sd = 0.1), 
                                      rnorm(25, mean = 1, sd = 0.1)),
                           group = rep(c('con', 'vac'), each = 25))
test_that("MFBoot",  {
  expect_equal(mfboot_ex1$nboot, expected = 10000, tolerance = 0)
  expect_equal(mfboot_ex1$alpha, expected = 0.05, tolerance = 0)
  expect_identical(mfboot_ex1$compare, expected = c('con', 'vac'))
  expect_identical(mfboot_ex1$rng, expected = "Mersenne-Twister")
  expect_equal(mfboot_ex1$stat[1, 'observed'], expected = 0.4400, tolerance = 0)
  expect_equal(mfboot_ex1$stat[2, 'observed'], expected = 0.4400, tolerance = 0)
  ## TODO: if seed is made reproducible, check median, lower and upper
  
  ## complete separation
  expect_message(MFBoot(lesion~group, complete_sep))
})

###################
## MFClus
###################
mfclus_ex <- MFClus(lesion ~ group + cluster(litter), piglung)
byclus_comp <- matrix(c(25, 12, 16, 3, 1, 8, 4, 3, 15, 15, 6, 9, 12, 13, 21, 14, 2, 2,
                        10, 2, 10, 2, 0, 5, 1, 2, 9, 9, 3, 3, 6, 7, 11, 8, 1, 1,
                        0.4000000, 0.2500000, 0.8333333, 1.0000000, 0.0000000, 0.8333333, 0.5000000, 0.6666667, 0.7500000, 1.0000000, 0.7500000, 0.3333333, 1.0000000, 0.7777778, 0.9166667, 0.8888889, 1.0000000, 1.0000000,
                        5, 4, 3, 1, 1, 2, 2, 1, 3, 3, 2, 3, 3, 3, 4, 3, 1, 1,
                        5, 2, 4, 2, 3, 3, 1, 3, 4, 3, 2, 3, 2, 3, 3, 3, 1, 1,
                        -0.2, -0.5, 0.666666666666667, 1, -1, 0.666666666666667, 0, 0.333333333333333, 0.5, 1, 0.5, -0.333333333333333, 1, 0.555555555555556, 0.833333333333333, 0.777777777777778, 1, 1), 
                      nrow = 18)
colnames(byclus_comp) <- c('w', 'u', 'r', 'n1','n2', 'mf')
rownames(byclus_comp) <- c('U', 'K', 'Z', 'D', 'N', 'T', 'P', 'L', 'G', 'J', 'W', 'A', 'X', 'F', 'S', 'H', 'Y', 'E')
omitattr <- c(3, 8, 12, 17, 19, 23, 25, 26)
names(omitattr) <- c( 'M',  'Q',  'R',  'B',  'O',  'V',  'I',  'C' )
attr(omitattr, 'class') <- 'omit'
attr(byclus_comp, 'na.action') <- omitattr
test_that("MFClus", {
  expect_equal(mfclus_ex$All$w, expected = 181)
  expect_equal(mfclus_ex$All$u, expected = 90)
  expect_equal(mfclus_ex$All$r, expected = 0.6766917, tolerance = 0.0001)
  expect_equal(mfclus_ex$All$n1, expected = 50)
  expect_equal(mfclus_ex$All$n2, expected = 52)
  expect_equal(mfclus_ex$All$mf, expected = 0.3533835, tolerance = 0.0001)
  expect_equal(na.omit(mfclus_ex$byCluster), expected = byclus_comp, tolerance = 0.0001)
  ## TODO: if seed is made reproducible, check median, lower and upper
})

###################
## MFCLusBoot
###################
mfclusboot_ex1 <- MFClusBoot(lesion ~ group + cluster(litter), piglung)
mfclusboot_ex2 <- MFClusBoot(lesion ~ group + cluster(litter), piglung, 
                             boot.unit = T, b = 12, B = 12)

test_that("MFClusBoot", {
  #mfclusboot_ex1
  expect_equal(mfclusboot_ex1$nboot, expected = 10000)
  expect_equal(mfclusboot_ex1$alpha, expected = 0.05)
  expect_identical(mfclusboot_ex1$compare, expected = c('con', 'vac'))
  expect_identical(mfclusboot_ex1$rng, expected = 'Mersenne-Twister')
  expect_equal(mfclusboot_ex1$stat['Equal Tailed', 'observed'], expected = 0.3533835, tolerance = 0.00001)
  expect_equal(mfclusboot_ex1$stat['Highest Density', 'observed'], expected = 0.3533835, tolerance = 0.00001)
  expect_identical(mfclusboot_ex1$excludedClusters, expected = c('M', 'Q', 'R', 'B', 'O', 'V', 'I', 'C') )
  #mfclusboot_ex2
  expect_equal(mfclusboot_ex2$nboot, expected = 144)
  expect_equal(mfclusboot_ex2$alpha, expected = 0.05)
  expect_identical(mfclusboot_ex2$compare, expected = c('con', 'vac'))
  expect_identical(mfclusboot_ex2$rng, expected = 'Mersenne-Twister')
  expect_equal(mfclusboot_ex2$stat['Equal Tailed', 'observed'], expected = 0.3533835, tolerance = 0.00001)
  expect_equal(mfclusboot_ex2$stat['Highest Density', 'observed'], expected = 0.3533835, tolerance = 0.00001)
  expect_identical(mfclusboot_ex2$excludedClusters, expected = c('M', 'Q', 'R', 'B', 'O', 'V', 'I', 'C') )
  ## TODO: if seed is made reproducible, check median, lower and upper
})

###################
## MFmp
###################
mfmp_ex1 <- MFmp(les ~ tx + cluster(cage), mlesions, compare = c('con', 'vac'))
ex1_byclus_comp <- array(c(12, 12, 2))
dimnames(ex1_byclus_comp) <- list('byCluster' = c(1, 0, -1))
mfmp_ex2 <- MFmp(x = c(12, 12, 2))

test_that("MFmp", {
  #mfmp_ex1
  expect_equal(mfmp_ex1$ci[["point"]], expected = 0.3846154, tolerance = 0.0001)
  expect_equal(mfmp_ex1$ci[["lower"]], expected = 0.1316679, tolerance = 0.0001)
  expect_equal(mfmp_ex1$ci[["upper"]], expected = 0.6375628, tolerance = 0.0001)
  expect_equal(mfmp_ex1$x, expected = ex1_byclus_comp)
  expect_equal(mfmp_ex1$alpha, expected = 0.05)
  expect_true(mfmp_ex1$tdist)
  expect_equal(mfmp_ex1$df, expected = 24)
  #mfmp_ex2
  expect_equal(mfmp_ex2$ci[["point"]], expected = 0.3846154, tolerance = 0.0001)
  expect_equal(mfmp_ex2$ci[["lower"]], expected = 0.1316679, tolerance = 0.0001)
  expect_equal(mfmp_ex2$ci[["upper"]], expected = 0.6375628, tolerance = 0.0001)
  expect_equal(mfmp_ex2$x, expected = c(12, 12, 2))
  expect_equal(mfmp_ex2$alpha, expected = 0.05)
  expect_true(mfmp_ex2$tdist)
  expect_equal(mfmp_ex2$df, expected = 24)
})

###################
## MFr
###################
mfr_ex <- MFr(lesion~group,calflung)
test_that("MFr", {
  expect_equal(mfr_ex, expected = 0.44, tolerance = 0.001)
})

###################
## MFSubj
###################
mfsubj_ex <-  MFSubj(lesion ~ group, calflung)
subj_comp <- matrix(c(3e-05, 8e-04, 0.00095, 0.00125, 0.0055, 0.0097, 0.0125, 0.01665, 0.0201, 0.0203, 0.02325, 0.026, 0.02675, 0.0281, 0.02885, 0.0319, 0.1321, 0.144575, 0.147, 0.16325, 0.21, 0.21925, 0.292, 0.3565, 0.4615,
                      1, 2, 3, 4, 5, 6, 9, 11, 12, 13, 15, 16, 17, 18, 19, 20, 29, 31, 32, 33, 36, 37, 40, 43, 48,
                      25, 25, 25, 25, 25, 25, 23, 22, 22, 22, 21, 21, 21, 21, 21, 21, 13, 12, 12, 12, 10, 10, 8, 6, 2,
                      1, 1, 1, 1, 1, 1, 0.92, 0.88, 0.88, 0.88, 0.84, 0.84, 0.84, 0.84, 0.84, 0.84, 0.52, 0.48, 0.48, 0.48, 0.4, 0.4, 0.32, 0.24, 0.08,
                      1, 1, 1, 1, 1, 1, 0.84, 0.76, 0.76, 0.76, 0.68, 0.68, 0.68, 0.68, 0.68, 0.68, 0.04, -0.04, -0.04, -0.04, -0.2, -0.2, -0.36, -0.52, -0.84),
                    nrow = 25)
colnames(subj_comp) <- c('y', 'rank', 'u.j', 'r.j', 'mf.j')
test_that("MFSubj", {
  expect_equal(mfsubj_ex$mf, expected = 0.44, tolerance = 0.001)
  expect_equal(mfsubj_ex$x, expected = c(0.0108, 0.0121, 0.01525, 0.021005, 0.03453, 0.0445, 0.054, 0.0614, 0.066925, 0.08585, 0.1089, 0.11275, 0.139275, 0.173, 0.175, 0.2248, 0.262, 0.31, 0.315, 0.36325, 0.41775, 0.44625, 0.4525, 0.5965, 0.6065), 
               tolerance = 0.00001)
  expect_equal(mfsubj_ex$y, expected = c(3e-05, 8e-04, 0.00095, 0.00125, 0.0055, 0.0097, 0.0125, 0.01665, 0.0201, 0.0203, 0.02325, 0.026, 0.02675, 0.0281, 0.02885, 0.0319, 0.1321, 0.144575, 0.147, 0.16325, 0.21, 0.21925, 0.292, 0.3565, 0.4615),
               tolerance = 0.00001)
  expect_equal(mfsubj_ex$subj, expected = subj_comp)
  expect_identical(mfsubj_ex$compare, expected = c('con', 'vac'))
  
})
