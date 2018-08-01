context("nested hierarchy")

# small even
nesteddf1 <- data.frame(litter = rep(LETTERS[1:4], each = 4),
                       sex = rep(rep(c('M', 'F'), each = 2), 4),
                       group = rep(c('vac', 'con'), 8), stringsAsFactors = FALSE)
set.seed(1234)
nesteddf1[nesteddf1$group == 'con', 'lesion'] <- rnorm(8, mean = 11, sd = 2)
set.seed(4321)
nesteddf1[nesteddf1$group == 'vac', 'lesion'] <- rnorm(8, mean = 10, sd = 1.5)

# uneven

nesteddf2 <- data.frame(room = rep(LETTERS[1:4], each = 8),
                        pen = rep(rep(1:2, each = 4), 4),
                        group = rep(rep(c('vac', 'con'), each = 2), 8), 
                        stringsAsFactors = FALSE)
set.seed(2345)
nesteddf2[nesteddf2$group == 'con', 'lesion'] <- rnorm(16, mean = 11, sd = 2)
set.seed(5432)
nesteddf2[nesteddf2$group == 'vac', 'lesion'] <- rnorm(16, mean = 10, sd = 1.5)                     
nesteddf2 <- nesteddf2[c(-3,-14, -29),]

# three levels
nesteddf3 <- data.frame(room = rep(LETTERS[1:4], each = 8),
                        pen = rep(rep(1:2, each = 4), 4),
                        litter = rep(c('123', '234', '345', '456', '567', '678', '789', '8910',
                                       '321', '432', '543', '654', '765', '876', '987', '1098'),
                                     2),
                        group = rep(rep(c('vac', 'con'), each = 2), 8),
                        stringsAsFactors = FALSE)
set.seed(2345)
nesteddf3[nesteddf3$group == 'con', 'lesion'] <- rnorm(16, mean = 11, sd = 2)
set.seed(5432)
nesteddf3[nesteddf3$group == 'vac', 'lesion'] <- rnorm(16, mean = 10, sd = 1.5)


## historical usage
mfhist1 <- MFClus(lesion ~ group + cluster(litter), nesteddf1)
mfhist2 <- MFClus(lesion ~ group + cluster(sex), nesteddf1)
mfhist3 <- MFClus(lesion ~ group + cluster(room), nesteddf2)
mfhist4 <- MFClus(lesion ~ group + cluster(pen), nesteddf2)

test_that("historical", {
  expect_identical(mfhist1$All$mf, expected = 0)
  expect_identical(mfhist2$All$mf, expected = 0.125)
  expect_equal(mfhist3$All$mf, expected = 0.3845, tolerance = 0.001)
  expect_identical(mfhist4$All$mf, expected = 0.25)
})

## hierarchal usage
## TODO: expected outcomes need to be checked. wait for output structure to be defined first.
MFClusNested(lesion ~ group + cluster(litter), nesteddf1)
MFClusNested(lesion ~ group + cluster(sex), nesteddf1)
MFClusNested(lesion ~ group + cluster(litter/sex ), nesteddf1)

MFClusNested(lesion ~ group + cluster(room), nesteddf2)
MFClusNested(lesion ~ group + cluster(pen), nesteddf2)
MFClusNested(lesion ~ group + cluster(room/pen ), nesteddf2)

MFClusNested(lesion ~ group + cluster(room), nesteddf3)
MFClusNested(lesion ~ group + cluster(pen), nesteddf3)
MFClusNested(lesion ~ group + cluster(room/pen), nesteddf3)
MFClusNested(lesion ~ group + cluster(room/pen/), nesteddf3)


