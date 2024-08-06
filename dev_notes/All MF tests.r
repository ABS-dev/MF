cat("\014")
rm(list = ls())
library(MF)
library(data.table)
# library(tidyr)
# library(magrittr)

b = 100
B = 100
nboot = b * B

library(tidyverse)
# test 1
test1 <- HLBoot(lesion~group, 
                calflung, 
                b = b, #
                B = B, #
                seed = 12345)

# test 2
test2 <- MFBoot(lesion~group, 
                calflung, 
                b = b, #
                B = B, #
                seed = 12345)

# test 3
test3 <- MFClus(lesion ~ group + cluster(litter), piglung)

# test 4
test4 <- MFClusBoot(lesion ~ group + cluster(litter), 
                    piglung, 
                    b = b, #
                    B = B, #
                    seed = 12345)

# test 5
a <- data.frame(
  room = paste('Room', rep(c('W', 'Z'), each = 24)), 
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
  litter = paste('Litter', rep(11:22, each = 4)), 
  tx = rep(rep(c('vac', 'con'), each = 2), 12), 
  stringsAsFactors = FALSE
  
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24, 5, 1.3)
a$lung[a$tx == 'con'] <- rnorm(24, 7, 1.3)
test5 <- MFClusBootHier(lung ~ tx + room/pen/litter, 
                        a, 
                        nboot = nboot, #
                        boot.cluster = TRUE, 
                        boot.unit = TRUE, 
                        seed = 12345)

# test 6
a <- data.frame(
  room = paste('Room', rep(c('W', 'Z'), each = 24)), 
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
  litter = paste('Litter', rep(11:22, each = 4)), 
  tx = rep(rep(c('vac', 'con'), each = 2), 12), 
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24, 5, 1.3)
a$lung[a$tx == 'con'] <- rnorm(24, 7, 1.3)
test6 <- MFClusHier(lung ~ tx + room/pen/litter, a)

# test 7
a <- data.frame(
  room = paste('Room', rep(c('W', 'Z'), each = 24)), 
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
  litter = paste('Litter', rep(11:22, each = 4)), 
  tx = rep(rep(c('vac', 'con'), each = 2), 12), 
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24, 5, 1.3)
a$lung[a$tx == 'con'] <- rnorm(24, 7, 1.3)

test7 <- MFh(lung ~ tx + room/pen/litter, a)

# test 8
a <- data.frame(
  room = paste('Room', rep(c('W', 'Z'), each = 24)), 
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
  litter = paste('Litter', rep(11:22, each = 4)), 
  tx = rep(rep(c('vac', 'con'), each = 2), 12), 
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24, 5, 1.3)
a$lung[a$tx == 'con'] <- rnorm(24, 7, 1.3)

test8 <- MFhBoot(lung ~ tx + room/pen/litter, 
                 a, 
                 nboot = nboot,  #
                 boot.cluster = TRUE, 
                 boot.unit = TRUE, 
                 seed = 12345)

# test 9
test9a <- MFmp(les ~ tx + cluster(cage), mlesions, compare = c('con', 'vac'))
test9b <- MFmp(x = c(12, 12, 2))

# test 10
a <- data.frame(
  room = paste('Room', rep(c('W', 'Z'), each = 24)), 
  pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
  litter = paste('Litter', rep(11:22, each = 4)), 
  tx = rep(rep(c('vac', 'con'), each = 2), 12), 
  stringsAsFactors = FALSE
)
set.seed(76153)
a$lung[a$tx == 'vac'] <- rnorm(24, 5, 1.3)
a$lung[a$tx == 'con'] <- rnorm(24, 7, 1.3)

aCore <- MFh(lung ~ tx + room/pen/litter, a)
test10 <- MFnest(aCore)

# test 11
set.seed(76153)
a <- tibble(room = paste('Room', rep(c('W', 'Z'), each = 24)), 
            pen = paste('Pen', rep(LETTERS[1:6], each = 8)), 
            litter = paste('Litter', rep(11:22, each = 4)), 
            tx = rep(rep(c('vac', 'con'), each = 2), 12)) %>%
  mutate(lung = ifelse(tx == 'vac', rnorm(24, 5, 1.3), rnorm(24, 7, 1.3)))

formula <- lung ~ tx + room/pen/litter
nboot <- 10000
boot.cluster <- TRUE
boot.unit <- TRUE
which.factors <- c('All', 'room', 'pen', 'litter')

thismfhboot <- MFhBoot(formula, a, 
                       nboot = nboot, #
                       boot.cluster = TRUE, 
                       boot.unit = TRUE, 
                       seed = 12345)
test11 <- MFnestBoot(thismfhboot, c('All', 'litter'))

# test 12
test12 <- MFr(lesion ~ group, calflung)

# test 13
test13 <- MFSubj(lesion ~ group, calflung)

if (version$major == 3) {
  test1.3 = test1
  test2.3 = test2
  test3.3 = test3
  test4.3 = test4
  test5.3 = test5 
  test6.3 = test6
  test7.3 = test7 
  test8.3 = test8
  test9a.3 = test9a
  test9b.3 = test9b
  test10.3 = test10
  test11.3 = test11
  test12.3 = test12
  test13.3 = test13
  save(test1.3, test2.3, test3.3, test4.3, test5.3, test6.3, test7.3,
       test8.3, test9a.3, test9b.3, test10.3, test11.3, test12.3, test13.3, 
       file = "~/CVB Packages/test.3.5.3.rdata")
} else {
  test1.4 = test1
  test2.4 = test2
  test3.4 = test3
  test4.4 = test4
  test5.4 = test5 
  test6.4 = test6
  test7.4 = test7 
  test8.4 = test8
  test9a.4 = test9a
  test9b.4 = test9b
  test10.4 = test10
  test11.4 = test11
  test12.4 = test12
  test13.4 = test13
  save(test1.4, test2.4, test3.4, test4.4, test5.4, test6.4, test7.4,
       test8.4, test9a.4, test9b.4, test10.4, test11.4, test12.4, test13.4, 
       file = "~/CVB Packages/test.4.0.3.rdata")
}






load(file = "~/CVB Packages/test.3.5.3.rdata")
load(file = "~/CVB Packages/test.4.0.3.rdata")

dt = data.table(
  test = c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8", 
    "test9a", "test9b", "test10", "test11", "test12", "test13"),
  R3 = c(object.size(test1.3), object.size(test2.3), object.size(test3.3), object.size(test4.3), 
    object.size(test5.3), object.size(test6.3), object.size(test7.3), object.size(test8.3), 
    object.size(test9a.3), object.size(test9b.3), object.size(test10.3), object.size(test11.3), 
    object.size(test12.3), object.size(test13.3)),
  R4 = c(object.size(test1.4), object.size(test2.4), object.size(test3.4), object.size(test4.4), 
    object.size(test5.4), object.size(test6.4), object.size(test7.4), object.size(test8.4), 
    object.size(test9a.4), object.size(test9b.4), object.size(test10.4), object.size(test11.4), 
    object.size(test12.4), object.size(test13.4))
)

dt[, diff := R4 - R3]

dt

test8.3
test8.4
str(test8.3)
str(test8.4)

test11.3
test11.4

str(test11.3)
str(test11.4)


test11.1 <- MFnestBoot(thismfhboot, c('All', 'litter'))
test11.2 <- MFnestBoot(thismfhboot, c('All', 'litter'))
test11.3 <- MFnestBoot(thismfhboot, c('All', 'litter'))
test11.4 <- MFnestBoot(thismfhboot, c('All', 'litter'))

object.size(test11.1)
object.size(test11.2)
object.size(test11.3)
object.size(test11.4)
str(test11.3)

for (i in 1:10) {
  print(object.size(MFhBoot(formula, a, 
                            nboot = nboot, #
                            boot.cluster = TRUE, 
                            boot.unit = TRUE, 
                            seed = i))
  )
}

MFhBoot(formula, a, 
        nboot = nboot, #
        boot.cluster = TRUE, 
        boot.unit = TRUE, 
        seed = 12346)