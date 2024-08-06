# library(MF)
# t1 = proc.time()
# test1 <- HLBoot(lesion~group, calflung, b = 1000, B = 2000, seed = 12345)
# test1
# proc.time() - t1

library(data.table)
library(ggplot)

dt = data.table(t1 = 1:10, t2 = 1:10)

n = 100
for (i in 1:10) {
  cat(i, "\n")
  t1 = proc.time()[3]
  for (B in 1:(n * 100)) {
    x = mean(sample(1:50, 20000, replace = TRUE))
  }
  t1 = proc.time()[3] - t1
  t2 = proc.time()[3]
  for (B in 1:n) {
    x = replicate(100, mean(sample(1:50, 20000, replace = TRUE)))
  }
  t2 = proc.time()[3] - t1
  dt[i, c("t1", "t2") := .(t1, t2)]
}

dt
