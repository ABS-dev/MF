library(MF)
# library(tidyr)
# library(magrittr)

k = 10
n = 10

results = vector(length = k, mode = "list")
for (i in 1:k) {
  results[[i]] = vector(length = n, mode = "list")
}

t3 = proc.time()[3]
for (i in 1:k) {
  t2 = proc.time()[3]
  for (j in 1:n) {
    t1 = proc.time()[3]
    results[[i]][[j]] = HLBoot(lesion~group, calflung, b = i * 100, B = 200)
    cat(proc.time()[3] - t1, "\n")
  }
  cat(proc.time()[3] - t2, "\n")
}
cat(proc.time()[3] - t3, "\n")

temp = vector(length = k, mode = "list")
for (i in 1:k) {
  QDIFstat = QXstat = HLstat = QYstat = MFstat = 0
  for (j in 1:n) {
    QDIFstat = QDIFstat + results[[i]][[j]]$QDIFstat
    QXstat   = QXstat   + results[[i]][[j]]$QXstat 
    HLstat   = HLstat   + results[[i]][[j]]$HLstat  
    QYstat   = QYstat   + results[[i]][[j]]$QYstat  
    MFstat   = MFstat   + results[[i]][[j]]$MFstat
  }
  temp[[i]]$QDIFstat = QDIFstat / n  
  temp[[i]]$QXstat = QXstat / n
  temp[[i]]$HLstat = HLstat / n
  temp[[i]]$QYstat = QYstat / n
  temp[[i]]$MFstat = MFstat / n
}

extreme = function(t1, t2) {
  res = t1
  idx = which(abs(t2) > abs(t1))
  res[idx] = t2[idx]
  return(res)
}


for (i in 1:k) {
  cat("\nb = B = ", i * 100, "\n\n")
  QDIFstat = temp[[i]]$QDIFstat - temp[[i]]$QDIFstat
  QXstat   = temp[[i]]$QXstat   - temp[[i]]$QXstat
  HLstat   = temp[[i]]$HLstat   - temp[[i]]$HLstat
  QYstat   = temp[[i]]$QYstat   - temp[[i]]$QYstat
  MFstat   = temp[[i]]$MFstat   - temp[[i]]$MFstat
  for (j in 1:n) {
    QDIFstat = extreme(QDIFstat, results[[i]][[j]]$QDIFstat - temp[[i]]$QDIFstat)
    QXstat   = extreme(QXstat,   results[[i]][[j]]$QXstat   - temp[[i]]$QXstat)
    HLstat   = extreme(HLstat,   results[[i]][[j]]$HLstat   - temp[[i]]$HLstat)
    QYstat   = extreme(QYstat,   results[[i]][[j]]$QYstat   - temp[[i]]$QYstat)
    MFstat   = extreme(MFstat,   results[[i]][[j]]$MFstat   - temp[[i]]$MFstat)
  }
  # print(round(QDIFstat, 3))
  # print(round(QXstat, 3))
  # print(round(HLstat, 3))
  # print(round(QYstat, 3))
  # print(round(MFstat, 3))
  cat("QDIFstat:", max(abs(round(QDIFstat, 2))), round(max(QDIFstat) - min(QDIFstat), 3), "\n")
  cat("QXstat:  ", max(abs(round(QXstat,   2))), round(max(QXstat)   - min(QXstat),   3), "\n")
  cat("HLstat:  ", max(abs(round(HLstat,   2))), round(max(HLstat)   - min(HLstat),   3), "\n")
  cat("QYstat:  ", max(abs(round(QYstat,   2))), round(max(QYstat)   - min(QYstat),   3), "\n")
  cat("MFstat:  ", max(abs(round(MFstat,   2))), round(max(MFstat)   - min(MFstat),   3), "\n")
  
  cat("\n\n")
}



library(data.table)
dt = CJ(b = 1:10,
        B = 1:10,
        time = 0)
dt = rbind(dt, dt, dt)
dt[, r := runif (nrow(dt))]
setorder(dt, r)
dt[, r := NULL]
for (i in 1:nrow(dt)) {
  cat(i)
  dt[i, time := system.time(HLBoot(lesion~group, calflung, b = dt[i, b * 100], B = dt[i, B * 100]))[3]]
}

# b = 16.9544
# B = 18.3654

fit = dt[, lm(time ~ b + B)]
summary(fit)
save(dt, file = "dt.rdata")

fit = dt[, lm(time ~ b + B + b:B + I(B^2) + I(b^2))]
summary(fit)
fit = dt[, lm(time ~ b + B + b:B + I(B^2))]
summary(fit)
fit = dt[, lm(time ~ b + B + b:B)]
summary(fit)
