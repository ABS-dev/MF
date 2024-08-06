progress = function(i, total) {
  p1 = floor(100 * (i - 1) / total)
  p2 = floor(100 * i / total)
  if (p1 < p2 | i == 1) {
    cat(paste0("\r", p2, "%"))
  }
}

total = 20000
for (i in 1:total) {
  r = runif(total)
  progress(i, total)
}
