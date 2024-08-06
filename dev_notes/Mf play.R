library(MF)
library(data.table)
library(ggplot2)


n = 50000
grp = 20

data = data.table(lesion = numeric(2 * grp),
                  group = rep(c("con", "vac"), each = grp))

dt_unif = data.table()
dt_norm = data.table()


for (i in 1:n) {
  cat(i, " ")
  data[, lesion := runif(2 * grp)]
  temp = MFBoot(lesion~group, data)
  dt_unif = rbind(dt_unif, as.data.table(temp$stat)[2])
  data[, lesion := rnorm(2 * grp)]
  temp = MFBoot(lesion~group, data)
  dt_norm = rbind(dt_norm, as.data.table(temp$stat)[2])
}


dt = rbind(data.table(dt_unif, type = "unif"),
           data.table(dt_norm, type = "norm"))

colors = c("red", "blue")
names(colors) = c("unif", "norm")

ggplot(dt, aes(x = observed, fill = type)) + 
  geom_histogram(alpha = 0.5, position="identity", bins = 50) +
  scale_fill_manual(values = colors)



dt_norm
dt_unif
dt_norm[, summary(observed)]
dt_unif[, summary(observed)]

dt_norm[abs(observed) > 0.2, .N] / n * 100
dt_unif[abs(observed) > 0.2, .N] / n * 100
dt_norm[lower * upper > 0, .N] / n * 100
dt_unif[lower * upper > 0, .N] / n * 100
dt_unif[lower > .15]
dt_norm[lower > .15]

