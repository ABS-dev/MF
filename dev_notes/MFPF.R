library(data.table)
library(MF)
library(PF)
cat("\014")
# y ~ x

MFr(   lesion ~ group, calflung)               # MF
MFBoot(lesion ~ group, calflung, seed = 12345) # MF & CI
MFSubj(lesion ~ group, calflung)               # MF & Subject components
HLBoot(lesion ~ group, calflung, seed = 12345) # MF & CI & Hodges-Lehmann, Quartile Differences, etc.

# y ~ x + cluster(w)


MFClus(lesion ~ group + cluster(litter), piglung)                    # MF & clusters
MFClusBoot(lesion ~ group + cluster(litter), piglung, seed = 12345)  # MF & CI

#HLBoot(lesion ~ group + cluster(litter), piglung, seed = 12345) # Does not work even thought Help says it should.

MFmp(les ~ tx + cluster(cage), mlesions, vac_grp = "vac", con_grp = "con")  # MF (matched pairs) & CI  No Boot to get CI?
MFmp(x = c(12, 12, 2)) # This seems like silly interface to me.  Why compute this?
mlesions
dt = dcast(as.data.table(mlesions), cage ~ tx)
dt[, .(sum(vac < con), sum(vac == con), sum(vac > con))]


# y ~ x + a/b/c

set.seed(76153)
data <- data.table(
  room   = paste('Room',   rep(c('W', 'Z'),  each = 24)),
  pen    = paste('Pen',    rep(LETTERS[1:6], each =  8)),
  litter = paste('Litter', rep(11:22,        each =  4)),
  tx     = rep(rep(c('vac','con'), each = 2), 12)
)
data[tx == "vac", lung := rnorm(24, 5, 1.3)]
data[tx == "con", lung := rnorm(24, 7, 1.3)]


MFClusHier(lung ~ tx + room/pen/litter, data)                     # MF & summary stats (cryptic output)
MFClusBootHier(lung ~ tx + room/pen/litter, data,                 # MF & CI (cryptic output)
               nboot = 10000, boot.cluster = TRUE, boot.unit = TRUE, seed = 12345)

(aCore = MFh(lung ~ tx + room/pen/litter, data))  # This look similar to all above, maybe with subject level information?
                                                  # Identify Ranks
MFnest(aCore)
MFnest(aCore$coreTbl)
MFnest(aCore, 'room')
MFnest(aCore, "pen")
MFnest(aCore, c("All", "litter"))
MFnest(aCore, "litter")
MFnest(aCore, c("room", "pen", "litter"))


(test1 = MFhBoot(lung ~ tx + room/pen/litter, data,  # MFh & CI
                 nboot = 10000, boot.cluster = TRUE, boot.unit = TRUE, seed = 12345))

MFnestBoot(test1, c('All', 'litter'))  ??

#


#


#


#

y_vector <- c(26, 204, 10, 205)
IDRlsi(y_vector, pf = FALSE)

y_matrix <- matrix(c(26, 178, 10, 195), 2, 2, byrow = TRUE)
y_matrix

IDRlsi(y_matrix, pf = FALSE)

data1 <- data.table(group = rep(c("treated", "control"), each = 5),
                    n = c(rep(41, 4), 40, rep(41, 5)),
                    y = c(4, 5, 7, 6, 4, 1, 3, 3, 2, 1),
                    cage = rep(paste('cage', 1:5), 2))

#works
IDRlsi(data = data1, formula = cbind(y, n) ~ group,
       vac_grp = "treated", con_grp = "control", pf = FALSE)

data2 = data1[, .(sum_y = sum(y), sum_n = sum(n)), by = group]

IDRlsi(data = data2, formula = cbind(sum_y, sum_n) ~ group,
       vac_grp = "treated", con_grp = "control", pf = FALSE)

#

IDRsc(y_vector, pf = FALSE)

IDRsc(y_matrix, pf = FALSE)

#works
IDRsc(data = data1, formula = cbind(y, n) ~ group,
      vac_grp = "treated", con_grp = "control", pf = FALSE)

IDRsc(data = data2, formula =  cbind(sum_y, sum_n) ~ group,
      vac_grp = "treated", con_grp = "control", pf = FALSE)

#

y_vector <- c(4, 24, 12, 28)
RRsc(y_vector, pf = FALSE)

y_matrix <- matrix(c(4, 20, 12, 16), 2, 2, byrow = TRUE)
RRsc(y_matrix, pf = FALSE)

data1 <- data.table(group = rep(c("treated", "control"), each = 2),
                    y = c(1, 3, 7, 5),
                    n = c(12, 12, 14, 14),
                    cage = rep(paste('cage', 1:2), 2))

RRsc(data = data1, formula = cbind(y, n) ~ group,
     vac_grp = "treated", con_grp = "control", pf = FALSE)

data2 = data1[, .(sum_y = sum(y), sum_n = sum(n)), by = group]

RRsc(data = data2, formula = cbind(sum_y, sum_n) ~ group,
     vac_grp = "treated", con_grp = "control", pf = FALSE)

#

RRlsi(y_vector, pf = FALSE)

RRlsi(y_matrix, pf = FALSE)

RRlsi(data = data1, formula = cbind(y, n) ~ group,
      vac_grp = "treated", con_grp = "control", pf = FALSE)

RRlsi(data = data2, formula =  cbind(sum_y, sum_n) ~ group,
      vac_grp = "treated", con_grp = "control", pf = FALSE)

#

RRmh(cbind(y,n) ~ tx + cluster(clus), Table6, vac_grp = "b", con_grp = "a", pf = FALSE)

RRmh(Y = table6, pf = FALSE)

#

# Note the error when this is run
RRmpWald(pos ~ tx + cluster(cage), New, vac_grp = "con", con_grp = "vac")

New1 = as.data.table(New)
New1[, pos := factor(pos, levels = 1:0)]
(thistable = dcast(New1, cage ~ tx)[, table(vac, con)])
as.vector(thistable)
RRmpWald(x = as.vector(thistable))

#

bird.fit <- glm(cbind(y, n - y) ~ tx - 1, binomial, bird)
RRor(tauWt(bird.fit))
RRor(phiWt(bird.fit))
