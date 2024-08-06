library(PF)

# PF
# RR Score based on asymptotic CI
t = RRsc(c(4, 24, 12, 28))
# str(t)
print(t)
t$estimate

## PF interval obtained by inverting two one-sided scores tests
# this is more conservative, preserving at least alpha / 2 in each tail
RRtosst(c(4, 24, 12, 28))

## PF interval obtained by inverting one two-sided score test
RRotsst(c(4, 24, 12, 28))

# Gart-Nam score method

Table6

# this does not work and is straigt from the vignette
RRstr(cbind(y, n) ~ tx + cluster(clus), Table6, pf = F)

# this works!
RRstr(Y = table6, pf = F)

# This does not work....
RRmh(cbind(y, n) ~ tx + cluster(clus), Table6, pf = F)

# this works
RRmh(Y = table6, pf = FALSE)

# 4.1
bird.fit = glm(cbind(y, n - y) ~ tx - 1, binomial, bird)
RRor(bird.fit) # not depreciation error

phiWt(bird.fit, fit.only = FALSE)$phi

summar = summary(update(bird.fit, family = quasibinomial))
print(summar)
print(summar$disp)

RRor(phiWt(bird.fit)) # Warning here

RRor(phiWt(bird.fit, subset.factor = bird$tx)) # warning here

RRor(phiWt(bird.fit, subset.factor = bird$tx), degf = 2) # warning here

RRor(tauWt(bird.fit, subset.factor = bird$tx)) # warning

