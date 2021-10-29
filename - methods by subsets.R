# HOMESCHOOLING METHODS
# this adds a lot by subsets
# it is PART II, after "Homeschooling Methods"

# NEW DIVISION, ONLY SES & ALWAYS v. TRANSFER

# count tables by group
all <- svytable(~SES2 + ALWAYS, HOMEdesign)
all
el <- svytable(~SES2 + ALWAYS, subset(HOMEdesign, elementary_secondary==1))
el
sec <- svytable(~SES2 + ALWAYS, subset(HOMEdesign, elementary_secondary==2))
sec

# totals
sum(all)
sum(el)
sum(sec)

# percent tables by group
svytable(~SES2 + ALWAYS, HOMEdesign) / 
  sum(all)
svytable(~SES2 + ALWAYS, subset(HOMEdesign, elementary_secondary==1)) / 
  sum(el)
svytable(~SES2 + ALWAYS, subset(HOMEdesign, elementary_secondary==2)) / 
  sum(sec)

# average grade in each group
svymean(~ALLGRADEX, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~ALLGRADEX, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~ALLGRADEX, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~ALLGRADEX, subset(HOMEdesign, SES != 3 & ALWAYS != 1))


# VARIANCE TESTING: testing something...

# calculating the variance
((ClusterT[1, 1] - mean(ClusterT))^2 +
    (ClusterT[1, 2] - mean(ClusterT))^2 + 
    (ClusterT[2, 1] - mean(ClusterT))^2 + 
    (ClusterT[2, 2] - mean(ClusterT))^2)/4

# WHO HOMESCHOOLS: MOTHER: K-3
# variance: 37

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX < 4)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX < 4)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX < 4)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX < 4)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: 4-6
# variance: 194

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & (ALLGRADEX == 6 | ALLGRADEX == 4 | ALLGRADEX == 5))$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & (ALLGRADEX == 6 | ALLGRADEX == 4 | ALLGRADEX == 5))$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & (ALLGRADEX == 6 | ALLGRADEX == 4 | ALLGRADEX == 5))$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & (ALLGRADEX == 6 | ALLGRADEX == 4 | ALLGRADEX == 5))$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: 7-9
# variance: 98

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & (ALLGRADEX == 9 | ALLGRADEX == 7 | ALLGRADEX == 8))$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & (ALLGRADEX == 9 | ALLGRADEX == 7 | ALLGRADEX == 8))$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & (ALLGRADEX == 9 | ALLGRADEX == 7 | ALLGRADEX == 8))$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & (ALLGRADEX == 9 | ALLGRADEX == 7 | ALLGRADEX == 8))$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: 10-12
# variance: 98

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & (ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & (ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & (ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & (ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: ELEMENTARY
# variance: 117.8

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & elementary_secondary == 1)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & elementary_secondary == 1)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & elementary_secondary == 1)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & elementary_secondary == 1)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: SECONDARY
# variance: 98.7

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & elementary_secondary == 2)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & elementary_secondary == 2)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & elementary_secondary == 2)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & elementary_secondary == 2)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

# WHO HOMESCHOOLS: MOTHER: HIGH SCHOOL

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & (ALLGRADEX == 9 | ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & (ALLGRADEX == 9 | ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & (ALLGRADEX == 9 | ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & (ALLGRADEX == 9 | ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12))$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1


# WHO HOMESCHOOLS: MOTHER: ALL
# variance: 140
# variance of elementary & secondary done separately, combined: 108

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1))

cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1)))

# GRADES K-6

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX < 7)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX < 7)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX < 7)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX < 7)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1))

cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1)))

# combined
svymean(~HSWHOX==1, subset(HOMEdesign, elementary_secondary==1))

# GRADES 7-12

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX > 6)$HSWHOX==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX > 6)$HSWHOX==1))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX > 6)$HSWHOX==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX > 6)$HSWHOX==1))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==1

svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2))

cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2)))

# combined
svymean(~HSWHOX==1, subset(HOMEdesign, elementary_secondary==2))


# WHO HOMESCHOOLS: ONLINE TEACHER

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSWHOX==5))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSWHOX==5))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSWHOX==5))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSWHOX==5))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==5

svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1))

cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1)))


# GRADES K-6

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX < 7)$HSWHOX==5))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX < 7)$HSWHOX==5))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX < 7)$HSWHOX==5))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX < 7)$HSWHOX==5))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==5

svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1))

cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1)))

# combined
svymean(~HSWHOX==5, subset(HOMEdesign, elementary_secondary==1))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, elementary_secondary==1)))

# GRADES 7-12

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX > 6)$HSWHOX==5))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX > 6)$HSWHOX==5))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX > 6)$HSWHOX==5))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX > 6)$HSWHOX==5))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSWHOX==5

svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2))

cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2)))

# combined
svymean(~HSWHOX==5, subset(HOMEdesign, elementary_secondary==2))
cv(svymean(~HSWHOX==5, subset(HOMEdesign, elementary_secondary==2)))

# -----


# ONLINE COURSES

# ALL COURSES ONLINE: all grades

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSINTNET==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSINTNET==1))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSINTNET==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSINTNET==1))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==1

svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1))

cv(svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1)))
cv(svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1)))

# ALL COURSES ONLINE: GRADES K-6

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX < 7)$HSINTNET==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX < 7)$HSINTNET==1))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX < 7)$HSINTNET==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX < 7)$HSINTNET==1))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==1

svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1))

# ALL COURSES ONLINE: GRADES 7-12

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX > 6)$HSINTNET==1))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX > 6)$HSINTNET==1))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX > 6)$HSINTNET==1))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX > 6)$HSINTNET==1))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==1

svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2))

# SOME COURSES ONLINE: all grades

svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS != 1))

cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS != 1)))
cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS != 1)))


# SOME COURSES ONLINE: GRADES K-6

svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1))

# SOME COURSES ONLINE: GRADES 7-12

svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2))

# NO ONLINE COURSES: all grades

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSINTNET==4))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSINTNET==4))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSINTNET==4))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSINTNET==4))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==4

svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1))

cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1)))

# NO ONLINE COURSES: GRADES K-6

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX < 7)$HSINTNET==4))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX < 7)$HSINTNET==4))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX < 7)$HSINTNET==4))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX < 7)$HSINTNET==4))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==1

svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1))

cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==1)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==1)))

# NO ONLINE COURSES: GRADES 7-12

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & ALLGRADEX > 6)$HSINTNET==4))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & ALLGRADEX > 6)$HSINTNET==4))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & ALLGRADEX > 6)$HSINTNET==4))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & ALLGRADEX > 6)$HSINTNET==4))
AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")
ClusterT # HSINTNET==1

svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2))
svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2))

cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & elementary_secondary==2)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & elementary_secondary==2)))
cv(svymean(~HSINTNET==4, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & elementary_secondary==2)))


# -----

# SOURCE OF COURSES BY SUBSET

# all students enrolled in all courses online
svymean(~HSINTPUB==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE)
# all students enrolled in some courses online
svymean(~HSINTPUB==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)

# CHECKING for coefficient of variance
# all students enrolled in all courses online
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE))
cv(svymean(~HSINTK12==1, subset(HOMEdesign, HSINTNET == 1), na.rm=TRUE))
# all students enrolled in some courses online
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE))
cv(svymean(~HSINTK12==1, subset(HOMEdesign, HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE))

# SEGMENT: low/middle SES, homeschool transfer
# percent using public school or virtual academy overall
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)
# percent using public school or virtual academy, all courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE)
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE))
# percent using public school or virtual academy, some courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)
# percent using a private company overall
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)
# percent using a private company, all courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE)
# percent using a private company, some courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)


# SEGMENT: low/middle SES, always homeschooled
# percent using public school or virtual academy overall
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
# percent using public school or virtual academy, all courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET==1), na.rm=TRUE)
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET==1), na.rm=TRUE))
# percent using public school or virtual academy, some courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)
# percent using a private company overall
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
# percent using a private company, all courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET==1), na.rm=TRUE)
# percent using a private company, some courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)


# SEGMENT: high SES, homeschool transfer
# percent using public school or virtual academy overall
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
# percent using public school or virtual academy, all courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE)
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE))
# percent using public school or virtual academy, some courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)
# percent using a private company overall
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
# percent using a private company, all courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET==1), na.rm=TRUE)
# percent using a private company, some courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 1), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)


# SEGMENT: high SES, always homeschooling
# percent using public school or virtual academy overall
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
# percent using public school or virtual academy, all courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET==1), na.rm=TRUE)
# percent using public school or virtual academy, some courses online
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)
# percent using a private company overall
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
# percent using a private company, all courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET==1), na.rm=TRUE)
# percent using a private company, some courses online
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & (HSINTNET==2 | HSINTNET==3)), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 1), na.rm=TRUE)

svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTPRI==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTCOL==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTK12==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)
svymean(~HSINTIND==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1 & HSINTNET == 2 | HSINTNET == 3), na.rm=TRUE)


# -----

# ADDITIONAL WORK ON SOURCE OF ONLINE COURSES

# Source: PUBLIC SCHOOL 

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSINTPUB==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSINTPUB==1, na.rm=TRUE))
H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSINTPUB==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSINTPUB==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])

ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSINTPUB==1

svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)

cv(svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE))

# SOURCE OF ONLINE COURSES: A VIRTUAL ACADEMY

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSINTVRT==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSINTVRT==1, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSINTVRT==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSINTVRT==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSINTVRT==1

svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)

cv(svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE))

# REAL QUICK! Combine public schools and virtual academies
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE))
cv(svymean(~HSINTPUB==1 | HSINTVRT==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE))
# END real quick combo!


# SOURCE OF ONLINE COURSES: A COMPANY THAT SELLS COURSES

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSINTCMP==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSINTCMP==1, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSINTCMP==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSINTCMP==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSINTCMP==1

svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE)

cv(svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS == 1), na.rm=TRUE))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, SES == 3 & ALWAYS != 1), na.rm=TRUE))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, SES != 3 & ALWAYS != 1), na.rm=TRUE))


# TOTAL NUMBER OF ONLINE COURSE SOURCES, by group

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$total_course_source, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$total_course_source, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$total_course_source, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$total_course_source, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # total_course_source==1

svymean(~total_course_source, HOMEdesign, na.rm=TRUE)

svymean(~total_course_source, subset(HOMEdesign, SES==3 & ALWAYS==1), na.rm=TRUE)
svymean(~total_course_source, subset(HOMEdesign, SES!=3 & ALWAYS==1), na.rm=TRUE)
svymean(~total_course_source, subset(HOMEdesign, SES==3 & ALWAYS!=1), na.rm=TRUE)
svymean(~total_course_source, subset(HOMEdesign, SES!=3 & ALWAYS!=1), na.rm=TRUE)

cv(svymean(~total_course_source, subset(HOMEdesign, SES==3 & ALWAYS==1), na.rm=TRUE))
cv(svymean(~total_course_source, subset(HOMEdesign, SES!=3 & ALWAYS==1), na.rm=TRUE))
cv(svymean(~total_course_source, subset(HOMEdesign, SES==3 & ALWAYS!=1), na.rm=TRUE))
cv(svymean(~total_course_source, subset(HOMEdesign, SES!=3 & ALWAYS!=1), na.rm=TRUE))


# Homeschooling style: strictly formal curriculum

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1)$HSSTYL==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1)$HSSTYL==1, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1)$HSSTYL==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1)$HSSTYL==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSSTYL==1

# Homeschooling style: strictly formal curriculum: ELEMENTARY

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & elementary_secondary==1)$HSSTYL==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & elementary_secondary==1)$HSSTYL==1, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & elementary_secondary==1)$HSSTYL==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & elementary_secondary==1)$HSSTYL==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSSTYL==1: ELEMENTARY

# Homeschooling style: strictly formal curriculum: SECONDARY

H_SES_A <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & elementary_secondary==2)$HSSTYL==1, na.rm=TRUE))
ML_SES_A <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS==1 & elementary_secondary==2)$HSSTYL==1, na.rm=TRUE))

H_SES_T <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS!=1 & elementary_secondary==2)$HSSTYL==1, na.rm=TRUE))
ML_SES_T <- as.data.frame(mean(subset(HOME, SES<3 & ALWAYS!=1 & elementary_secondary==2)$HSSTYL==1, na.rm=TRUE))

AlwaysT <- c(H_SES_A[, 1], ML_SES_A[, 1])
TransfT <- c(H_SES_T[, 1], ML_SES_T[, 1])
ClusterT <- round((rbind(AlwaysT, TransfT)*100), digits = 1)
colnames(ClusterT) <- c("HighSES", "M.L.SES")

ClusterT # HSSTYL==1: SECONDARY

part <- subset(HOME)
table(part$SES, part$ALWAYS)

table(HOME$SES, HOME$ALWAYS)

round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

# Test for coefficient of variation: IMPORTANT
cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES<3 & ALWAYS==1 & elementary_secondary==2)), na.rm=TRUE)


# STRICTLY FOLLOWING FORMAL CURRICULUM: HSSTYL == 1

# ALL GRADES
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))

# MOSTLY FORMAL CURRICULUM, SOME INFORMAL LEARNING: HSSTYL == 2

# ALL GRADES
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))

# MOSTLY INFORMAL LEARNING: HSSTYL > 2

# ALL GRADES
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))

# do first-year homeschoolers start more formal & become more informal?
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & FIRST==1))
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & FIRST!=1))

svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & FIRST==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & ALWAYS!=1 & FIRST!=1))

# is the percent of first-year homeschoolers confounding low SES?

# HS STYLE, Low/middle SES homeschool transfers FIRST YEAR
svymean(~HSSTYL==1, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                             elementary_secondary==1))
svymean(~HSSTYL==1, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                             elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                             elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                             elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))

cv(svymean(~HSSTYL==1, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                             elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==2)))

# HS STYLE, Low/middle SES homeschool transfers NOT FIRST YEAR
svymean(~HSSTYL==1, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==1))
svymean(~HSSTYL==1, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))

# FIRST year v other transfers, internet classes (low/middle SES, 7-12)
svymean(~HSINTNET==1, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==2))
svymean(~HSINTNET==1, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==2))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==2))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==2))
svymean(~HSINTNET==4, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))
svymean(~HSINTNET==4, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))

# FIRST year v other transfers, internet classes (low/middle SES, K-6)
svymean(~HSINTNET==1, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==1))
svymean(~HSINTNET==1, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                                             elementary_secondary==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                                             elementary_secondary==1))
svymean(~HSINTNET==4, subset(HOMEdesign, FIRST==1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==1))
svymean(~HSINTNET==4, subset(HOMEdesign, FIRST!=1 & SES!=3 & ALWAYS!=1 &
                               elementary_secondary==1))

# How many middle/low SES transfers are first year v not first year?
svytable(~FIRST==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 &
                            elementary_secondary==1))
svytable(~FIRST!=1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 &
                            elementary_secondary==2))

# is disabilities affecting this? 

svymean(~disability==1, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))

svymean(~disability==1, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))

svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & 
                                 elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & 
                                 elementary_secondary==2))
cv(svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & 
                                 elementary_secondary==1)))
cv(svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & 
                                 elementary_secondary==2)))



# grades 7-12
svymean(~poverty==1, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==2))
svymean(~poverty==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~poverty==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))

svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))

# grades K-6
svymean(~poverty==1, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==1))
svymean(~poverty==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~poverty==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))

svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX<3, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))

# families without a high school diploma
# elementary school
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1)))
# secondary school
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS==1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~PARGRADEX<2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2)))

svymean(~PARGRADEX==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX==3, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
svymean(~PARGRADEX==4, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))

svymean(~PARGRADEX==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX==3, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
svymean(~PARGRADEX==4, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))


# FIRST YEAR, SOME YEARS, all grades (K-12)
# first year, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3)))
# some years, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3)))


# FIRST YEAR, SOME YEARS, 7-12
# first year, high SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==2)))
# some years, high SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==2)))
# first year, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==2)))
# some years, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==2)))

# FIRST YEAR, SOME YEARS, K-6
# first year, high SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES==3 & elementary_secondary==1)))
# some years, high SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES==3 & elementary_secondary==1)))
# first year, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST==1 & SES!=3 & elementary_secondary==1)))
# some years, low/middle SES
svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1 & SES!=3 & elementary_secondary==1)))

part <- subset(HOME, SES!=3)
table(part$ALWAYS)
part <- subset(HOME, SES!=3 & ALWAYS!=1)
table(part$FIRST)

part <- subset(HOME, SES!=3 & ALWAYS==1)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST!=1)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST==1)
table(part$HSSTYL)

part <- subset(HOME, SES!=3 & ALWAYS==1 & elementary_secondary==1)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST!=1 & elementary_secondary==1)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST==1 & elementary_secondary==1)
table(part$HSSTYL)

part <- subset(HOME, SES!=3 & ALWAYS==1 & elementary_secondary==2)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST!=1 & elementary_secondary==2)
table(part$HSSTYL)
part <- subset(HOME, SES!=3 & ALWAYS!=1 & FIRST==1 & elementary_secondary==2)
table(part$HSSTYL)

# total low/middle SES, total high SES
# IMPORTANT this is in the chart!
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3)))
# low/middle SES, elementary
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & elementary_secondary==1)))
# low/middle SES, secondary
svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES!=3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & elementary_secondary==2)))
# high SES, elementary
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & elementary_secondary==1))
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & elementary_secondary==1))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & elementary_secondary==1)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & elementary_secondary==1)))
# high SES, secondary
svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & elementary_secondary==2))
svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & elementary_secondary==2))
cv(svymean(~HSSTYL==1, subset(HOMEdesign, SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL==2, subset(HOMEdesign, SES==3 & elementary_secondary==2)))
cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES==3 & elementary_secondary==2)))


# END ANALYSIS OF LOW/MIDDLE SES HOMESCHOOL TRANSFERS HOMESCHOOL STYLE


# elementary level
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & FIRST==1 & elementary_secondary==1))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & FIRST!=1 & elementary_secondary==1))
# secondary level
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & FIRST==1 & elementary_secondary==2))
svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & FIRST!=1 & elementary_secondary==2))


cv(svymean(~HSSTYL>2, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# HOMESCHOOL GROUPS & CO-OPS

# ALL GRADES: LOCAL HOMESCHOOL GROUP
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSASSNX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS!=1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS!=1), na.rm=TRUE)

svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1), na.rm=TRUE)

svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary==2), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2), na.rm=TRUE)


# ALL GRADES: NATIONAL HOMESCHOOL ORG
svymean(~HSNATL==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSNATL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSNATL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSNATL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))

# ALL GRADES
cv(svymean(~HSNATL==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSNATL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSNATL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSNATL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))

svymean(~SES==3 & ALWAYS==1, subset(HOMEdesign, HSNATL==1))


# ----- 

# WHERE parents get physical curriculum

# public library

# ALL GRADES
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCLIBRX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# catalog or homeschool publisher

# ALL GRADES
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCHSPUBX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# local public school district

# ALL GRADES
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCPUBLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))

svymean(~HSCPUBLX==1, subset(HOMEdesign, enrolled==1))
svymean(~HSCPUBLX==1, subset(HOMEdesign, enrolled!=1))

svymean(~enrolled==1, subset(HOMEdesign, HSCPUBLX==1), na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, ALWAYS==1 & FIRST!=1))

svymean(~TOTAL, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1))
svymean(~returner==1, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1))

svytable(~TOTAL, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1))/
  sum(svytable(~TOTAL, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1)))

svytable(~TOTAL, subset(HOMEdesign, ALWAYS==1 & FIRST!=1))/
  sum(svytable(~TOTAL, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1)))

svyquantile(~TOTAL, subset(HOMEdesign, ALWAYS!=1 & FIRST!=1), c(.25,.5,.75))
svyquantile(~TOTAL, subset(HOMEdesign, ALWAYS==1 & FIRST!=1), c(.25,.5,.75))

# Homeschool convention

# ALL GRADES
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# Curriculum swap

# ALL GRADES
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCEVTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# Other homeschooling family

# ALL GRADES
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))


# Homeschool convention, curriculum swap, OR other homeschooling family

# ALL GRADES
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1)))
# ELEMENTARY
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1)))
# SECONDARY
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2)))
cv(svymean(~HSCCNVX==1 | HSCEVTX==1 | HSCFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2)))



# DISABILITIES

svytable(~disability, HOMEdesign)

svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))

svymean(~disability==1, subset(HOMEdesign, ALWAYS==1 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, ALWAYS!=1 & elementary_secondary==2))

svymean(~disability==1, subset(HOMEdesign, SES==3 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & elementary_secondary==2))

svymean(~SES==3 & ALWAYS==1, subset(HOMEdesign, disability==1))
svymean(~SES!=3 & ALWAYS==1, subset(HOMEdesign, disability==1))
svymean(~SES==3 & ALWAYS!=1, subset(HOMEdesign, disability==1))
svymean(~SES!=3 & ALWAYS!=1, subset(HOMEdesign, disability==1))

# Percent taking online courses, by disability or no, for each group
# low/middle SES transfers
svymean(~HSINTNET==1, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS!=1))
svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS!=1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS!=1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS!=1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS!=1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS!=1))
# high SES transfers
svymean(~HSINTNET==1, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS!=1))
svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS!=1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS!=1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS!=1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS!=1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS!=1))
# low/middle SES always homeschoolers
svymean(~HSINTNET==1, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS==1))
svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS==1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS==1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS==1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability==1 & SES!=3 & ALWAYS==1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability!=1 & SES!=3 & ALWAYS==1))
# high SES always homeschoolers
svymean(~HSINTNET==1, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS==1))
svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS==1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS==1))
svymean(~HSINTNET==3 | HSINTNET==2, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS==1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability==1 & SES==3 & ALWAYS==1))
svymean(~HSINTNET==4, subset(HOMEdesign, disability!=1 & SES==3 & ALWAYS==1))

# Disability or no, some or all online courses
svymean(~HSINTNET==1, subset(HOMEdesign, disability==1))
svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1))
cv(svymean(~HSINTNET==1, subset(HOMEdesign, disability==1)))
cv(svymean(~HSINTNET==1, subset(HOMEdesign, disability!=1)))

svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, disability==1))
svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, disability!=1))
cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, disability==1)))
cv(svymean(~HSINTNET==2 | HSINTNET==3, subset(HOMEdesign, disability!=1)))

# SOURCES of courses, ALL families combined
# public school
svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))
# virtual academy
svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))
# company for purchase
svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))

# JUST low middle SES transfers
# public school
svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))
# virtual academy
svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))
# company for purchase
svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4 & SES!=3 & ALWAYS!=1)))

# ALL children enrolled in all online courses
# public school
svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET==1))
svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET==1))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET==1)))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET==1)))
# virtual academy
svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET==1))
svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET==1))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET==1)))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET==1)))
# company for purchase
svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET==1))
svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET==1))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET==1)))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET==1)))

# ALL children enrolled in any online courses
# public school
svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTPUB==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))
# virtual academy
svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTVRT==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))
# company for purchase
svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4))
svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability==1 & HSINTNET!=4)))
cv(svymean(~HSINTCMP==1, subset(HOMEdesign, disability!=1 & HSINTNET!=4)))


# TEXT TO BE USED ELSEWHERE IF NEEDED

# running t-tests

HOMEdesign <- update(HOMEdesign,  high_midlow = ifelse(SES == 3, "high", "midlow"))

# all high SES to all mid-low SES
svyttest((HSINTCMP == 1) ~ high_midlow, 
         HOMEdesign,
         na.rm=TRUE)
# all always hsed to all transfers
svyttest((HSINTCMP == 1) ~ (ALWAYS==1), 
         HOMEdesign,
         na.rm=TRUE)
# high SES always to mid-low SES always
svyttest((HSINTCMP == 1) ~ high_midlow, 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)
# high SES transfers to mid-low SES transfers
svyttest((HSINTCMP == 1) ~ high_midlow, 
         subset(HOMEdesign, ALWAYS != 1),
         na.rm=TRUE)
# high SES always to high SES transfers
svyttest((HSINTCMP == 1) ~ (ALWAYS==1), 
         subset(HOMEdesign, SES == 3),
         na.rm=TRUE)
# mid-low SES always to mid-low SES transfers
svyttest((HSINTCMP == 1) ~ (ALWAYS==1), 
         subset(HOMEdesign, SES != 3),
         na.rm=TRUE)



# END DOCUMENT