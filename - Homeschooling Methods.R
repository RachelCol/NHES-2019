# Homeschooling Methods

PFIdesign <- update(PFIdesign,  home_public = ifelse(SCHTYPE==3, "home", ifelse(SCHTYPE==1, "public", NA)))
HOMEdesign <- update(HOMEdesign,  low_high = ifelse(SES == 1, "low", ifelse(SES == 3, "high", NA)))
HOMEdesign <- update(HOMEdesign,  lm_high = ifelse(SES < 3, "lm", "high"))


# Questions for each item: 
# -- Does it differ by SES?
# -- Does it differ by reasons for homeschooling? 
#     -- HSRELGON, disability, HSDISSATX, HSSAFETYX

# create tables by categories

HSA <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==1 & HSRELGON==1)$HSINTNET==4))
MSA <- as.data.frame(mean(subset(HOME, SES==2 & ALWAYS==1 & HSRELGON==1)$HSINTNET==4))
LSA <- as.data.frame(mean(subset(HOME, SES==1 & ALWAYS==1 & HSRELGON==1)$HSINTNET==4))

HSS <- as.data.frame(mean(subset(HOME, SES==3 & ALWAYS==0 & FIRST==0 & HSRELGON==1)$HSINTNET==4))
MSS <- as.data.frame(mean(subset(HOME, SES==2 & ALWAYS==0 & FIRST==0 & HSRELGON==1)$HSINTNET==4))
LSS <- as.data.frame(mean(subset(HOME, SES==1 & ALWAYS==0 & FIRST==0 & HSRELGON==1)$HSINTNET==4))

HSF <- as.data.frame(mean(subset(HOME, SES==3 & FIRST==1 & ALLGRADEX>0 & HSRELGON==1)$HSINTNET==4))
MSF <- as.data.frame(mean(subset(HOME, SES==2 & FIRST==1 & ALLGRADEX>0 & HSRELGON==1)$HSINTNET==4))
LSF <- as.data.frame(mean(subset(HOME, SES==1 & FIRST==1 & ALLGRADEX>0 & HSRELGON==1)$HSINTNET==4))

AlwaysT <- c(HSA[, 1], MSA[, 1], LSA[, 1])
SomeT <- c(HSS[, 1], MSS[, 1], LSS[, 1])
FirstT <- c(HSF[, 1], MSF[, 1], LSF[, 1])
ClusterT <- round((rbind(AlwaysT, SomeT, FirstT)*100), digits = 1)
colnames(ClusterT) <- c("HSES", "MSES", "LSES")

# ALL HSRELGON==1
ClusterT # HSINTNET==4
# END CHART CREATION

# TOTAL NUMBERS IN EACH CATEGORY

svytotal(~SES == 3 & ALWAYS == 1, HOMEdesign)
svytotal(~SES == 2 & ALWAYS == 1, HOMEdesign)
svytotal(~SES == 1 & ALWAYS == 1, HOMEdesign)
svytotal(~ALWAYS == 1, HOMEdesign)

svytotal(~SES == 3 & ALWAYS == 0 & FIRST == 0, HOMEdesign)
svytotal(~SES == 2 & ALWAYS == 0 & FIRST == 0, HOMEdesign)
svytotal(~SES == 1 & ALWAYS == 0 & FIRST == 0, HOMEdesign)
svytotal(~ALWAYS == 0 & FIRST == 0, HOMEdesign)

svytotal(~SES == 3 & FIRST == 1 & ALWAYS == 0, HOMEdesign)
svytotal(~SES == 2 & FIRST == 1 & ALWAYS == 0, HOMEdesign)
svytotal(~SES == 1 & FIRST == 1 & ALWAYS == 0, HOMEdesign)
svytotal(~FIRST == 1 & ALWAYS == 0, HOMEdesign)


HOME$group <- ifelse(HOME$ALWAYS == 1 & HOME$SES == 3, 1,
                     ifelse(HOME$SES == 3, 2,
                            ifelse(HOME$ALWAYS == 1, 3, 4)))

# test with the outputs
mean(subset(HOME, group==1)$HSINTNET==4)
mean(subset(HOME, group==2)$HSINTNET==4)
mean(subset(HOME, group==3)$HSINTNET==4)
mean(subset(HOME, group==4)$HSINTNET==4)

mean(subset(HOME, group==1)$disability==1)
mean(subset(HOME, group==2)$disability==1)
mean(subset(HOME, group==3)$disability==1)
mean(subset(HOME, group==4)$disability==1)

mean(subset(HOME, group==1)$HSCLIBRX==1)
mean(subset(HOME, group==2)$HSCLIBRX==1)
mean(subset(HOME, group==3)$HSCLIBRX==1)
mean(subset(HOME, group==4)$HSCLIBRX==1)






# SOMETHING ELSE STARTS HERE?

part <- subset(HOME, ALWAYS != 1 & TOTAL < 3 & SES < 3 & HSRELGON == 1)
round(wpct(part$elementary_secondary, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS != 1 & TOTAL < 3 & SES < 3 & HSRELGON != 1)
round(wpct(part$elementary_secondary, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, HSRELGON == 1)
round(wpct(part$elementary_secondary, weight=part$FPWT, na.rm=TRUE), digits = 3)

# result: for low or middle SES homeschool transfers in their 1st or 2nd 
# year of homeschooling, having religious reasons for homeschooling actually
# makes one LESS likely to be similar to all religiously motivated hsers
# on some metrics, including HSINTNET, HSINTVRT, and disability However, they 
# are more like religious homeschoolers in being more in grades K-6 (but
# not as MUCH more as all religious homeschoolers). 
# This is likely because the "all religious hsers" total is being impacted
# primarily by the high number of high SES parents.

# here is the metric I'm playing with:
ALWAYS != 1 & TOTAL < 3 & SES < 3
# these homeschoolers comprise 23.3% of all homeschoolers
svymean(~ALWAYS != 1 & TOTAL < 3 & SES < 3, HOMEdesign)

# QUESTION: are there differences between low & middle SES ppl who have
# hsed 3+ years & those who have homeschooled 1 or 2 yrs?

part <- subset(HOME, TOTAL < 3 & SES < 3 & elementary_secondary == 2)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, TOTAL > 2 & SES < 3 & elementary_secondary == 2)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

svymean(~disability == 1, subset(HOMEdesign, TOTAL < 2 & SES < 3 & elementary_secondary == 2))
svymean(~disability == 1, subset(HOMEdesign, TOTAL > 1 & SES < 3 & elementary_secondary == 2))
svymean(~disability == 1, subset(HOMEdesign, SES == 3))

svymean(~HSENRL == 1 | school > 0, subset(HOMEdesign, TOTAL < 3 & SES < 3 & elementary_secondary == 2))
svymean(~HSENRL == 1 | school > 0, subset(HOMEdesign, TOTAL > 2 & SES < 3 & elementary_secondary == 2))
svymean(~HSENRL == 1 | school > 0, subset(HOMEdesign, SES == 3))

svymean(~HSRELGON == 1, subset(HOMEdesign, TOTAL < 3 & SES < 3 & elementary_secondary == 2))
svymean(~HSRELGON == 1, subset(HOMEdesign, TOTAL > 2 & SES < 3 & elementary_secondary == 2))

svymean(~sahp == 1, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~sahp == 1, subset(HOMEdesign, SES == 3 & ALWAYS == 0 & FIRST == 0))
svymean(~sahp == 1, subset(HOMEdesign, SES == 3 & FIRST == 1 & ALLGRADEX > 0))



# in grades 7-12, low & middle SES ppl who have hsed only 1 or 2 yrs
# are more likely to be nonwhite, compared w/ those hsed for 3+ yrs

HOME$school

# JUST GRADES K-6

# of all grades 7-12, these homeschoolers comprise 31.0%
svymean(~ALWAYS != 1 & TOTAL < 3 & SES < 3, subset(HOMEdesign, elementary_secondary == 1))

# of all grades 7-12, high SES homeschoolers comprise 31.2%
svymean(~SES == 3, subset(HOMEdesign, elementary_secondary == 1))

# the remainder are low or middle SES ppl who have been hsing >2 years
svymean(~SES < 3 & (TOTAL > 2 | (ALWAYS == 1 & TOTAL < 3)), subset(HOMEdesign, elementary_secondary == 1))

# THESE THREE ARE ALL OF GRADES K-6

# JUST GRADES 7-12

# of all grades 7-12, these homeschoolers comprise 31.0%
svymean(~ALWAYS != 1 & TOTAL < 3 & SES < 3, subset(HOMEdesign, elementary_secondary == 2))

# of all grades 7-12, high SES homeschoolers comprise 31.2%
svymean(~SES == 3, subset(HOMEdesign, elementary_secondary == 2))

# the remainder are low or middle SES ppl who have been hsing >2 years
svymean(~TOTAL > 2 & SES < 3, subset(HOMEdesign, elementary_secondary == 2))

# THESE THREE ARE ALL OF GRADES 7-12



# ---

# FIRST QUESTION: Who provides the instruction? HSWHOX

round(wpct(HOME$HSWHOX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, HOMEdesign))
cv(svymean(~HSWHOX == 2, HOMEdesign))
cv(svymean(~HSWHOX == 5, HOMEdesign))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, HOMEdesign))

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES != 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES != 3)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSWHOX == 1) ~ low_high, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((HSWHOX == 2) ~ low_high, # not significant
         HOMEdesign,
         na.rm=TRUE)
svyttest((HSWHOX == 5) ~ low_high, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((other_family == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)


cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 1)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 2)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSWHOX == 1) ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSWHOX == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSWHOX == 5) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of hsing
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSWHOX == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSWHOX == 5) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, elementary_secondary != 1)))

svyttest((HSWHOX == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSWHOX == 5) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

# ---

# NEXT QUESTION: Is the child enrolled in online classes? HSINTNET

# What percent of those who selected "full-time virtual school"
# at the start of the survey were ultimately coded as homeschooled?
svymean(~SCHTYPE==3, subset(PFIdesign, EDCINTK12 == 1))

# what percent of all homeschoolers take online classes?
round(wpct(HOME$HSINTNET, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, HOMEdesign))
cv(svymean(~HSINTNET == 2, HOMEdesign))
cv(svymean(~HSINTNET == 3, HOMEdesign))
cv(svymean(~HSINTNET == 4, HOMEdesign))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, HOMEdesign))

# what percent of hsers are enrolled full-time in a virtual school?
round(wpct(HOME$EDCINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# what were these full-time virtual schoolers like?
part <- subset(HOME, EDCINTK12 == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$ALWAYS, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$disability, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$elementary_secondary, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, EDCINTK12 != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$ALWAYS, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$disability, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$elementary_secondary, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 1) ~ (EDCINTK12==1), 
         HOMEdesign,
         na.rm=TRUE)
svyttest((ALWAYS == 1) ~ (EDCINTK12==1), 
         HOMEdesign,
         na.rm=TRUE)
svyttest((disability == 1) ~ (EDCINTK12==1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, EDCINTK12 == 1))
svymean(~TOTAL, subset(HOMEdesign, EDCINTK12 != 1))

# what percent of full-time virtual schoolers take online classes?
part <- subset(HOME, EDCINTK12 == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

# what percent of all online courses, were full-time virtual students?
part <- subset(HOME, HSINTNET == 1)
round(wpct(part$EDCINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)

# what percent of those NOT full-time virtual schoolers take online classes?
part <- subset(HOME, EDCINTK12 != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES < 3)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET == 1) ~ lm_high, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSINTNET == 4) ~ lm_high, 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, SES == 1)))

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, SES == 2)))

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET < 4) ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET == 4) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET == 4) ~ (ba_no_ba == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSINTNET == 4) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSINTNET == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSINTNET < 4) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)
 
cv(svymean(~HSINTNET == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 2 | HSINTNET == 3, subset(HOMEdesign, elementary_secondary != 1)))


# ---

# NEXT QUESTION: What is their teaching style? HSSTYL

round(wpct(HOME$HSSTYL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, HOMEdesign))
cv(svymean(~HSSTYL == 2, HOMEdesign))
cv(svymean(~HSSTYL == 3, HOMEdesign))
cv(svymean(~HSSTYL == 4, HOMEdesign))

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSTYL == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 1)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 2)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of homeschooling
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSTYL == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSSTYL == 2) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, HSINTNET == 1) # testing something
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET != 1) # testing something
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSTYL == 1) ~ (HSINTNET == 1), 
         HOMEdesign,
         na.rm=TRUE)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, elementary_secondary != 1)))

# ---

# NEXT QUESTION: Co-ops and homeschool groups?

round(wpct(HOME$HSCOOP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSKACTIV, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSASSNX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSNATL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, HOMEdesign))
cv(svymean(~HSKACTIV == 1, HOMEdesign))
cv(svymean(~HSASSNX == 1, HOMEdesign))
cv(svymean(~HSNATL == 1, HOMEdesign))

# BY INTERNET CLASSES
part <- subset(HOME, HSINTNET == 1) # testing something
round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET == 1) # testing something
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET == 1) # testing something
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET == 1) # testing something
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, HSINTNET != 1) # testing something
round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET != 1) # testing something
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET != 1) # testing something
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSINTNET != 1) # testing something
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSTYL == 1) ~ (HSINTNET == 1), 
         HOMEdesign,
         na.rm=TRUE)

# by SES level
part <- subset(HOME, SES == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 1)))

part <- subset(HOME, SES == 2)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 2)))

part <- subset(HOME, SES == 3)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 3)))

part <- subset(HOME, SES == 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES != 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)
svyttest((HSNATL == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, SES == 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES != 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
svyttest((HSASSNX == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, SES == 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES != 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
svyttest((HSKACTIV == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, disability == 1)))

part <- subset(HOME, disability == 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSASSNX == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, disability == 1 & ALWAYS == 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1 & ALWAYS == 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSASSNX == 1) ~ (disability == 1), 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)

part <- subset(HOME, disability == 1 & SES == 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1 & SES == 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSASSNX == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 3),
         na.rm=TRUE)

part <- subset(HOME, disability == 1 & elementary_secondary != 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1 & elementary_secondary != 1)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
svyttest((HSASSNX == 1) ~ (disability == 1), 
         subset(HOMEdesign, elementary_secondary != 1),
         na.rm=TRUE)


# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, ALWAYS == 1) # always homeschooled

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ALWAYS == 1)))

part <- subset(HOME, ALWAYS != 1) # all homeschool transfers

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ALWAYS != 1)))

svyttest((HSNATL == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSASSNX == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSKACTIV == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSCOOP == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# elementary v. secondary students
part <- subset(HOME, elementary_secondary == 1) # grades K-6

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, elementary_secondary != 1)))

svyttest((HSASSNX == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, elementary_secondary == 1 & ALWAYS == 1 & SES == 3) 
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, elementary_secondary != 1 & ALWAYS != 1 & SES != 3) 
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

HOMEdesign <- update(HOMEdesign,  all_none = ifelse(elementary_secondary == 1 & ALWAYS == 1 & SES == 3, "all", 
                                             ifelse(elementary_secondary != 1 & ALWAYS != 1 & SES != 3, "none", NA)))
svyttest((HSASSNX == 1) ~ all_none, 
         HOMEdesign,
         na.rm=TRUE)

part <- subset(HOME, ALWAYS == 1 & SES == 3) 
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS != 1 & SES != 3) 
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)

HOMEdesign <- update(HOMEdesign,  all_noneNG = ifelse(ALWAYS == 1 & SES == 3, "all", 
                                                    ifelse(ALWAYS != 1 & SES != 3, "none", NA)))
svyttest((HSASSNX == 1) ~ all_noneNG, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary == 1 & ALWAYS == 1 & SES == 3), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary != 1 & ALWAYS != 1 & SES != 3), na.rm=TRUE)

svymean(~HSFREQX, subset(HOMEdesign, SES == 3), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, SES != 3), na.rm=TRUE)

svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~HSFREQX, subset(HOMEdesign, elementary_secondary != 1), na.rm=TRUE)

part <- subset(HOME, elementary_secondary == 1 & ALWAYS == 1 & SES == 3) 
table(part$HSFREQX)

part <- subset(HOME, elementary_secondary != 1 & ALWAYS != 1 & SES != 3) 
table(part$HSFREQX)



# ---

# NEXT QUESTION: 

#START with where they get curriculum

# public library
round(wpct(HOME$HSCLIBRX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# homeschool catalogue or publisher
round(wpct(HOME$HSCHSPUBX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# public school
round(wpct(HOME$HSCPUBLX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# homeschool convention or other event for homeschooling families
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, HOMEdesign)

round(wpct(HOME$HSCCNVX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSCEVTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSCFMLY, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCLIBRX == 1, HOMEdesign))
cv(svymean(~HSCHSPUBX == 1, HOMEdesign))
cv(svymean(~HSCPUBLX == 1, HOMEdesign))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, HOMEdesign))

# by SES level

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, SES == 2))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 2)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, SES == 3))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 3)))

svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES != 1))
svyttest((HSCPUBLX == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)

# HIGH SES v. OTHER, EACH CATEGORY

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES != 3))
svyttest((HSCLIBRX == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES != 3))
svyttest((HSCHSPUBX == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES != 3))
svyttest((HSCPUBLX == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCCNVX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCCNVX == 1, subset(HOMEdesign, SES != 3))
svyttest((HSCCNVX == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

# by reasons for homeschooling: religion
svymean(~HSCLIBRX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, HSRELGON == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, HSRELGON == 1)))

svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, HSRELGON != 1))
svyttest((HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1) ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON != 1))
svyttest((HSCHSPUBX == 1) ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON != 1))
svyttest((HSCPUBLX == 1) ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

# by reasons for homeschooling: disability
svymean(~HSCLIBRX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, disability == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, disability == 1)))

svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability != 1))

svyttest((HSCPUBLX == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCCNVX == 1, subset(HOMEdesign, disability == 1 & SES == 3))
svymean(~HSCCNVX == 1, subset(HOMEdesign, disability != 1 & SES == 3))

# FIGURE THIS OUT
svyttest((HSCCNVX == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 3),
         na.rm=TRUE)

# by bachelor's degree or no
svymean(~HSCLIBRX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ba_no_ba == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
# by bachelor's degree or no
svymean(~HSCLIBRX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, PARGRADEX == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ALWAYS == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ALWAYS == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ALWAYS != 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ALWAYS != 1)))

svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS != 1))
svyttest((HSCPUBLX == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, ALWAYS != 1))
svyttest((HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# elementary v. secondary students
svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, elementary_secondary == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, elementary_secondary == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, elementary_secondary == 2))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, elementary_secondary == 2)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary != 1))
svyttest((HSCLIBRX == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, subset(HOMEdesign, elementary_secondary != 1))
svyttest((HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)


# NUMBER OF OPTIONS THEY CHOSE (of 4)

svymean(~total_curr, HOMEdesign)

svymean(~total_curr, subset(HOMEdesign, SES == 3))
svymean(~total_curr, subset(HOMEdesign, SES != 3))
svyttest((total_curr) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_curr, subset(HOMEdesign, ALWAYS == 1))
svymean(~total_curr, subset(HOMEdesign, ALWAYS != 1))
svyttest((total_curr) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_curr, subset(HOMEdesign, disability == 1))
svymean(~total_curr, subset(HOMEdesign, disability != 1))
svyttest((total_curr) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_curr, subset(HOMEdesign, elementary_secondary == 1))
svymean(~total_curr, subset(HOMEdesign, elementary_secondary != 1))
svyttest((total_curr) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCOTH == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCOTH == 1, subset(HOMEdesign, elementary_secondary != 1))
svyttest((HSCOTH == 1) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSCHSRELX==1, HOMEdesign, na.rm=TRUE)
svymean(~HSCHSRELX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSCHSRELX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

# NUMBER OF OPTIONS THEY CHOSE -- ONLINE CURRICULUM (of 5)

svymean(~total_onl, HOMEdesign)

svymean(~total_onl, subset(HOMEdesign, SES == 3))
svymean(~total_onl, subset(HOMEdesign, SES != 3))
svyttest((total_onl) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_onl, subset(HOMEdesign, ALWAYS == 1))
svymean(~total_onl, subset(HOMEdesign, ALWAYS != 1))
svyttest((total_onl) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_onl, subset(HOMEdesign, disability == 1))
svymean(~total_onl, subset(HOMEdesign, disability != 1))
svyttest((total_onl) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~total_onl, subset(HOMEdesign, elementary_secondary == 1))
svymean(~total_onl, subset(HOMEdesign, elementary_secondary != 1))
svyttest((total_onl) ~ (elementary_secondary == 1), 
         HOMEdesign,
         na.rm=TRUE)



# --------------------------------


# NEXT QUESTION: What types of schools or teachers provide the child's
# online courses? No need for chart, describe only.

# create a design object so I can test the coefficient of variance
# note that these respondents are ONLY the ones providing online courses
ONLINE <- subset(HOME, HSINTNET != 4)
ONLINEdesign <- svrepdesign(
  data = ONLINE, 
  repweights = subset(ONLINE, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(ONLINEdesign)
# end creation of survey object

# full list of options for this question
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCOL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTIND, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, ONLINEdesign), na.rm=TRUE)

# PLAYGROUND full list of options
part <- subset(HOME, HSINTNET == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)

# PLAYGROUND testing coefficient of variation
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)

# SPECIFIC STATISTICAL SIGNIFICANCE CACULATIONS

# through public school, low SES v other
svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES != 1))
svyttest((HSINTPUB == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)
# through a virtual school, low SES v other
svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES != 1))
svyttest((HSINTVRT == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)
# through a company for purchase, high SES v other
svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 3))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES != 3))
svyttest((HSINTCMP == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

# through public school, all online courses v. some online courses
svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTPUB == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)
# through virtual academy, all online courses v. some online courses
svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTVRT == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)
# through company, all online courses v. some online courses
svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTCMP == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)

# parents with a BA v. those without, online courses through a public school
svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX < 4))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX > 3))
svyttest((HSINTPUB == 1) ~ (PARGRADEX < 4), 
         HOMEdesign,
         na.rm=TRUE)
# parents with a BA v. those without, online courses through a private company
svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX < 4))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX > 3))
svyttest((HSINTCMP == 1) ~ (PARGRADEX < 4), 
         HOMEdesign,
         na.rm=TRUE)

# virtual school, disability v. no
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)
# online public school, disability v. no
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1))
svyttest((HSINTPUB == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

# disabilities, online courses v. all other options -- THIS IS THE MOTHERLOAD
# note: asking "what percent of students in X online option have disabilities"
svymean(~disability == 1, subset(ONLINEdesign, HSINTPUB == 1))
svymean(~disability == 1, subset(ONLINEdesign, HSINTVRT == 1))
HOMEdesign <- update(HOMEdesign,  pub_vir = ifelse(HSINTPUB == 1, "pub", ifelse((HSINTVRT == 1), "vir", NA)))
svyttest((disability == 1) ~ pub_vir, 
         HOMEdesign,
         na.rm=TRUE)

# ALL PARENTS WITH ONLINE COURSES
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCOL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTIND, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTOH, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, ONLINEdesign), na.rm=TRUE)
# Respondents whose kids are in online courses for ALL classes
part <- subset(HOME, HSINTNET == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTOH, weight=part$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
# Respondents whose kids are in online courses for SOME classes
part <- subset(HOME, HSINTNET == 2 | HSINTNET == 3)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTOH, weight=part$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)

# SOURCES FOR ONLINE COURSES
# Mundane overall examination, like the others

# potentially am only looking at these:
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)

# BY SES LEVEL
part <- subset(HOME, SES == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)

part <- subset(HOME, SES == 2)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)

part <- subset(HOME, SES == 3)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)

# BY REASONS FOR HOMESCHOOLING
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)

part <- subset(HOME, disability == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)

# BY PARENT EDUCATION LEVEL
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)

part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)

# BY LENGTH OF TIME HOMESCHOOLING
part <- subset(HOME, ALWAYS == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)

part <- subset(HOME, ALWAYS != 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)

# BY CHILD'S GRADE LEVEL
part <- subset(HOME, elementary_secondary == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)

part <- subset(HOME, elementary_secondary != 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)


# ONE LAST COMPARISON

part <- subset(HOME, SES == 3 & ALWAYS == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES == 1 & FIRST == 1 & disability != 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)


# What role does religion play, versus SES and always?

svymean(~HSWHOX==1, subset(HOMEdesign, HSRELGON==1 & SES == 3 & ALWAYS == 1))
svymean(~HSWHOX==1, subset(HOMEdesign, HSRELGON!=1 & SES == 3 & ALWAYS == 1))

svymean(~HSINTNET==1, subset(HOMEdesign, HSRELGON==1 & SES == 3 & ALWAYS == 1))
svymean(~HSINTNET==1, subset(HOMEdesign, HSRELGON!=1 & SES == 3 & ALWAYS == 1))

part <- subset(HOME, HSRELGON==1 & SES == 3 & ALWAYS == 1)
table(part$HSSTYL)
part <- subset(HOME, HSRELGON!=1 & SES == 3 & ALWAYS == 1)
table(part$HSSTYL)


# END section on homeschooling methods