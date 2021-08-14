# Clean data, create data sets, load libraries

# NOTE: need to check which of these packages I actually need.

# install.packages("tables")
# install.packages("haven")
# install.packages("survey")
# install.packages("jtools")
# install.packages("remotes")
# remotes::install_github("carlganz/svrepmisc")

library("tables")
library("haven")
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")

load(file = "pfi_pu_pert.RData")

# PFI data: Replace all -1s with NAs, change grade level numbers
pfi_pu_pert[pfi_pu_pert == -1] <- NA
pfi_pu_pert$ALLGRADEX <- c(pfi_pu_pert$ALLGRADEX -3)
pfi_pu_pert$ALLGRADEX[pfi_pu_pert$ALLGRADEX == -1] <- 0

PFI <- pfi_pu_pert

# Create a column with: 1 = Elementary; 2 = Middle; 3 = High School
PFI$SCHLEVEL <- ifelse(PFI$ALLGRADEX == 0 | 
                         PFI$ALLGRADEX == 1 | 
                         PFI$ALLGRADEX == 2 | 
                         PFI$ALLGRADEX == 3 | 
                         PFI$ALLGRADEX == 4 | 
                         PFI$ALLGRADEX == 5, 1, 
                       ifelse(PFI$ALLGRADEX == 6 | 
                                PFI$ALLGRADEX == 7 | 
                                PFI$ALLGRADEX == 8, 2, 
                              ifelse(PFI$ALLGRADEX == 9 | 
                                       PFI$ALLGRADEX == 10 | 
                                       PFI$ALLGRADEX == 11 | 
                                       PFI$ALLGRADEX == 12, 3, NA)))
# check work using table (should be: 6230, 3812, 6404)
table(PFI$SCHLEVEL)
# note that this column (SCHLEVEL) does not include any NAs

# Create a column with: 1 = Public; 2 = Home; 3 = Private; 4 = Virtual
PFI$SCHTYPE <- ifelse(PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (
                          PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK)), 3, 
                      ifelse(PFI$EDCINTK12 == 1, 4, 
                             ifelse((PFI$EDCCAT == 1 | 
                                       PFI$EDCREL == 1 | 
                                       PFI$EDCPRI == 1), 2, 
                                    ifelse(PFI$EDCPUB == 1, 1, 5))))
# Note: SCHTYPE == 5 needs to be eliminated. 5 is all "yes" responses to 
    # "EDCINTCOL" or "EDCCOL" and that did not also answer "yes" to other 
    # school types. (EDCINTCOL and EDCCOL are online or in-person college.)
# Remove all rows where SCHTYPE == 5.
PFI <- PFI[ which(PFI$SCHTYPE < 5), ]
# check work using table(should be: 13882, 1736, 496, 250)
table(PFI$SCHTYPE)

# ON DETERMINING which students are homeschooled (lines 48 to 54):
# PFI$EDCHSFL == 1: 
#      1 = 13874, 2 = 1732, 3 = 532, 4 = 232
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1: 
#      1 = 13874, 2 = 1734, 3 = 519, 4 = 240
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK):
#      1 = 13882, 2 = 1736, 3 = 496, 4 = 250

# Create column with: homeschool (1) or public school (2)
PFI$home_public <- ifelse(PFI$SCHTYPE == 3, 1, 
                          ifelse(PFI$SCHTYPE == 1, 2, NA))

# Create column with: white (1) or non-white (2)
PFI$white_nonwhite <- ifelse(PFI$RACEETH == 1, 1, 2)

# Create column with: BA (1) or no BA (2)
PFI$ba_no_ba <- ifelse(PFI$PARGRADEX > 3, 1, 2)

# Create column with: grades 1-6 (1) or grades 7-12 (2)
PFI$elementary_secondary <- ifelse(PFI$ALLGRADEX > 6, 2, 
                                   ifelse(PFI$ALLGRADEX == 0, NA, 1))

# Create column with: two-parent household (1) or single parent (2)
PFI$two_parent_or_single <- ifelse(PFI$HHPARN19_BRD = 1, 1, 2)

# Create column with: English spoken at home (1) or no (2)
PFI$english_or_no <- ifelse(PFI$HHENGLISH == 1, 1, 2)

# May want to add a column comparing virtual v. non-virtual

# For general PFI data file, input details into survey package
PFIdesign <- svrepdesign(
  data = PFI, 
  repweights = subset(PFI, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(PFIdesign)

# END creation of PFI data set and PFIdesign object


# DELETE BELOW

# Testing what repweights do by changing them
PFIdesign <- svrepdesign(data = PFI, repweights = subset(PFI, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
TESTdesign <- svrepdesign(data = PFI, repweights = subset(PFI, select = FPWT1:FPWT10), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=9/10)

G112design <- subset(PFIdesign, ALLGRADEX != 0)
summary(G112design)

TESTrep <- subset(PFIdesign, FPWT < 10000)
summary(TESTrep)

svytable(~SCHTYPE, PFIdesign)
svytable(~SCHTYPE, TESTdesign)
svytable(~ALLGRADEX, G112design)

svysd(~SCHTYPE, PFIdesign)
svysd(~SCHTYPE, TESTdesign)

svymean(~PARGRADEX > 3, subset(PFIdesign, SCHTYPE == 3))
svymean(~PARGRADEX > 3, subset(TESTdesign, SCHTYPE == 3))

svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, PFIdesign)
svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, TESTdesign)
svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, G112design)
svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, TESTrep)

svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, subset(PFIdesign, ALLGRADEX == 6 | ALLGRADEX == 7 | ALLGRADEX == 8))
svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, subset(G112design, ALLGRADEX == 6 | ALLGRADEX == 7 | ALLGRADEX == 8))

# Looking at large weighted items

a <- (PFI$FPWT > 10000)
print(a)

ifelse(PFI$FPWT > 10000, "view", NA)
apply(PFI, 2, function(x) which(x > 10000))
apply(PFI, 2, function(x) PFI$FPWT > 10000)

over10K <- subset(PFI, FPWT > 10000)
over10K$FPWT

over20K <- subset(PFI, FPWT > 20000)
over20K$FPWT

over50K <- subset(PFI, FPWT > 50000)
over50K$FPWT


# Testing replicate weights

# Create a subset of just homeschooled students
HOME <- subset(PFI, SCHTYPE == 3)
# Create an array of 1 or 0 for parents having a BA
BA <- ifelse(HOME$PARGRADEX > 3, 1, 0)
as.array(BA)
# Create a data frame of replicate weights
RW <- HOME[c(409:488)]
# Multiply replicate weights by the 1 or 0
RW.BA <- RW*BA
# Sum each column (each replicate weight)
RW.BA.sum <- colSums(RW.BA)

# Turn replicate weight sums into percents 
    # by dividing by the sum of the base weights 
RW.BA.per <- RW.BA.sum / sum(HOME$FPWT)

# Create a base weight percent as a comparison
BW <- ((sum((HOME[c(408)]) * BA) / sum(HOME$FPWT)))

# COMPARE CALCULATIONS:

# The jackknife variance SE estimator, from the NHES User Manual, p. 157:
sqrt(
  (79/80) * sum(
    (RW.BA.per - BW)^2)
)

# Compare to Survey package calculation of standard error:
svymean(~PARGRADEX > 3, subset(PFIdesign, SCHTYPE == 3))

# The manual calculation of SE   = 0.0352
# The Survey package calculation = 0.0327

# BLOODY HELL these should be the same!!








# OLD, DELETE
HOME %>%
  group_by(PARGRADEX > 3)%>%
  summarise_all(weighted.mean, w = HOME$FPWT)


svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, PFIdesign)
svymean(~PARGRADEX > 3, subset(PFIdesign, SCHTYPE == 3))

HOME <- subset(PFI, SCHTYPE == 3)

weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT)

(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT1) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT2) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT3) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT4) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT5) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT6) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT7) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT8) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT9) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT10) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT11) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT12) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT13) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT14) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT15) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT16) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT17) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT18) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT19) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT20) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT21) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT22) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT23) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT24) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT25) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT26) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT27) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT28) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT29) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT30) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT31) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT32) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT33) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT34) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT35) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT36) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT37) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT38) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT39) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT40) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT41) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT42) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT43) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT44) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT45) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT46) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT47) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT48) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT49) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT50) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT51) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT52) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT53) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT54) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT55) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT56) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT57) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT58) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT59) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT60) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT61) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT62) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT63) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT64) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT65) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT66) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT67) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT68) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT69) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT70) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT71) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT72) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT73) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT74) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT75) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT76) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT77) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT78) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT79) - 0.4852701)^2 +
(weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT80) - 0.4852701)^2 

sqrt(0.001081879*(4/18))


apply(HOME, 1, function(x) weighted.mean(HOME$PARGRADEX > 3, HOME$FPWT1:HOME$FPWT80))

for(x in PFI){weighted.mean(HOME$PARGRADEX > 3, HOME$FPWTx)}

apply(HOME, 1, function(x) weighted.mean(HOME$PARGRADEX > 3, HOME$FPWTx))




library(dplyr)
PFI %>%
  group_by(SCHTYPE) %>% 
  mutate(weighted_income = weighted.mean(income, weight))

# END DELETE


# Note: I am not currently using the below data sets or designs.
# I am able to do everything I need by making subsets of PFIdesign.

# Create homeschool data subset, input details into survey package
HSData <- subset(PFI, HOMESCHLX == 1)
HSData <- subset(HSData, HSData$SCHLHRSWK != 4 | is.na(HSData$SCHLHRSWK))
HSdesign <- svrepdesign(data = HSData, repweights = subset(HSData, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(HSdesign)
# Note: 2nd line above removed students who "attend" school 25 hours or more

# Create public school data subset, input details into survey package
PublicSchool <- subset(PFI, EDCPUB == 1)
PuSdesign <- svrepdesign(data = PublicSchool, repweights = subset(PublicSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PuSdesign)

# Create virtual school data subset, input details into survey package
VirtualSchool <- subset(PFI, EDCINTK12 == 1)
VSdesign <- svrepdesign(data = VirtualSchool, repweights = subset(VirtualSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(VSdesign)

# Create private school data subset, input details into survey package
PrivateSchool <- subset(PFI, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1)
PrSdesign <- svrepdesign(data = PrivateSchool, repweights = subset(PrivateSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PrSdesign)
