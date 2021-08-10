# Clean data, create data sets, load libraries

# NOTE: need to check which of these packages I actually need.

install.packages("tables")
library("tables")

# install.packages("haven")
# install.packages("survey")
# install.packages("jtools")
# install.packages("remotes")
# remotes::install_github("carlganz/svrepmisc")

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
PFI$SCHLEVEL <- ifelse(PFI$ALLGRADEX == 0 | PFI$ALLGRADEX == 1 | PFI$ALLGRADEX == 2 | PFI$ALLGRADEX == 3 | PFI$ALLGRADEX == 4 | PFI$ALLGRADEX == 5, 1, ifelse(PFI$ALLGRADEX == 6 | PFI$ALLGRADEX == 7 | PFI$ALLGRADEX == 8, 2, ifelse(PFI$ALLGRADEX == 9 | PFI$ALLGRADEX == 10 | PFI$ALLGRADEX == 11 | PFI$ALLGRADEX == 12, 3, NA)))
table(PFI$SCHLEVEL)

# Create a column with: 1 = Public; 2 = Home; 3 = Private; 4 = Virtual
PFI$SCHTYPE <- ifelse(PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK)), 3, ifelse(PFI$EDCINTK12 == 1, 4, ifelse((PFI$EDCCAT == 1 | PFI$EDCREL == 1 | PFI$EDCPRI == 1), 2, ifelse(PFI$EDCPUB == 1, 1,  NA))))

# ON DETERMINING which students are homeschooled (in script line above):
# PFI$EDCHSFL == 1: 
#      1 = 13874, 2 = 1732, 3 = 532, 4 = 232
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1: 
#      1 = 13874, 2 = 1734, 3 = 519, 4 = 240
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK):
#      1 = 13882, 2 = 1736, 3 = 496, 4 = 250

# Evaluating script above:
table(PFI$SCHTYPE)

# For general PFI data file, input details into survey package
PFIdesign <- svrepdesign(data = PFI, repweights = subset(PFI, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PFIdesign)

TESTdesign <- svydesign(data=PFI,id=~PPSU,strata=~PSTRATUM,weights=~FPWT,nest=TRUE)
summary(TESTdesign)

svytable(~SCHTYPE, PFIdesign)
svytable(~SCHTYPE, TESTdesign)

svymean(~PARGRADEX > 3, subset(PFIdesign, SCHTYPE == 3))
svymean(~PARGRADEX > 3, subset(TESTdesign, SCHTYPE == 3))

svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, PFIdesign)
svyttest(PARGRADEX > 3 ~ SCHTYPE == 3, TESTdesign)


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
