# Clean data, create data sets, load libraries

# install.packages("haven")
# install.packages("survey")
# install.packages("jtools")
# install.packages("remotes")
# remotes::install_github("carlganz/svrepmisc")

# library("haven")
# library("survey")
# library("jtools")
# library("remotes")
# library("svrepmisc")

# PFI data: Replace all -1s with NAs, change grade level numbers
# pfi_pu_pert[pfi_pu_pert == -1] <- NA
# pfi_pu_pert$ALLGRADEX <- c(pfi_pu_pert$ALLGRADEX -3)
# pfi_pu_pert$ALLGRADEX[pfi_pu_pert$ALLGRADEX == -1] <- 0

# For general PFI data file, input details into survey package
PFIdesign <- svrepdesign(data = pfi_pu_pert, repweights = subset(pfi_pu_pert, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PFIdesign)

# Create homeschool data subset, input details into survey package
HSData <- subset(pfi_pu_pert, HOMESCHLX == 1)
HSData <- subset(HSData, HSData$SCHLHRSWK != 4 | is.na(HSData$SCHLHRSWK))
HSdesign <- svrepdesign(data = HSData, repweights = subset(HSData, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(HSdesign)
# Note: 2nd line above removed students who "attend" school 25 hours or more

# Create public school data subset, input details into survey package
PublicSchool <- subset(pfi_pu_pert, EDCPUB == 1)
PuSdesign <- svrepdesign(data = PublicSchool, repweights = subset(PublicSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PuSdesign)

# Create virtual school data subset, input details into survey package
VirtualSchool <- subset(pfi_pu_pert, EDCINTK12 == 1)
VSdesign <- svrepdesign(data = VirtualSchool, repweights = subset(VirtualSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(VSdesign)

# Create private school data subset, input details into survey package
PrivateSchool <- subset(pfi_pu_pert, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1)
PrSdesign <- svrepdesign(data = PrivateSchool, repweights = subset(PrivateSchool, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
summary(PrSdesign)
