# Parent education, by school type
# This script is dependent on the "Data Subsets" script

# Run parental education levels, for each educational method

# PFI overall, by parent education level grouping
svymean(~PARGRADEX == 1, PFIdesign, na.rm = TRUE)
svymean(~PARGRADEX == 2, PFIdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, PFIdesign, na.rm = TRUE)
svymean(~PARGRADEX == 4, PFIdesign, na.rm = TRUE)
svymean(~PARGRADEX == 5, PFIdesign, na.rm = TRUE)

# Homeschool overall, by parent education level grouping
svymean(~PARGRADEX == 1, HSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 2, HSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, HSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 4, HSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 5, HSdesign, na.rm = TRUE)

# Public school overall, by parent education level grouping
svymean(~PARGRADEX == 1, PuSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 2, PuSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, PuSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 4, PuSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 5, PuSdesign, na.rm = TRUE)

# Private school overall, by parent education level grouping
svymean(~PARGRADEX == 1, PrSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 2, PrSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, PrSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 4, PrSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 5, PrSdesign, na.rm = TRUE)

# Virtual school overall, by parent education level grouping
svymean(~PARGRADEX == 1, VSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 2, VSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, VSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 4, VSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 5, VSdesign, na.rm = TRUE)


# Testing looking at different education levels

# Create education level data subset, input details into survey package

# Elementary school
homeES <- subset(HSData, ((ALLGRADEX == 1) | (ALLGRADEX == 2) | (ALLGRADEX == 3) | (ALLGRADEX == 4) | (ALLGRADEX == 5)))
homeESdesign <- svrepdesign(data = homeES, repweights = subset(homeES, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
pubES <- subset(pfi_pu_pert, ((ALLGRADEX == 1) | (ALLGRADEX == 2) | (ALLGRADEX == 3) | (ALLGRADEX == 4) | (ALLGRADEX == 5)))
pubESdesign <- svrepdesign(data = pubES, repweights = subset(pubES, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
priES <- subset(PrivateSchool, ((ALLGRADEX == 1) | (ALLGRADEX == 2) | (ALLGRADEX == 3) | (ALLGRADEX == 4) | (ALLGRADEX == 5)))
priESdesign <- svrepdesign(data = priES, repweights = subset(priES, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)

# Middle school
homeMS <- subset(HSData, ((ALLGRADEX == 6) | (ALLGRADEX == 7) | (ALLGRADEX == 8)))
homeMSdesign <- svrepdesign(data = homeMS, repweights = subset(homeMS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
pubMS <- subset(pfi_pu_pert, ((ALLGRADEX == 6) | (ALLGRADEX == 7) | (ALLGRADEX == 8)))
pubMSdesign <- svrepdesign(data = pubMS, repweights = subset(pubMS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
priMS <- subset(PrivateSchool, ((ALLGRADEX == 6) | (ALLGRADEX == 7) | (ALLGRADEX == 8)))
priMSdesign <- svrepdesign(data = priMS, repweights = subset(priMS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)

# High school
  homeHS <- subset(HSData, ALLGRADEX > 8)
homeHSdesign <- svrepdesign(data = homeHS, repweights = subset(homeHS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
  pubHS <- subset(pfi_pu_pert, ALLGRADEX > 8)
pubHSdesign <- svrepdesign(data = pubHS, repweights = subset(pubHS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)
  priHS <- subset(PrivateSchool, ALLGRADEX > 8)
priHSdesign <- svrepdesign(data = priHS, repweights = subset(priHS, select = FPWT1:FPWT80), weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, scale=79/80)


# Bachelor's degree, compared: 
svymean(~PARGRADEX > 3, homeESdesign, na.rm = TRUE)
svymean(~PARGRADEX > 3, pubESdesign, na.rm = TRUE)

svymean(~PARGRADEX > 3, homeMSdesign, na.rm = TRUE)
svymean(~PARGRADEX > 3, pubMSdesign, na.rm = TRUE)

svymean(~PARGRADEX > 3, homeHSdesign, na.rm = TRUE)
svymean(~PARGRADEX > 3, pubHSdesign, na.rm = TRUE)

# Vocational or some college, compared: 
svymean(~PARGRADEX == 3, homeESdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, pubESdesign, na.rm = TRUE)

svymean(~PARGRADEX == 3, homeMSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, pubMSdesign, na.rm = TRUE)

svymean(~PARGRADEX == 3, homeHSdesign, na.rm = TRUE)
svymean(~PARGRADEX == 3, pubHSdesign, na.rm = TRUE)

# High school diploma or less, compared:
svymean(~PARGRADEX < 3, homeESdesign, na.rm = TRUE)
svymean(~PARGRADEX < 3, pubESdesign, na.rm = TRUE)

svymean(~PARGRADEX < 3, homeMSdesign, na.rm = TRUE)
svymean(~PARGRADEX < 3, pubMSdesign, na.rm = TRUE)

svymean(~PARGRADEX < 3, homeHSdesign, na.rm = TRUE)
svymean(~PARGRADEX < 3, pubHSdesign, na.rm = TRUE)





