# Clean data, create data sets for project

# PFI data: Replace all -1s with NAs, change grade level numbers
# pfi_pu_pert[pfi_pu_pert == -1] <- NA
# pfi_pu_pert$ALLGRADEX <- c(pfi_pu_pert$ALLGRADEX -3)
# pfi_pu_pert$ALLGRADEX[pfi_pu_pert$ALLGRADEX == -1] <- 0


# Create homeschool data subset
HSData <- subset(pfi_pu_pert, HOMESCHLX == 1)
# Remove students who "attend" school 25 hours or more
HSData <- subset(HSData, HSData$SCHLHRSWK != 4 | is.na(HSData$SCHLHRSWK))

# Create public school data subset
PublicSchool <- subset(pfi_pu_pert, EDCPUB == 1)

# Create virtual school data subset
VirtualSchool <- subset(pfi_pu_pert, EDCINTK12 == 1)

# Create private school data subset
PrivateSchool <- subset(pfi_pu_pert, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1)