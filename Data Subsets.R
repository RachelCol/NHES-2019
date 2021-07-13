# START

# Create a homeschool data subset
HSData <- subset(pfi_pu_pert, HOMESCHLX == 1)

# Change grade level numbers
HSData$ALLGRADEX <- c(HSData$ALLGRADEX -3)

# Change remaining -1s to 0s
HSData$ALLGRADEX[HSData$ALLGRADEX == -1] <- 0

# Remove students who "attend" school 25 hours or more
HSData <- subset(HSData, HSData$SCHLHRSWK != 4 | is.na(HSData$SCHLHRSWK))

#END


#START

# Create a public school data subset
PublicSchool <- subset(pfi_pu_pert, EDCPUB == 1)

# Change grade level numbers
PublicSchool$ALLGRADEX <- c(PublicSchool$ALLGRADEX -3)

# Change remaining -1s to 0s
PublicSchool$ALLGRADEX[PublicSchool$ALLGRADEX == -1] <- 0

# END


#START

# Create a virtual school data subset
VirtualSchool <- subset(pfi_pu_pert, EDCINTK12 == 1)

# Change grade level numbers
VirtualSchool$ALLGRADEX <- c(VirtualSchool$ALLGRADEX -3)

# Change remaining -1s to 0s
VirtualSchool$ALLGRADEX[VirtualSchool$ALLGRADEX == -1] <- 0

# END


#START

# Create a private school data subset
PrivateSchool <- subset(pfi_pu_pert, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1)

# Change grade level numbers
PrivateSchool$ALLGRADEX <- c(PrivateSchool$ALLGRADEX -3)

# Change remaining -1s to 0s
PrivateSchool$ALLGRADEX[PrivateSchool$ALLGRADEX == -1] <- 0

# END

