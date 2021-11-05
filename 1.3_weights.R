# ANALYZING THE WEIGHTS

# note: This script is designed to run after 0_data_subsets script.


HOME <- subset(PFI, SCHTYPE == 3)

mean(HOME$FPWT)
mean(HOME$FPWT)
max(HOME$FPWT)

mean(PFI$FPWT)
mean(PFI$FPWT)
max(PFI$FPWT)


# This section checks what the weights look like for each grade
# It creates a table with information about weights by grade

WEIGHTS <- c(1:5)
WEIGHTS <- as.data.frame(WEIGHTS)

WEIGHTS$GK <- c(sum(HOME$countn[HOME$ALLGRADEX == 0]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 0])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 0])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 0])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 0])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 0])*100)))
WEIGHTS$G1 <- c(sum(HOME$countn[HOME$ALLGRADEX == 1]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 1])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 1])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 1])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 1])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 1])*100)))
WEIGHTS$G2 <- c(sum(HOME$countn[HOME$ALLGRADEX == 2]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 2])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 2])),
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 2])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 2])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 2])*100)))
WEIGHTS$G3 <- c(sum(HOME$countn[HOME$ALLGRADEX == 3]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 3])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 3])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 3])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 3])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 3])*100)))
WEIGHTS$G4 <- c(sum(HOME$countn[HOME$ALLGRADEX == 4]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 4])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 4])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 4])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 4])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 4])*100)))
WEIGHTS$G5 <- c(sum(HOME$countn[HOME$ALLGRADEX == 5]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 5])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 5])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 5])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 5])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 5])*100)))
WEIGHTS$G6 <- c(sum(HOME$countn[HOME$ALLGRADEX == 6]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 6])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 6])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 6])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 6])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 6])*100)))
WEIGHTS$G7 <- c(sum(HOME$countn[HOME$ALLGRADEX == 7]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 7])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 7])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 7])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 7])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 7])*100)))
WEIGHTS$G8 <- c(sum(HOME$countn[HOME$ALLGRADEX == 8]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 8])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 8])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 8])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 8])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 8])*100)))
WEIGHTS$G9 <- c(sum(HOME$countn[HOME$ALLGRADEX == 9]), 
                round(sum(HOME$FPWT[HOME$ALLGRADEX == 9])),
                round(max(HOME$FPWT[HOME$ALLGRADEX == 9])), 
                round(mean(HOME$FPWT[HOME$ALLGRADEX == 9])), 
                (round(max(HOME$FPWT[HOME$ALLGRADEX == 9])/
                         sum(HOME$FPWT[HOME$ALLGRADEX == 9])*100)))
WEIGHTS$G10 <- c(sum(HOME$countn[HOME$ALLGRADEX == 10]), 
                 round(sum(HOME$FPWT[HOME$ALLGRADEX == 10])),
                 round(max(HOME$FPWT[HOME$ALLGRADEX == 10])), 
                 round(mean(HOME$FPWT[HOME$ALLGRADEX == 10])), 
                 (round(max(HOME$FPWT[HOME$ALLGRADEX == 10])/
                          sum(HOME$FPWT[HOME$ALLGRADEX == 10])*100)))
WEIGHTS$G11 <- c(sum(HOME$countn[HOME$ALLGRADEX == 11]), 
                 round(sum(HOME$FPWT[HOME$ALLGRADEX == 11])),
                 round(max(HOME$FPWT[HOME$ALLGRADEX == 11])), 
                 round(mean(HOME$FPWT[HOME$ALLGRADEX == 11])), 
                 (round(max(HOME$FPWT[HOME$ALLGRADEX == 11])/
                          sum(HOME$FPWT[HOME$ALLGRADEX == 11])*100)))
WEIGHTS$G12 <- c(sum(HOME$countn[HOME$ALLGRADEX == 12]), 
                 round(sum(HOME$FPWT[HOME$ALLGRADEX == 12])),
                 round(max(HOME$FPWT[HOME$ALLGRADEX == 12])), 
                 round(mean(HOME$FPWT[HOME$ALLGRADEX == 12])), 
                 (round(max(HOME$FPWT[HOME$ALLGRADEX == 12])/
                          sum(HOME$FPWT[HOME$ALLGRADEX == 12])*100)))

WEIGHTS$WEIGHTS <- NULL
WEIGHTS <- t(WEIGHTS)
colnames(WEIGHTS) <- c("Count", "Weighted Total", "Max Weight", "Mean Weight", "Percent")
print(WEIGHTS)

write.csv(WEIGHTS,"/Users/Rachel/R-Projects/NHES-2019/data/WEIGHTS.csv", row.names = TRUE)

# End weight table section
