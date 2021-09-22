# Create a profile of an individual student
# Goal here is to explain reasoning behind excluding kindergarten

PFIwKHS <- subset(PFIwK, SCHTYPE == 3)
PFIHS <- subset(PFI, SCHTYPE == 3)

mean(PFIwKHS$FPWT)
mean(PFIwK$FPWT)
max(PFIwK$FPWT)

subset(PFIwK, FPWT > 50,000)

max(PFIwK$FPWT[PFIwKHS$ALLGRADEX == 1])

WEIGHTS <- c(1:4)
WEIGHTS <- as.data.frame(WEIGHTS)

WEIGHTS$GK <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 0]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 0])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 0])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 0])/
                  sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 0])*100)))
WEIGHTS$G1 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 1]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 1])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 1])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 1])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 1])*100)))
WEIGHTS$G2 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 2]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 2])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 2])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 2])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 2])*100)))
WEIGHTS$G3 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 3]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 3])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 3])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 3])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 3])*100)))
WEIGHTS$G4 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 4]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 4])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 4])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 4])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 4])*100)))
WEIGHTS$G5 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 5]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 5])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 5])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 5])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 5])*100)))
WEIGHTS$G6 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 6]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 6])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 6])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 6])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 6])*100)))
WEIGHTS$G7 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 7]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 7])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 7])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 7])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 7])*100)))
WEIGHTS$G8 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 8]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 8])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 8])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 8])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 8])*100)))
WEIGHTS$G9 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 9]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 9])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 9])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 9])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 9])*100)))
WEIGHTS$G10 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 10]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 10])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 10])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 10])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 10])*100)))
WEIGHTS$G11 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 11]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 11])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 11])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 11])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 11])*100)))
WEIGHTS$G12 <- c(sum(PFIwKHS$countn[PFIwKHS$ALLGRADEX == 12]), 
                round(sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 12])),
                round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 12])), 
                (round(max(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 12])/
                         sum(PFIwKHS$FPWT[PFIwKHS$ALLGRADEX == 12])*100)))

WEIGHTS$WEIGHTS <- NULL
WEIGHTS <- t(WEIGHTS)
colnames(WEIGHTS) <- c("Count", "Weighted Total", "Max Weight", "Percent")

print(WEIGHTS)

write.csv(WEIGHTS,"/Users/Rachel/R-Projects/NHES-2019/WEIGHTS.csv", row.names = TRUE)


# Weight Tables: 
# print(AllWeight)
# print(HSWeight)
# print(PublicWeight)
# print(PrivateWeight)
# print(VirtualWeight)



# set which grade to examine
PFIgrade <- subset(PFIwK, ALLGRADEX == 0 & SCHTYPE == 3)

# creates matrix "examine" for the max weighted child in that grade
examine <- PFIgrade[(which.max(PFIgrade$FPWT)), ]

# START HERE
PROFILE <- matrix(1, 2)
PROFILE <- as.data.frame(PROFILE)

# What percent of the grade is the child
PROFILE$PERCT <- c("Percent of grade", (round(100*(max(PFIgrade$FPWT)/sum(PFIgrade$FPWT)), digits = 3)))

# Child's sex
PROFILE$CSEX <- c("Child's sex", (ifelse(examine$CSEX == 1, print("male"), print("female"))))

# Child's race or ethnicity
PROFILE$RACEETH <- c("Child's race/ethnicity", 
                      (ifelse(examine$RACEETH == 1, print("a White"), 
                         ifelse(examine$RACEETH == 2, print("a Black"),
                            ifelse(examine$RACEETH == 3, print("a Hispanic"),
                               ifelse(examine$RACEETH == 4, print("an Asian or Pacific Islander"),
                                  ifelse(examine$RACEETH == 5, print("an other race or mixed race"), 
                                            print("[race not provided]"))))))))

# Child's age
PROFILE$AGE2018 <- c("Age as of Dec. 31, 2018", sum(examine$AGE2018))
PROFILE$BIRTHMONTH <- c("Birth Month", 
                        (ifelse(examine$CDOBMM == 1, "January", 
                         ifelse(examine$CDOBMM == 2, "February", 
                         ifelse(examine$CDOBMM == 3, "March", 
                         ifelse(examine$CDOBMM == 4, "April", 
                         ifelse(examine$CDOBMM == 5, "May", 
                         ifelse(examine$CDOBMM == 6, "June", 
                         ifelse(examine$CDOBMM == 7, "July", 
                         ifelse(examine$CDOBMM == 8, "August", 
                         ifelse(examine$CDOBMM == 9, "September", 
                         ifelse(examine$CDOBMM == 10, "October", 
                         ifelse(examine$CDOBMM == 11, "November", 
                         ifelse(examine$CDOBMM == 12, "December",                                                             
                                   NA))))))))))))))
                  
# Child's grade
PROFILE$ALLGRADEX <- c("child's current grade", sum(examine$ALLGRADEX))

# Facts about the questionnaire 
PROFILE$ENGLSPANX <- c("Questionnaire language", (ifelse(examine$ENGLSPANX == 1, print("in English"), print("in Spanish"))))
PROFILE$MODECOMP <- c("Questionnaire mode", (ifelse(examine$MODECOMP == 1, print("on the Web"), 
                                                ifelse(examine$MODECOMP == 2, print("on paper"), print("by telephone")))))


# Facts about the household
PROFILE$FAMILY19_BRD <- c("Household composition", (ifelse(examine$FAMILY19_BRD == 1, print("two parents and siblings"), 
                                                  ifelse(examine$FAMILY19_BRD == 2, print("two parents and no siblings"),
                                                         ifelse(examine$FAMILY19_BRD == 3, print("one parent and siblings"), 
                                                                ifelse(examine$FAMILY19_BRD == 4, print("one parent and no siblings"), ))))))

PROFILE$HHTOTALXX <- c("Total people in the household", sum(examine$HHTOTALXX))

if(examine$HHMOM == 1) {
PROFILE$HHMOM <- c("Mother in the household?", (ifelse(examine$HHMOM == 1, print("mother, "), print("NA"))))
}else{
  print("")
}

if(examine$HHDAD == 1) {
PROFILE$HHDAD <- c("Father in the household?", (ifelse(examine$HHDAD == 1, print("father, "), print("NA"))))
}else{
  print("")
}

PROFILE$HHSISSX <- c("Sisters in the household", (ifelse(examine$HHSISSX == 1, print("1 sister"), 
                                                         ifelse(examine$HHSISSX == 2, print("2 sisters"), 
                                                                ifelse(examine$HHSISSX == 3, print("3 sisters"), 
                                                                       ifelse(examine$HHSISSX == 4, print("4 sisters"), 
                                                                              ifelse(examine$HHSISSX == 5, print("5 sisters"), 
                                                                                     ifelse(examine$HHSISSX == 6, print("6 sisters"), 
                                                                                            ifelse(examine$HHSISSX == 7, print("7 sisters"), 
                                                         
                                                         print("no sisters"))))))))))

PROFILE$HHBROSX <- c("Brothers in the household", (ifelse(examine$HHBROSX == 1, print("1 brother"), 
                                                          ifelse(examine$HHBROSX == 2, print("2 brothers"), 
                                                                 ifelse(examine$HHBROSX == 3, print("3 brothers"), 
                                                                        ifelse(examine$HHBROSX == 4, print("4 brothers"), 
                                                                               ifelse(examine$HHBROSX == 5, print("5 brothers"), 
                                                                                      ifelse(examine$HHBROSX == 6, print("6 brothers"), 
                                                                                             ifelse(examine$HHBROSX == 7, print("7 brothers"), 
                                                                                                    
                                                                                                    print("no brothers"))))))))))

PROFILE$HHUNDR6X <- c("Children under 6", (ifelse(examine$HHUNDR6X == 0, print("no children under 6"), 
                                                  ifelse(examine$HHUNDR6X == 1, print("1 child under 6"),
                                                         ifelse(examine$HHUNDR6X == 2, print("2 children under 6"),
                                                                ifelse(examine$HHUNDR6X == 3, print("3 children under 6"),
                                                                       ifelse(examine$HHUNDR6X == 4, print("4 children under 6"), NA)))))))

PROFILE$HHUNDR10X <- c("Children under 10", (ifelse(examine$HHUNDR10X == 0, print("no children under 10"), 
                                                  ifelse(examine$HHUNDR10X == 1, print("1 child under 10"),
                                                         ifelse(examine$HHUNDR10X == 2, print("2 children under 10"),
                                                                ifelse(examine$HHUNDR10X == 3, print("3 children under 10"),
                                                                       ifelse(examine$HHUNDR10X == 4, print("4 children under 10"), 
                                                                              ifelse(examine$HHUNDR10X == 5, print("5 children under 10"), NA))))))))

PROFILE$HHUNDR16X <- c("Children under 16", (ifelse(examine$HHUNDR16X == 0, print("no children under 16"), 
                                                    ifelse(examine$HHUNDR16X == 1, print("1 child under 16"),
                                                           ifelse(examine$HHUNDR16X == 2, print("2 children under 16"),
                                                                  ifelse(examine$HHUNDR16X == 3, print("3 children under 16"),
                                                                         ifelse(examine$HHUNDR16X == 4, print("4 children under 16"), 
                                                                                ifelse(examine$HHUNDR16X == 5, print("5 children under 16"), NA))))))))

PROFILE$HHUNDR18X <- c("Children under 18", (ifelse(examine$HHUNDR18X == 0, print("no children under 18"), 
                                                    ifelse(examine$HHUNDR18X == 1, print("1 child under 18"),
                                                           ifelse(examine$HHUNDR18X == 2, print("2 children under 18"),
                                                                  ifelse(examine$HHUNDR18X == 3, print("3 children under 18"),
                                                                         ifelse(examine$HHUNDR18X == 4, print("4 children under 18"), 
                                                                                ifelse(examine$HHUNDR18X == 5, print("5 children under 18"), NA))))))))

if(is.na(examine$CHAGE1) == TRUE) {
PROFILE$NOSIB <- c("no siblings", "The child has no siblings under age 21")
}else{
  print("")
}

# FIRST SIBLING
if(is.na(examine$CHAGE1) == FALSE){
PROFILE$CHAGE1 <- c("1st unsambled sibling's age", (ifelse((!is.na(examine$CHAGE1) == TRUE), 
                                                           paste("The child's siblings are: a", examine$CHAGE1, "year old"), )))
PROFILE$CHSEX1 <- c("1st unsambled sibling's sex", (ifelse(examine$CHSEX1 == 1, print(" boy"), 
                                                           ifelse(examine$CHSEX1 == 2, print(" girl"), ))))
PROFILE$CHENRL1 <- c("1st unsambled sibling's school", 
                     (ifelse(examine$CHENRL1 == 1, print(" who is being homeschooled"), 
                             ifelse(examine$CHENRL1 == 2, print(" who is enrolled in public or private school"),
                                    ifelse(examine$CHENRL1 == 3, print(" who is attending college"), 
                                           ifelse(examine$CHENRL1 == 4, print(" who is not in school"), ))))))
}else{print("")}

# SECOND SIBLING
if(is.na(examine$CHAGE2) == FALSE){
PROFILE$CHAGE2 <- c("2nd unsambled sibling's age", (ifelse((!is.na(examine$CHAGE2) == TRUE), 
                                                           paste(", a", examine$CHAGE2, "year old"), )))
PROFILE$CHSEX2 <- c("2nd unsambled sibling's sex", (ifelse(examine$CHSEX2 == 1, print(" boy"), 
                                                           ifelse(examine$CHSEX2 == 2, print(" girl"), ))))
PROFILE$CHENRL2 <- c("2nd unsambled sibling's school",  
                     (ifelse(examine$CHENRL2 == 1, print(" who is being homeschooled"), 
                             ifelse(examine$CHENRL2 == 2, print(" who is enrolled in public or private school"),
                                    ifelse(examine$CHENRL2 == 3, print(" who is attending college"), 
                                           ifelse(examine$CHENRL2 == 4, print(" who is not in school"), ))))))
}else{print("")}

# THIRD SIBLING
if(is.na(examine$CHAGE3) == FALSE){
PROFILE$CHAGE3 <- c("3rd unsambled sibling's age", (ifelse((!is.na(examine$CHAGE3) == TRUE), 
                                                           paste(", a", examine$CHAGE3, "year old"), )))
PROFILE$CHSEX3 <- c("3rd unsambled sibling's sex", (ifelse(examine$CHSEX3 == 1, print(" boy"), 
                                                          ifelse(examine$CHSEX3 == 2, print(" girl"), ))))
PROFILE$CHENRL3 <- c("3rd unsambled sibling's school",  
                     (ifelse(examine$CHENRL3 == 1, print(" who is being homeschooled"), 
                             ifelse(examine$CHENRL3 == 2, print(" who is enrolled in public or private school"),
                                    ifelse(examine$CHENRL3 == 3, print(" who is attending college"), 
                                           ifelse(examine$CHENRL3 == 4, print(" who is not in school"), ))))))
}else{print("")}

# FOURTH SIBLING
if(is.na(examine$CHAGE4) == FALSE){
PROFILE$CHAGE4 <- c("4th unsambled sibling's age", (ifelse((!is.na(examine$CHAGE4) == TRUE), 
                                                           paste(", a", examine$CHAGE4, "year old"), )))
PROFILE$CHSEX4 <- c("4th unsambled sibling's sex", (ifelse(examine$CHSEX4 == 1, print(" boy"), 
                                                          ifelse(examine$CHSEX4 == 2, print(" girl"), ))))
PROFILE$CHENRL4 <- c("4th unsambled sibling's school", 
                     (ifelse(examine$CHENRL4 == 1, print(" who is being homeschooled"), 
                             ifelse(examine$CHENRL4 == 2, print(" who is enrolled in public or private school"),
                                    ifelse(examine$CHENRL4 == 3, print(" who is attending college"), 
                                           ifelse(examine$CHENRL4 == 4, print(" who is not in school"), ))))))
}else{print("")}

# Family's REASONS for homeschooling
PROFILE$HSSAFETYX <- c("Concern about the environment", 
                       (ifelse(examine$HSSAFETYX == 1, print(" due to concern about the school environment"), )))
PROFILE$HSDISSATX <- c("Dissatisfaction with academics", 
                         (ifelse(examine$HSDISSATX == 1, print(", for academic reasons"), )))
PROFILE$HSRELGON <- c("To provide religious instruction", 
                       (ifelse(examine$HSRELGON == 1, print(", to provide religious instruction"), )))
PROFILE$HSMORAL <- c("To provide moral instruction", 
                      (ifelse(examine$HSMORAL == 1, print(", to provide moral instruction"), )))
PROFILE$disability <- c("Disability", 
                       (ifelse(examine$disability == 1, print(", child has a disability"), )))
PROFILE$HSALTX <- c("Alternative education", 
                       (ifelse(examine$HSALTX == 1, print(", to provide a nontraditional education"), )))
PROFILE$HSFMLY <- c("family togetherness", 
                       (ifelse(examine$HSFMLY == 1, print(", to emphasize family life together"), )))
PROFILE$HSOTHERX <- c("other", 
                    (ifelse(examine$HSOTHERX == 1, print(", for other reasons"), )))
PROFILE$HSBULLY <- c("child was bullying", 
                      (ifelse(examine$HSBULLY == 1, print(", child was bullied"), )))
PROFILE$NOREASON <- c("parent selected no reason", 
                      (ifelse(examine$HSSAFETYX != 1 & examine$HSDISSATX != 1 & examine$HSRELGON != 1 & 
                                 examine$HSMORAL != 1 & examine$disability != 1 & examine$HSALTX != 1 & 
                                 examine$HSFMLY != 1 & examine$HSOTHERX != 1 & examine$HSBULLY != 1,
                              print(" respondent did not select any reasons"), print(""))))

# MOST IMPORTANT reason for homeschooling
PROFILE$HSMOSTX <- c("most important reason", 
                     (ifelse(is.na(examine$HSMOSTX), "respondent did not select a reason",
                     ifelse(examine$HSMOSTX == 1, "school environment", 
                     ifelse(examine$HSMOSTX == 2, "academic instruction",
                     ifelse(examine$HSMOSTX == 3, "religious instruction",
                     ifelse(examine$HSMOSTX == 4, "moral instruction",
                     ifelse(examine$HSMOSTX == 5, "child has a physical or mental health problem",
                     ifelse(examine$HSMOSTX == 6, "child has temporary illness",
                     ifelse(examine$HSMOSTX == 7, "child has special needs",
                     ifelse(examine$HSMOSTX == 8, "nontraditional approach",
                     ifelse(examine$HSMOSTX == 9, "family life",
                     ifelse(examine$HSMOSTX == 10, "other reason",
                     ifelse(examine$HSMOSTX == 11, "child was bullied", ))))))))))))))

# About the child's homeschooling

# Involvement in a homeschool group
PROFILE$HSASSNX <- c("homeschool group involvement", 
                     (ifelse(examine$HSASSNX == 1, print("are involved in a homeschool group or co-op"), 
                             ifelse(examine$HSASSNX == 2, print("are not involved in a homeschool group or co-op"), ))))
if(examine$HSASSNX == 1) {
PROFILE$HSFREQX <- c("frequency of co-op meetings", paste(", and have gone to approximately", sum(examine$HSFREQX), "meetings since September"))
}else{
  print("")
}

PROFILE$CHAGE1 <- c("1st unsambled sibling's age", (ifelse((!is.na(examine$CHAGE1) == TRUE), 
                                                           paste("a", examine$CHAGE1, "year old"), )))

# Is the child also enrolled in a school? 
PROFILE$HSENRL <- c("school enrollment", 
                     (ifelse(examine$HSENRL == 1, print("yes;"), 
                             ifelse(examine$HSENRL == 2, print("no;"), ))))

PROFILE$HOWEVER <- ifelse(rowSums(examine[3:9] == 1) >= 1 & examine$HSENRL == 2, " however,", "")

PROFILE$TYPE <- c("type of school", 
                  (ifelse(examine$EDCPUB == 1, print("also selected public school"), 
                      ifelse(examine$EDCCAT == 1, print("also selected Catholic school"), 
                          ifelse(examine$EDCREL == 1, print("also selected private religious school"),
                              ifelse(examine$EDCPRI == 1, print("also selected secular private school"),
                                  ifelse(examine$EDCINTK12 == 1, print("also selected virtual or online school"),
                                     ifelse(examine$EDCINTCOL == 1, print("also selected virtual college or university"),
                                         ifelse(examine$EDCCOL == 1, print("also selected college or university"), 
                                                print("selected only 'homeschool'") )))))))))

# EDCPUB = public school
# EDCCAT = Catholic school
# EDCREL = other private religious school
# EDCPRI = private, non-religious school
# EDCINTK12 = full-time online or virtual school

# Is the child taking online classes?
PROFILE$HSINTNET <- c("online courses", 
                    (ifelse(examine$HSINTNET == 1, print(". The student is taking online classes for all of their coursework"), 
                      ifelse(examine$HSINTNET == 2, print(". The student is taking online classes for half or more (but not all) of their coursework"), 
                        ifelse(examine$HSINTNET == 3, print(". The student is taking online classes for fewer than half of their coursework"), 
                          ifelse(examine$HSINTNET == 4, print(". The student is not taking online classes"), 
                             ))))))

# What are the online classes through?
if(examine$HSINTNET != 4) {
PROFILE$HSINTPUB <- c("online class source", (ifelse(examine$HSINTPUB == 1, print(", through the child's public school"), )))
PROFILE$HSINTPRI <- c("online class source", (ifelse(examine$HSINTPRI == 1, print(", through the child's private school"), )))
PROFILE$HSINTCOL <- c("online class source", (ifelse(examine$HSINTCOL == 1, print(", through a college"), )))
PROFILE$HSINTVRT <- c("online class source", (ifelse(examine$HSINTVRT == 1, print(", through a virtual academy or online charter school"), )))
PROFILE$HSINTCMP <- c("online class source", (ifelse(examine$HSINTCMP == 1, print(", through a company"), )))
PROFILE$HSINTK12 <- c("online class source", (ifelse(examine$HSINTK12 == 1, print(", through a another public or private school"), )))
PROFILE$HSINTIND <- c("online class source", (ifelse(examine$HSINTIND == 1, print(", through an independent instructor"), )))
PROFILE$HSINTOH <- c("online class source", (ifelse(examine$HSINTOH == 1, print(", through a nontraditional source"), )))
}else{
  print("")
}

# What grades has the student been homeschooled?
PROFILE$GK <- c("kindergarten", 
                (ifelse(examine$HOMEKX == 1, "K, ", ""))) 
PROFILE$G1 <- c("1st grade", 
                (ifelse(examine$HOME1 == 1, "1, ", ""))) 
PROFILE$G2 <- c("2nd grade", 
                (ifelse(examine$HOME2 == 1, "2, ", ""))) 
PROFILE$G3 <- c("3rd grade", 
                (ifelse(examine$HOME3 == 1, "3, ", ""))) 
PROFILE$G4 <- c("4th grade", 
                (ifelse(examine$HOME4 == 1, "4, ", ""))) 
PROFILE$G5 <- c("5th grade", 
                (ifelse(examine$HOME5 == 1, "5, ", ""))) 
PROFILE$G6 <- c("6th grade", 
                (ifelse((examine$HOME6 == 1), "6, ", "")))
PROFILE$G7 <- c("7th grade", 
                (ifelse(examine$HOME7 == 1, "7, ", "")))
PROFILE$G8 <- c("8th grade", 
                (ifelse(examine$HOME8 == 1, "8, ", "")))
PROFILE$G9 <- c("9th grade", 
                (ifelse(examine$HOME9 == 1, "9, ", "")))
PROFILE$G10 <- c("10th grade", 
                (ifelse(examine$HOME10 == 1, "10, ", "")))
PROFILE$G11 <- c("11th grade", 
                (ifelse(examine$HOME11 == 1, "11, ", "")))
PROFILE$G12 <- c("12th grade", 
                (ifelse(examine$HOME12 == 1, "12, ", "")))

PROFILE$HSSTYL <- c("homeschool style", 
                    ifelse(examine$HSSTYL == 1, "strictly follow a formal curriculum", 
                    ifelse(examine$HSSTYL == 2, "mostly use a formal curriculum, but also informal learning",   
                    ifelse(examine$HSSTYL == 3, "mostly use informal learning, but sometimes use a formal curriculum", 
                    ifelse(examine$HSSTYL == 4, "always use informal learning, and never follow a formal curriculum", 
                           )))))


# --------------------------------------------------

# PARENT ONE INFORMATION
PROFILE$P1REL <- c("Parent 1 relationship", 
                   (ifelse(examine$P1REL == 1, "biological parent", 
                    ifelse(examine$P1REL == 2, "adoptive parent",
                    ifelse(examine$P1REL == 3, "stepparent",
                    ifelse(examine$P1REL == 4, "foster parent",
                    ifelse(examine$P1REL == 5, "grandparent", 
                    ifelse(examine$P1REL == 6, "other guardian", 
                                   NA))))))))

PROFILE$P1MRSTA <- c("Parent 1 marital status", 
                     (ifelse(examine$P1MRSTA == 1, "is married", 
                      ifelse(examine$P1MRSTA == 2, "is widowed",
                      ifelse(examine$P1MRSTA == 3, "is divorced",
                      ifelse(examine$P1MRSTA == 4, "is separated",
                      ifelse(examine$P1MRSTA == 5, "is never married", 
                            "did not disclose marital status")))))))

PROFILE$P1RACEETH <- c("Parent 1 race", 
                       (ifelse(examine$P1WHITE == 1 & examine$P1HISPAN, "White ",
                        ifelse(examine$P1BLACK == 1, "Black ", 
                        ifelse(examine$P1ASIAN == 1, "Asian ", 
                        ifelse(examine$P1AMIND == 1, "American Indian ",
                        ifelse(examine$P1PACI == 1, "Pacific Islander ", 
                        ifelse(examine$P1HISPAN != 1, "Hispanic ", 
                                      ))))))))

PROFILE$P1AGE <- c("Parent 1's age", sum(examine$P1AGE))

PROFILE$P1SEX <- c("Parent 1's sex", (ifelse(examine$P1SEX == 1, print("male "), print("female "))))

PROFILE$P1EDUC <- c("Parent 1 education", 
                    (ifelse(examine$P1EDUC == 1, print("8th grade or less"), 
                     ifelse(examine$P1EDUC == 2, print("high school, no diploma"),
                     ifelse(examine$P1EDUC == 3, print("high school diploma or equivalent"),
                     ifelse(examine$P1EDUC == 4, print("vocational diploma after high school"),
                     ifelse(examine$P1EDUC == 5, print("some college, no degree"), 
                     ifelse(examine$P1EDUC == 6, print("associates degree"), 
                     ifelse(examine$P1EDUC == 7, print("bachelor's degree"), 
                     ifelse(examine$P1EDUC == 8, print("some graduate or professional school, no degree"), 
                     ifelse(examine$P1EDUC == 9, print("master's degree"), 
                     ifelse(examine$P1EDUC == 10, print("doctorate degree"), 
                     ifelse(examine$P1EDUC == 11, print("other professional degree beyond bachelor's degree"),  
                               NA)))))))))))))

# PARENT ONE EMPLOYMENT
PROFILE$P1EMPL <- c("Parent 1 employment", 
                    (ifelse(examine$P1EMPL == 1, print("employed for pay or income"), 
                            ifelse(examine$P1EMPL == 2, print("self-employed"),
                                   ifelse(examine$P1EMPL == 3, print("unemployed or out of work"),
                                          ifelse(examine$P1EMPL == 4, print("a full-time student"),
                                                 ifelse(examine$P1EMPL == 5, print("a stay at home parent"), 
                                                        ifelse(examine$P1EMPL == 6, print("retired"), 
                                                               ifelse(examine$P1EMPL == 7, print("disabled or unable to work"), 
                                                                      NA)))))))))

if(examine$P1HRSWK > 0) {
  PROFILE$P1HRSWK <- c("Parent 1 hrs/week", paste(", and works", examine$P1HRSWK, "hours per week"))
}else{
  print("")
}

# --------------------------------------------------

if(examine$P2GUARD == 2) {
PROFILE$SECPAR <- c("second parent", "There is no second parent in the household")
}else{
  print("")
}

if(examine$P2GUARD == 1) {
  
# PARENT TWO INFORMATION
PROFILE$P2RACEETH <- c("Parent 2 race", 
                       (ifelse(examine$P2WHITE == 1 & examine$P2HISPAN, "Parent 2 is a White ",
                            ifelse(examine$P2BLACK == 1, "Parent 2 is a Black ", 
                              ifelse(examine$P2ASIAN == 1, "Parent 2 is an Asian ", 
                                 ifelse(examine$P2AMIND == 1, "Parent 2 is an American Indian ",
                                    ifelse(examine$P2PACI == 1, "Parent 2 is a Pacific Islander ", 
                                       ifelse(examine$P2HISPAN != 1, "Parent 2 is a Hispanic ", 
                                                           ))))))))

PROFILE$P2SEX <- c("Parent 2's sex", (ifelse(examine$P2SEX == 1, print("male "), print("female "))))

PROFILE$P2REL <- c("Parent 2 relationship", 
                   (ifelse(examine$P2REL == 1, print("biological parent"), 
                     ifelse(examine$P2REL == 2, print("adoptive parent"),
                       ifelse(examine$P2REL == 3, print("stepparent"),
                         ifelse(examine$P2REL == 4, print("foster parent"),
                           ifelse(examine$P2REL == 5, print("grandparent"), 
                              ifelse(examine$P2REL == 6, print("other guardian"), 
                                        NA))))))))

PROFILE$P2MRSTA <- c("Parent 2 marital status", 
                   (ifelse(examine$P2MRSTA == 1, print(", is married"), )))
                     ifelse(examine$P2MRSTA == 2, print(", is widowed"),
                      ifelse(examine$P2MRSTA == 3, print(", is divorced"),
                       ifelse(examine$P2MRSTA == 4, print(", is separated"),
                         ifelse(examine$P2MRSTA == 5, print(", has never married"), 
                                         NA))))
                                                    
PROFILE$P2AGE <- c("Parent 2's age", paste(", and is", sum(examine$P2AGE), "years old. "))

PROFILE$P2EDUC <- c("Parent 2 education", 
                    (ifelse(examine$P2EDUC == 1, print("Parent 2's highest level of education is 8th grade or less. "), 
                      ifelse(examine$P2EDUC == 2, print("Parent 2's highest level of education is some high school, no diploma. "),
                      ifelse(examine$P2EDUC == 3, print("Parent 2's highest level of education is a high school diploma or equivalent. "),
                      ifelse(examine$P2EDUC == 4, print("Parent 2's highest level of education is a vocational diploma after high school. "),
                      ifelse(examine$P2EDUC == 5, print("Parent 2's highest level of education is some college, no degree. "), 
                      ifelse(examine$P2EDUC == 6, print("Parent 2's highest level of education is an associates degree. "), 
                      ifelse(examine$P2EDUC == 7, print("Parent 2's highest level of education is a bachelor's degree. "), 
                      ifelse(examine$P2EDUC == 8, print("Parent 2's highest level of education is some graduate or professional school, no degree. "), 
                      ifelse(examine$P2EDUC == 9, print("Parent 2's highest level of education is a master's degree. "), 
                      ifelse(examine$P2EDUC == 10, print("Parent 2's highest level of education is a doctorate degree. "), 
                      ifelse(examine$P2EDUC == 11, print("other professional degree beyond bachelor's degree. ",
                                      NA))))))))))))))

# PARENT TWO EMPLOYMENT
PROFILE$P2EMPL <- c("Parent 2 employment", 
                    (ifelse(examine$P2EMPL == 1, print("Parent 2 is employed for pay or income"), 
                      ifelse(examine$P2EMPL == 2, print("Parent 2 is self-employed"),
                       ifelse(examine$P2EMPL == 3, print("Parent 2 is unemployed or out of work"),
                        ifelse(examine$P2EMPL == 4, print("Parent 2 is a full-time student"),
                         ifelse(examine$P2EMPL == 5, print("Parent 2 is a stay at home parent"), 
                          ifelse(examine$P2EMPL == 6, print("Parent 2 is retired"), 
                           ifelse(examine$P2EMPL == 7, print("Parent 2 is disabled or unable to work"), 
                                              NA)))))))))

if(examine$P2HRSWK > 0) {
PROFILE$P2HRSWK <- c("Parent 2 hrs/week", paste(", and works", examine$P2HRSWK, "hours per week"))
}else{
  print("")
}

}else{
  print("")
}

# --------------------------------------------------

PROFILE$CENREG <- c("Region", 
                   (ifelse(examine$CENREG == 1, "Northeast", 
                           ifelse(examine$CENREG == 2, "South",
                                  ifelse(examine$CENREG == 3, "Midwest",
                                         ifelse(examine$CENREG == 4, "West", ))))))

PROFILE$ZIP18PO2 <- c("Poverty", 
                    (ifelse(examine$ZIP18PO2 == 1, "less than 5 percent", 
                            ifelse(examine$ZIP18PO2 == 2, "5 to 9 percent",
                                   ifelse(examine$ZIP18PO2 == 3, "10 to 19 percent",
                                          ifelse(examine$ZIP18PO2 == 4, "20 percent or more", ))))))

PROFILE$ZIPBLHI2 <- c("Black or Hispanic", 
                    (ifelse(examine$ZIPBLHI2 == 1, "less than 6 percent", 
                            ifelse(examine$ZIPBLHI2 == 2, "6 to 15 percent",
                                   ifelse(examine$ZIPBLHI2 == 3, "16 to 40 percent",
                                          ifelse(examine$ZIPBLHI2 == 4, "41 percent or more", ))))))

PROFILE$ZIPLOCL <- c("City type", 
                    (ifelse(examine$ZIPLOCL == 11, "large city", 
                     ifelse(examine$ZIPLOCL == 12, "midsize city",
                     ifelse(examine$ZIPLOCL == 13, "small city",
                     ifelse(examine$ZIPLOCL == 21, "large suburb", 
                     ifelse(examine$ZIPLOCL == 22, "midsize suburb", 
                     ifelse(examine$ZIPLOCL == 23, "small suburb",
                     ifelse(examine$ZIPLOCL == 31, "fringe town",
                     ifelse(examine$ZIPLOCL == 32, "distant town", 
                     ifelse(examine$ZIPLOCL == 33, "remote town", 
                     ifelse(examine$ZIPLOCL == 41, "fringe rural area",
                     ifelse(examine$ZIPLOCL == 42, "distant rural area",
                     ifelse(examine$ZIPLOCL == 43, "remote rural area", ))))))))))))))
                                                 
PROFILE$OWNRNTHB <- c("home ownership", 
                      ifelse(examine$OWNRNTHB == 1, "own their house or apartment", 
                             ifelse(examine$OWNRNTHB == 2, "rent a house or apartment", 
                                    ifelse(examine$OWNRNTHB == 3, "do not rent or own (they have have other arrangements),"))))

PROFILE$WELFARE <- c("welfare", ifelse(examine$HCHIP == 1 | examine$HMEDICAID == 1 | examine$HFOODST == 1 | 
                                         examine$HWIC == 1 | examine$HWELFTANST == 1, 
                                       "are on food stamps, Medicaid, or another form of welfare", 
                                       "are not on food stamps, Medicaid, or any form of welfare"))

PROFILE$TTLHHINC <- c("income", ifelse(examine$TTLHHINC == 1, "between $0 and $10,000", 
                                ifelse(examine$TTLHHINC == 2, "between $10,001 and $20,000", 
                                ifelse(examine$TTLHHINC == 3, "between $20,001 and $30,000", 
                                ifelse(examine$TTLHHINC == 4, "between $30,001 and $40,000",
                                ifelse(examine$TTLHHINC == 5, "between $40,001 and $50,000", 
                                ifelse(examine$TTLHHINC == 6, "between $50,001 and $60,000", 
                                ifelse(examine$TTLHHINC == 7, "between $60,001 and $75,000", 
                                ifelse(examine$TTLHHINC == 8, "between $75,001 and $100,000", 
                                ifelse(examine$TTLHHINC == 9, "between $100,001 and $150,000", 
                                ifelse(examine$TTLHHINC == 10, "between $150,001 and $200,000", 
                                ifelse(examine$TTLHHINC == 11, "between $200,001 and $250,000", 
                                ifelse(examine$TTLHHINC == 12, "between $250,001 or more", 
                                       )))))))))))))

# --------------------------------------------------

# various cleanup
PROFILE$V1 <- NULL

PROFILE[is.na(PROFILE)] = ""

PROFILE <- as.data.frame(PROFILE)
t(PROFILE)

# NARRATIVE

test <- paste("The child is ", PROFILE$RACEETH[2], " ", PROFILE$CSEX[2], ". The child was ", 
      PROFILE$AGE2018[2], " years old on December 31st, 2018, has a birthday in ", 
      PROFILE$BIRTHMONTH[2], ", and is in grade ", PROFILE$ALLGRADEX[2], ". ", 
      "The family’s questionnaire was completed ", PROFILE$ENGLSPANX[2], " and ", 
      PROFILE$MODECOMP[2], ". ", "There are ", PROFILE$HHTOTALXX[2], 
      " people living in the household. The child lives with ", 
      PROFILE$FAMILY19_BRD[2], ": ", PROFILE$HHMOM[2], PROFILE$HHDAD[2], PROFILE$HHSISSX[2], 
      ", and ", PROFILE$HHBROSX[2], ". ", "The child's siblings are: ", PROFILE$NOSIB[2], PROFILE$CHAGE1[2], PROFILE$CHSEX1[2], 
      PROFILE$CHENRL1[2], PROFILE$CHAGE2[2], PROFILE$CHSEX2[2], PROFILE$CHENRL2[2], PROFILE$CHAGE3[2], 
      PROFILE$CHSEX3[2], PROFILE$CHENRL3[2], PROFILE$CHAGE4[2], PROFILE$CHSEX4[2], PROFILE$CHENRL4[2], ". ", 
      "For verification of ages, the household has ", PROFILE$HHUNDR6X[2], ", ", PROFILE$HHUNDR10X[2], ", ", 
      PROFILE$HHUNDR16X[2], " and ", PROFILE$HHUNDR18X[2], ". ", "\n", "\n",
  "Parent 1 is a ", PROFILE$P1RACEETH[2], PROFILE$P1SEX[2], PROFILE$P1REL[2], ", ", PROFILE$P1MRSTA[2], 
      ", and is ", PROFILE$P1AGE[2], " years old. Parent 1's highest level of education is ", PROFILE$P1EDUC[2], 
      ". Parent 1 is ", PROFILE$P1EMPL[2], PROFILE$P1HRSWK[2], ". ", 
      PROFILE$P2RACEETH[2], PROFILE$P2SEX[2], PROFILE$P2REL[2], PROFILE$P2MRSTA[2], 
      PROFILE$P2AGE[2], PROFILE$P2EDUC[2], PROFILE$P2EMPL[2], PROFILE$P2HRSWK[2], 
      PROFILE$SECPAR[2], ".", "\n", "\n",
  "The child lives in a ", PROFILE$ZIPLOCL[2], " in the ", PROFILE$CENREG[2], ". In the child’s zip code, ", 
      PROFILE$ZIP18PO2[2], " of families with children under 18 live below the poverty line, and ", 
      PROFILE$ZIPBLHI2[2]," of people are Black or Hispanic. The respondents ", PROFILE$OWNRNTHB[2],
      " and ", PROFILE$WELFARE[2], ". The family's total income is ", PROFILE$TTLHHINC[2], ".", "\n", "\n",
  "The child is being homeschooled for the following reasons: ", PROFILE$HSSAFETYX[2], PROFILE$HSDISSATX[2], 
      PROFILE$HSRELGON[2], PROFILE$HSMORAL[2], PROFILE$disability[2], PROFILE$HSALTX[2], PROFILE$HSFMLY[2], 
      PROFILE$HSBULLY[2], PROFILE$HSOTHERX[2], PROFILE$NOREASON[2], ". ", 
      "The family's most important reason for homsechooling is: ", PROFILE$HSMOSTX[2], ". ", 
      "The child has been homeschooled for grades ", PROFILE$GK[2], PROFILE$G1[2], PROFILE$G2[2], PROFILE$G3[2],
      PROFILE$G4[2], PROFILE$G5[2], PROFILE$G6[2], PROFILE$G7[2], PROFILE$G8[2], PROFILE$G9[2], 
      PROFILE$G10[2], PROFILE$G11[2], PROFILE$G12[2], "and no others. The respondents ", PROFILE$HSSTYL[2], ", and ", 
      PROFILE$HSASSNX[2], PROFILE$HSFREQX[2], ". ", "When asked if the child is also enrolled in a school, ", 
      "the parent said ", PROFILE$HSENRL[2], PROFILE$HOWEVER[2], 
      " at the beginning of the survey, the respondent ", PROFILE$TYPE[2], 
      PROFILE$HSINTNET[2], PROFILE$HSINTPUB[2], PROFILE$HSINTPRI[2], PROFILE$HSINTCOL[2], PROFILE$HSINTVRT[2], 
      PROFILE$HSINTCMP[2], PROFILE$HSINTK12[2], PROFILE$HSINTIND[2], PROFILE$HSINTOH[2], ".", 
      sep = '')

cat(test)

sum(examine$HSWHOX) # child is homeschooled by
sum(examine$HSTUTOR) # child has a tutor
sum(examine$HSCOOP) # instruction provided by a coop
examine$HSOTHERXOS


table(PFIwKHS$AGE2018)


