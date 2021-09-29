# HOMESCHOOL RETENTION

# CREATE NEW COLUMNS & DATASET
# NOTE: run "Data Subsets" script first

HOME <- subset(PFI, SCHTYPE == 3)

# replace NAs with 0 ("child not homeschooled")
HOME$HOMEKX[is.na(HOME$HOMEKX)] <- 0
HOME$HOME1[is.na(HOME$HOME1)] <- 0
HOME$HOME2[is.na(HOME$HOME2)] <- 0
HOME$HOME3[is.na(HOME$HOME3)] <- 0
HOME$HOME4[is.na(HOME$HOME4)] <- 0
HOME$HOME5[is.na(HOME$HOME5)] <- 0
HOME$HOME6[is.na(HOME$HOME6)] <- 0
HOME$HOME7[is.na(HOME$HOME7)] <- 0
HOME$HOME8[is.na(HOME$HOME8)] <- 0
HOME$HOME9[is.na(HOME$HOME9)] <- 0
HOME$HOME10[is.na(HOME$HOME10)] <- 0
HOME$HOME11[is.na(HOME$HOME11)] <- 0
HOME$HOME12[is.na(HOME$HOME12)] <- 0

# replace 2s with 0 ("child not homeschooled")
HOME$HOMEKX[HOME$HOMEKX == 2] <- 0
HOME$HOME1[HOME$HOME1 == 2] <- 0
HOME$HOME2[HOME$HOME2 == 2] <- 0
HOME$HOME3[HOME$HOME3 == 2] <- 0
HOME$HOME4[HOME$HOME4 == 2] <- 0
HOME$HOME5[HOME$HOME5 == 2] <- 0
HOME$HOME6[HOME$HOME6 == 2] <- 0
HOME$HOME7[HOME$HOME7 == 2] <- 0
HOME$HOME8[HOME$HOME8 == 2] <- 0
HOME$HOME9[HOME$HOME9 == 2] <- 0
HOME$HOME10[HOME$HOME10 == 2] <- 0
HOME$HOME11[HOME$HOME11 == 2] <- 0
HOME$HOME12[HOME$HOME12 == 2] <- 0

# create new column with total number of years homeschooled
HOME$TOTAL <- HOME$HOMEKX + 
                HOME$HOME1 + 
                HOME$HOME2 + 
                HOME$HOME3 + 
                HOME$HOME4 + 
                HOME$HOME5 + 
                HOME$HOME6 + 
                HOME$HOME7 + 
                HOME$HOME8 + 
                HOME$HOME9 + 
                HOME$HOME10 + 
                HOME$HOME11 + 
                HOME$HOME12

table(HOME$TOTAL)
wpct(HOME$TOTAL, weight=HOME$FPWT, na.rm=TRUE)

# Create new column to find students who were always homeschooled
# 1 = always homeschooled, 0 = not always homeschooled
HOME$ALWAYS <- ifelse((HOME$TOTAL == (HOME$ALLGRADEX + 1)), 1, 0)

# Create a new column for first-year homeschooled students
# 1 = first year homeschooling, 0 = not first year
HOME$FIRST <- ifelse(HOME$TOTAL == 1, 1, 0)

# WERE THEY HOMESCHOOLED CONSEQUTIVE YEARS? ADD COLUMNS!

# First year homeschooling, after a gap (includes only grades 2-12)
HOME$FIRSTgap <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 0 & HOME$TOTAL > 1, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 0 & HOME$TOTAL > 1, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 0 & HOME$TOTAL > 1, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 0 & HOME$TOTAL > 1, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 0 & HOME$TOTAL > 1, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 0 & HOME$TOTAL > 1, 1, 
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 0 & HOME$TOTAL > 1, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 0 & HOME$TOTAL > 1, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 0 & HOME$TOTAL > 1, 1, 
                                                                              ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 0 & HOME$TOTAL > 1, 1, 
                                                                                     ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 0 & HOME$TOTAL > 1, 1, 
                                                                                                   0)))))))))))



# Second consecutive year homeschooling
HOME$SECOND <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 0, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 0, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                                              ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                                     ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                                            ifelse(HOME$ALLGRADEX == 1 & HOME$HOMEKX == 1, 1, 
                                                                                                   0))))))))))))

# Third consecutive year homeschooling
HOME$THIRD <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 0, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                              ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                                     ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1,
                                                                                                   0)))))))))))

# Fourth consecutive year homeschooling
HOME$FOURTH <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                     ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                            ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                   ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                          ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                 ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1,
                                                        ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                               ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                      ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                             ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1,
                                                                                           0))))))))))

# Fifth consecutive year homeschooling
HOME$FIFTH <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1, 
                                                                                     0)))))))))

# What percent homeschooled, returned to school, & came back to homeschooling?
# In columns HOMEKX through HOME12: 
#   -- 1 means "was homeschooled that year"
#   -- 2 means "was not homeschooled that year"
#   -- NA means "child has not reached that grade yet"
HSKids <- subset(PFI, SCHTYPE == 3)
YearsHomeschooled <- HSKids[, 58:70]
# turn all "not homeschooled" (2) before the first "was homeschooled" (1) into (0)
# this way all (2)s will mean "was not homeschooled, in between years of homeschooling"
YearsHomeschooled$HOMEKX[YearsHomeschooled$HOMEKX == 2] <- 0
YearsHomeschooled$HOME1[YearsHomeschooled$HOME1 == 2 & YearsHomeschooled$HOMEKX == 0] <- 0
YearsHomeschooled$HOME2[YearsHomeschooled$HOME2 == 2 & YearsHomeschooled$HOME1 == 0] <- 0
YearsHomeschooled$HOME3[YearsHomeschooled$HOME3 == 2 & YearsHomeschooled$HOME2 == 0] <- 0
YearsHomeschooled$HOME4[YearsHomeschooled$HOME4 == 2 & YearsHomeschooled$HOME3 == 0] <- 0
YearsHomeschooled$HOME5[YearsHomeschooled$HOME5 == 2 & YearsHomeschooled$HOME4 == 0] <- 0
YearsHomeschooled$HOME6[YearsHomeschooled$HOME6 == 2 & YearsHomeschooled$HOME5 == 0] <- 0
YearsHomeschooled$HOME7[YearsHomeschooled$HOME7 == 2 & YearsHomeschooled$HOME6 == 0] <- 0
YearsHomeschooled$HOME8[YearsHomeschooled$HOME8 == 2 & YearsHomeschooled$HOME7 == 0] <- 0
YearsHomeschooled$HOME9[YearsHomeschooled$HOME9 == 2 & YearsHomeschooled$HOME8 == 0] <- 0
YearsHomeschooled$HOME10[YearsHomeschooled$HOME10 == 2 & YearsHomeschooled$HOME9 == 0] <- 0
YearsHomeschooled$HOME11[YearsHomeschooled$HOME11 == 2 & YearsHomeschooled$HOME10 == 0] <- 0
# create vector with "TRUE" if there are any 2s and "FALSE" if there aren't
returners <- apply(YearsHomeschooled, 1, function(r) any(r %in% c(2)))
# create new column with 1s for returners, 0s for not returners
HOME$returner <- ifelse(returners == TRUE, 1, 0)

HOME$sibHS <- ifelse(HOME$CHENRL1 == 1 & !is.na(HOME$CHENRL1), 1,
                     ifelse(HOME$CHENRL2 == 1 & !is.na(HOME$CHENRL2), 1,
                            ifelse(HOME$CHENRL3 == 1 & !is.na(HOME$CHENRL3), 1, 
                                   ifelse(HOME$CHENRL4 == 1 & !is.na(HOME$CHENRL4), 1, 0))))

HOME$sibENRL <- ifelse(HOME$CHENRL1 == 2 & !is.na(HOME$CHENRL1), 1,
                       ifelse(HOME$CHENRL2 == 2 & !is.na(HOME$CHENRL2), 1,
                              ifelse(HOME$CHENRL3 == 2 & !is.na(HOME$CHENRL3), 1, 
                                     ifelse(HOME$CHENRL4 == 2 & !is.na(HOME$CHENRL4), 1, 0))))

HOME$sibBOTH <- ifelse(HOME$sibHS == 1 & HOME$sibENRL == 1, 1, 0)

# HAVE CREATED ALL NEW VALUES AT THIS POINT
# if need to create new values, add here, before creating design object!

# CREATE SURVEY PACKAGE DESIGN OBJECT

HOMEdesign <- svrepdesign(
  data = HOME, 
  repweights = subset(HOME, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(HOMEdesign)

# END DESIGN CREATION


# YEAR OVER YEAR RETENTION RATE

# CREATE a table for years homeschooled and grades, with weighted counts
YearsTable <- round(svytable(~TOTAL + ALLGRADEX, HOMEdesign))
YearsTable

# FIRST YEAR of homeschooling, number and percent
round(svytable(~FIRST, HOMEdesign)) 
round(sum(HOME$FIRST * HOME$FPWT)/sum(HOME$FPWT), digits = 3)
# excluding kindergarten
NoK <- subset(HOME, ALLGRADEX > 0)
round(sum(NoK$FIRST * NoK$FPWT)/sum(NoK$FPWT), digits = 3)

# SECOND YEAR RETENTION RATES

#  METHOD 1: All second year (grades 1-12) OVER all first year (grades K-11)
# — this method asks: what percent of students homeschooled for one year go on
# to be homeschooled a second year AT SOME POINT (not year over year)
# Note: we use YearsTable for this calculation (YearsTable was created earlier)
sum(YearsTable[2, 2:13]) / sum(YearsTable[1, 1:12])
# result: 47.1% of first-year hsers go on to a second year at some point

#  METHOD 2: All second *consecutive year* homeschoolers (grades 1-12) 
# *who never homeschooled before last year* OVER all first year homeschoolers 
# — this method asks: what percent of students homeschooled *for the very 
# first time* go on to homeschool *the following* year?
SecondTable <- round(svytable(~(SECOND == 1 & TOTAL < 3) + ALLGRADEX, HOMEdesign))
sum(SecondTable[2,])  / sum(YearsTable[1, 1:12])
# result: 41.5% of first-time hsers go on to homeschool the following year

# THIRD YEAR RETENTION RATES
ThirdTable <- round(svytable(~(THIRD == 1 & TOTAL < 4) + ALLGRADEX, HOMEdesign))
# rate over second year homeschooling
sum(ThirdTable[2,])  / sum(SecondTable[2, 1:12])
# result: 73.3% of those who homeschool for a 2nd consecutive yr go on to a 3rd

# FOURTH YEAR RETENTION RATES
FourthTable <- round(svytable(~(FOURTH == 1 & TOTAL < 5) + ALLGRADEX, HOMEdesign))
# rate over second year homeschooling
sum(FourthTable[2,])  / sum(ThirdTable[2, 1:12])
# result: 88.8% of those who homeschool for a 3rd consecutive yr go on to a 4th

# FIFTH YEAR RETENTION RATES
FifthTable <- round(svytable(~(FIFTH == 1 & TOTAL < 6) + ALLGRADEX, HOMEdesign))
# rate over second year homeschooling
sum(FifthTable[2,])  / sum(FourthTable[2, 1:12])
# result: 98.4% of those who homeschool for a 3rd consecutive yr go on to a 4th

# HOW MANY FAMILIES HAVE BEEN HOMESCHOOLING ONLY A FEW YEARS?

# TOTAL first year homeschooling
round(sum((HOME$TOTAL == 1) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second year homeschooling
round(sum((HOME$TOTAL < 3) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second AND third year homeschooling
round(sum((HOME$TOTAL < 4) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second AND third AND fourth year homeschooling
round(sum((HOME$TOTAL < 5) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# ALWAYS HOMESCHOOLED

# overall v elementary v secondary
svymean(~ALWAYS, HOMEdesign)
# excluding kindergarten
svymean(~ALWAYS, subset(HOMEdesign,ALLGRADEX > 0))
# elementary school, excluding kindergarten
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary ==1 & ALLGRADEX > 0))
# secondary school
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary ==2))

# percent by individual grade
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 1))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 2))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 3))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 4))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 5))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 6))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 7))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 8))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 9))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 10))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 11))
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX == 12))


# ALWAYS HOMESCHOOLED V LATER HOMESCHOOLED
# DEMOGRAPHIC DIFFERENCES

ALW <- subset(HOME, ALWAYS == 1)
LAT <- subset(HOME, ALWAYS == 0)

# White or minority
 wpct(ALW$white_nonwhite, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$white_nonwhite, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((white_nonwhite == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
 # Bachelor's degree or no
 wpct(ALW$ba_no_ba, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$ba_no_ba, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((ba_no_ba == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
 # Two parent or single parent
 wpct(ALW$two_parent_or_single, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$two_parent_or_single, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((two_parent_or_single == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
 # Stay at home parent or no
 wpct(ALW$sahp, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$sahp, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((sahp == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
 # Income below the poverty line or no
 wpct(ALW$poverty, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$poverty, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((poverty == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)

 # English or no
 wpct(ALW$english_or_no, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$english_or_no, weight=LAT$FPWT, na.rm= TRUE)
 
 svyttest((english_or_no == 1) ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 

 # ALWAYS HOMESCHOOLED V LATER HOMESCHOOLED
# DIFFERENCES IN REASONS FOR HOMESCHOOLING
 
# Religious reasons for homeschooling
 svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON == 1))
 svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON == 2))
 
 svyttest(HSRELGON ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
# ALL disability (HSDISABLX, HSILLX, and HSSPCLNDX combined)
 svymean(~ ALWAYS, subset(HOMEdesign, disability == 1))
 svymean(~ ALWAYS, subset(HOMEdesign, disability == 2))
 
 svyttest(disability ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)

# "To emphasize family life together"
 svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY == 1))
 svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY == 2))
 
 svyttest(HSFMLY ~ (ALWAYS == 1), 
          HOMEdesign,
          na.rm=TRUE)
 
# COMPARE FIRST YEAR ELEMENTARY AND FIRST YEAR SECONDARY STUDENTS
 
 ELM <- subset(HOME, FIRST == 1 & elementary_secondary == 1)
 SEC <- subset(HOME, FIRST == 1 & elementary_secondary == 2)
 
 # White or no
 wpct(ELM$white_nonwhite, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$white_nonwhite, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(white_nonwhite ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)
 
 # College degree or no 
 wpct(ELM$ba_no_ba, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$ba_no_ba, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(ba_no_ba ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)

 # Two parent or single parent
 wpct(ELM$two_parent_or_single, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$two_parent_or_single, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(two_parent_or_single ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)
 
 # Stay at home parent or no
 wpct(ELM$sahp, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$sahp, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(sahp ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)
 
 # Income under the poverty line
 wpct(ELM$poverty, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$poverty, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(poverty ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)
 
 # Food stamps or no
 wpct(ELM$food_stamps, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$food_stamps, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(food_stamps ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)
 
 # English or no
 wpct(ELM$english_or_no, weight=ELM$FPWT, na.rm= TRUE)
 wpct(SEC$english_or_no, weight=SEC$FPWT, na.rm= TRUE)
 
 svyttest(english_or_no ~ elementary_secondary, 
          subset(HOMEdesign, FIRST == 1),
          na.rm=TRUE)

# DEFUNCT: FIRST YEAR v. SOMETIMES v. ALWAYS HOMESCHOOLED STUDENTS
# demographic differences (elementary and secondary are combined)
ALW <- subset(HOME, ALWAYS == 1)
SOM <- subset(HOME, ALWAYS == 0 & FIRST == 0)
LAT <- subset(HOME, FIRST == 1)
# Bachelor's degree or no
wpct(ALW$ba_no_ba, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$ba_no_ba, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$ba_no_ba, weight=LAT$FPWT, na.rm= TRUE)
# White or minority
wpct(ALW$white_nonwhite, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$white_nonwhite, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$white_nonwhite, weight=LAT$FPWT, na.rm= TRUE)
# Two parent or single parent
wpct(ALW$two_parent_or_single, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$two_parent_or_single, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$two_parent_or_single, weight=LAT$FPWT, na.rm= TRUE)
# Stay at home parent or no
wpct(ALW$sahp, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$sahp, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$sahp, weight=LAT$FPWT, na.rm= TRUE)
# Income over 50K or under 50K
wpct(ALW$income, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$income, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$income, weight=LAT$FPWT, na.rm= TRUE)
# Food stamps or no
wpct(ALW$food_stamps, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$food_stamps, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$food_stamps, weight=LAT$FPWT, na.rm= TRUE)
# English or no
wpct(ALW$english_or_no, weight=ALW$FPWT, na.rm= TRUE)
wpct(SOM$english_or_no, weight=SOM$FPWT, na.rm= TRUE)
wpct(LAT$english_or_no, weight=LAT$FPWT, na.rm= TRUE)


# IS SECONDARY SCHOOL A DIFFERENT SET OF KIDS

# Number of students in each level:
svytable(~elementary_secondary, HOMEdesign)

# Percent of secondary students (7-12) homeschooled in grade 6
svymean(~HOME6, subset(HOMEdesign, elementary_secondary == 2))

# Percent of high school students (9-12) homeschooled in grade 6
svymean(~HOME6, subset(HOMEdesign, ALLGRADEX > 8))

# Percent of high school students (9-12) homeschooled in grade 8
svymean(~HOME8, subset(HOMEdesign, ALLGRADEX > 8))

# PERCENT OF SECONDARY STUDENTS HOMESCHOOLED LONGTERM
svymean(~TOTAL > 7, subset(HOMEdesign, elementary_secondary == 2))

# What percent of homeschooled high school students 
# were not homeschooled before high school?

# combine rows for grades K-8, so that 0 means no homeschooling before 9th grade
HOME$GK8 <- HOME$HOMEKX + HOME$HOME1 + HOME$HOME2 + HOME$HOME3 + HOME$HOME4 + 
  HOME$HOME5 + HOME$HOME6 + HOME$HOME7 + HOME$HOME8

# create a column showing which homeschooled high school students 
# were NOT homeschooled before high school
HOME$NBHS <- ifelse(((HOME$ALLGRADEX == 9 | 
                        HOME$ALLGRADEX == 10 | 
                        HOME$ALLGRADEX == 11 | 
                        HOME$ALLGRADEX == 12) & 
                       HOME$GK8 == 0), 1, 0)

# multiply the not-homeschooled-before-HS HS students by their weight, then sum
num <- HOME$NBHS*HOME$FPWT
NBHS <- sum(num)

# Calculate the number of students homeschooled in high school
HS <- sum((HOME$ALLGRADEX == 9)*HOME$FPWT) +
  sum((HOME$ALLGRADEX == 10)*HOME$FPWT) +
  sum((HOME$ALLGRADEX == 11)*HOME$FPWT) +
  sum((HOME$ALLGRADEX == 12)*HOME$FPWT)

# what percent were not homeschooled before high school?
NBHS/HS

# end this calculation


# FIRST YEAR TURNOVER, ELEMENTARY V SECONDARY
# Creating table, first-year turnover for all students combined
SecondTable <- round(svytable(~(SECOND == 1 & TOTAL < 3) + ALLGRADEX, HOMEdesign))
sum(SecondTable[2, 2:13])  / sum(YearsTable[1, 1:12])
# second-year hsers in grades 1-7 v first-year hsers in grades K-6
sum(SecondTable[2, 2:8]) / sum(YearsTable[1, 1:7])
# second-year hsers in grades 8-12 v first-year hsers in grades 7-11
sum(SecondTable[2, 9:13]) / sum(YearsTable[1, 8:12])


# HOMESCHOOL RETURNERS

svymean(~returner, HOMEdesign)
svymean(~returner, subset(HOMEdesign, elementary_secondary == 1))
svymean(~returner, subset(HOMEdesign, elementary_secondary == 2))

# KIDS WITH SIBLINGS IN SCHOOL

svymean(~sibHS, HOMEdesign)
svymean(~sibENRL, HOMEdesign)

# by length of homeschooling and age
svymean(~sibENRL, subset(HOMEdesign, ALWAYS == 1))
svymean(~sibENRL, subset(HOMEdesign, FIRST == 1))

svymean(~sibENRL, subset(HOMEdesign, elementary_secondary == 1))
svymean(~sibENRL, subset(HOMEdesign, elementary_secondary == 2))

svymean(~sibENRL, subset(HOMEdesign, FIRST == 1 & elementary_secondary == 2))

# by poverty level
svymean(~sibENRL, subset(HOMEdesign, poverty == 1))
svymean(~sibENRL, subset(HOMEdesign, poverty != 1))

HOMEdesign <- update(HOMEdesign,  poverty_no = ifelse(poverty == 1, "poverty", ifelse(poverty != 1, "no", NA)))

svyttest(sibENRL ~ poverty_no, 
         HOMEdesign,
         na.rm=TRUE)

# by parent education
svymean(~sibENRL, subset(HOMEdesign, ba_no_ba == 1))
svymean(~sibENRL, subset(HOMEdesign, ba_no_ba == 2))

svyttest(sibENRL ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# by reasons for homeschooling
svymean(~sibENRL, subset(HOMEdesign, HSRELGON == 1))
svymean(~sibENRL, subset(HOMEdesign, HSDISABLX == 1))

# END SCRIPT