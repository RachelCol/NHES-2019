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
# start by creating table and adding kindergarten back in

# CREATE a table for years homeschooled and grades, with weighted counts
YearsTable <- round(svytable(~TOTAL + ALLGRADEX, HOMEdesign))
YearsTable
# CALCULATE NUMBER OF KINDERGARTENERS (WEIGHTED) using "PFIwK" dataset
# from the main Data Subsets script.
KONLY <- subset(PFIwK, ALLGRADEX == 0 & SCHTYPE == 3)
k <- sum(KONLY$FPWT)
# ADD column for kindergarten to YearsTable
k <- c(k, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
YearsTable <- cbind(YearsTable, "0" = k)
YearsTable <- round(YearsTable[,c("0",1,2,3,4,5,6,7,8,9,10,11,12)])
YearsTable

# SECOND YEAR RETENTION RATE SUMMARIES
# Overall second year retention rate
   sum(YearsTable[2, 1:12])/sum(YearsTable[1, 0:11])
# Elementary school second year retention rate
YearsTableE <- YearsTable[, 0:6]
   sum(YearsTableE[2, 1:6])/sum(YearsTableE[1, 0:5]) 
# Secondary school second year retention rate:
YearsTableS <- YearsTable[, 6:12]
   sum(YearsTableS[2, 2:7]) / sum(YearsTableS[1, 1:6]) 

# YEAR OVER YEAR RETENTION RATES
# Second over first year retention rate
 sum(YearsTable[2, 1:12])/sum(YearsTable[1, 0:11])
# Third over second year retention rate
 sum(YearsTable[3, 2:12])/sum(YearsTable[2, 1:11])
# Fourth over third year retention rate
 sum(YearsTable[4, 3:12])/sum(YearsTable[3, 2:11])
# Fifth over fourth year retention rate
 sum(YearsTable[5, 4:12])/sum(YearsTable[4, 3:11])
# Sixth over Fifth year retention rate
 sum(YearsTable[6, 5:12])/sum(YearsTable[5, 4:11])
# Seventh over Sixth year retention rate
 sum(YearsTable[7, 6:12])/sum(YearsTable[6, 5:11])
# Eighth over Seventh year retention rate
 sum(YearsTable[8, 7:12])/sum(YearsTable[7, 6:11])
# Ninth over Eighth year retention rate
 sum(YearsTable[9, 8:12])/sum(YearsTable[8, 7:11])


# FIRST YEAR HOMESCHOOLERS
 
# overall v elementary v secondary
svymean(~FIRST, HOMEdesign)
svymean(~FIRST, subset(HOMEdesign, elementary_secondary == 1))
svymean(~FIRST, subset(HOMEdesign, elementary_secondary == 2))

# percent by individual grade
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 1))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 2))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 3))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 4))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 5))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 6))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 7))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 8))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 9))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 10))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 11))
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX == 12))

# By grade range
# Grades 1-2
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 1 | 
                         ALLGRADEX == 2))
# Grades 3-4
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 3 | 
                         ALLGRADEX == 4))
# Grades 5-6
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 5 | 
                         ALLGRADEX == 6))
# Grades 7-8
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 7 | 
                         ALLGRADEX == 8))
# Grades 9-10
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 9 | 
                         ALLGRADEX == 10))
# Grades 11-12
svymean(~FIRST, subset(HOMEdesign, 
                       ALLGRADEX == 11 | 
                         ALLGRADEX == 12))


# ALWAYS HOMESCHOOLED

# overall v elementary v secondary
svymean(~ALWAYS, HOMEdesign)
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary ==1))
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

# By grade range
# Grades 1-2
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 1 | 
                          ALLGRADEX == 2))
# Grades 3-4
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 3 | 
                          ALLGRADEX == 4))
# Grades 5-6
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 5 | 
                          ALLGRADEX == 6))
# Grades 7-8
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 7 | 
                          ALLGRADEX == 8))
# Grades 9-10
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 9 | 
                          ALLGRADEX == 10))
# Grades 11-12
svymean(~ALWAYS, subset(HOMEdesign, 
                        ALLGRADEX == 11 | 
                          ALLGRADEX == 12))


# SHOWING SECONDARY SCHOOL IS A DIFFERENT SET OF KIDS

# Percent of secondary students (7-12) homeschooled in grade 6
svymean(~HOME6, subset(HOMEdesign, elementary_secondary == 2))

# Percent of high school students (9-12) homeschooled in grade 6
svymean(~HOME6, subset(HOMEdesign, ALLGRADEX > 8))

# PERCENT OF SECONDARY STUDENTS HOMESCHOOLED LONGTERM
svymean(~TOTAL > 7, subset(HOMEdesign, elementary_secondary ==2))


# ALWAYS HOMESCHOOLED, BY REASONS FOR HOMESCHOOLING

# What percent of religious homeschoolers were always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON == 1))
# What percent of nonreligious homeschoolers were always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON == 2))
# Finding: 50.0% of religious homeschoolers have always homeschooled;
#          23.7% of NONreligious homeschoolers have always homeschooled

# ALL disability (HSDISABLX, HSILLX, and HSSPCLNDX combined)
# What percent of disabled homeschoolers were always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, disability == 1))
# What percent of NOT disabled homeschoolers were always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, disability == 2))
# Finding: 16.7% of disabled homeschoolers have always homeschooled;
#          46.5% of NONdisabled homeschoolers have always homeschooled

# What percent of respondents who homeschool "to emphasize family
# life together" have always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY == 1))
# What percent of nonreligious homeschoolers were always homeschooled?
svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY == 2))
# Finding: 45.9% of "family life together" homeschoolers have always homeschooled;
#          19.2% of NON "family life together" homeschoolers have always homeschooled


# ALWAYS HOMESCHOOLED V LATER HOMESCHOOLED
# DEMOGRAPHIC DIFFERENCES

# PERCENT OF GROUP ALWAYS HOMESCHOOLED
# Bachelor's degree or no
 svymean(~ALWAYS, subset(HOMEdesign, ba_no_ba == 1))
 svymean(~ALWAYS, subset(HOMEdesign, ba_no_ba == 2))
# White or minority
 svymean(~ALWAYS, subset(HOMEdesign, white_nonwhite == 1))
 svymean(~ALWAYS, subset(HOMEdesign, white_nonwhite == 2))
# Two parent or single parent
 svymean(~ALWAYS, subset(HOMEdesign, two_parent_or_single == 1))
 svymean(~ALWAYS, subset(HOMEdesign, two_parent_or_single == 2))
# Stay at home parent or no
 svymean(~ALWAYS, subset(HOMEdesign, sahp == 1))
 svymean(~ALWAYS, subset(HOMEdesign, sahp == 2))
# Income over 50K or under 50K
 svymean(~ALWAYS, subset(HOMEdesign, income > 2))
 svymean(~ALWAYS, subset(HOMEdesign, income < 3))
# Food stamps or no
 svymean(~ALWAYS, subset(HOMEdesign, food_stamps == 1))
 svymean(~ALWAYS, subset(HOMEdesign, food_stamps == 2))
# English or no
 svymean(~ALWAYS, subset(HOMEdesign, english_or_no == 1))
 svymean(~ALWAYS, subset(HOMEdesign, english_or_no == 2))

# ALWAYS HOMESCHOOLED V LATER HOMESCHOOLERS, BY DEMOGRAPHIC FACTORS
ALW <- subset(HOME, ALWAYS == 1)
LAT <- subset(HOME, ALWAYS == 0)
# Bachelor's degree or no
 wpct(ALW$ba_no_ba, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$ba_no_ba, weight=LAT$FPWT, na.rm= TRUE)
# White or minority
 wpct(ALW$white_nonwhite, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$white_nonwhite, weight=LAT$FPWT, na.rm= TRUE)
# Two parent or single parent
 wpct(ALW$two_parent_or_single, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$two_parent_or_single, weight=LAT$FPWT, na.rm= TRUE)
# Stay at home parent or no
 wpct(ALW$sahp, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$sahp, weight=LAT$FPWT, na.rm= TRUE)
# Income over 50K or under 50K
 wpct(ALW$income, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$income, weight=LAT$FPWT, na.rm= TRUE)
# Food stamps or no
 wpct(ALW$food_stamps, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$food_stamps, weight=LAT$FPWT, na.rm= TRUE)
# English or no
 wpct(ALW$english_or_no, weight=ALW$FPWT, na.rm= TRUE)
 wpct(LAT$english_or_no, weight=LAT$FPWT, na.rm= TRUE)
 
 # SECONDARY SCHOOL AWLAYS HOMESCHOOLERS V SOME TIME V FIRST TIME HOMESCHOOLERS
 ALW <- subset(HOME, ALWAYS == 1 & elementary_secondary == 2)
 SOM <- subset(HOME, ALWAYS == 0 & FIRST == 0 & elementary_secondary == 2)
 LAT <- subset(HOME, FIRST == 1 & elementary_secondary == 2)
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
 
 
# EXAMINING NON ENGLISH SPEAKING PARENTS

# Total percent of students with non-English speaking parents
wpct(HOME$english_or_no, weight=HOME$FPWT, na.rm = TRUE)

# What percent of students without English speaking parents
# are first-year hoemschooled students
NoE <- subset(HOME, english_or_no == 2)
wpct(NoE$TOTAL, weight=NoE$FPWT, na.rm = TRUE)

# Confidence interval
svyby(~english_or_no == 1, ~(elementary_secondary == 2 & FIRST == 1), 
      HOMEdesign, 
      svymean, vartype="ci")


# START BY CREATING A CHART SHOWING HOW DIFFERENT OVERALL
# FIRST YEAR HOMESCHOOLERS ARE, AND WRITING ABOUT WHY

# HOW DIFFERENT ARE FIRST-YEAR HOMESCHOOLERS?
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


# END SCRIPT