# HOMESCHOOL RETENTION

# note: This script is designed to run after 0_data_subsets script.

# -----

# NUMBER OF CHILDREN HOMESCHOOLED

# Number of children homeschooled in each grade
round(svytable(~ALLGRADEX, HOMEdesign))
# Total number of children homeschooled
sum(round(svytable(~ALLGRADEX, HOMEdesign)))

# PERCENT OF CHILDREN IN THEIR FIRST YEAR OF HOMESCHOOLING

# Number of children homeschooled in each grade
round(svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1)))
# Total number of children in their first year of homeschooling
sum(round(svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1))))
# Percent in their first year of homeschooling, by grade
round(((svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1))) / 
         svytable(~ALLGRADEX, HOMEdesign)*100), digits = 1)

# note: Bar plots for the above are found in 2.2_retention_plots.

# -----

# FIRST YEAR: OVERALL 
svymean(~FIRST, HOMEdesign)
# overall, excluding kindergarten
svymean(~FIRST, subset(HOMEdesign, ALLGRADEX > 0))
# elementary school
svymean(~FIRST, subset(HOMEdesign, elementary_secondary == 1))
# elementary school, excluding kindergarten
svymean(~FIRST, subset(HOMEdesign, elementary_secondary == 1 & ALLGRADEX > 0))
# secondary school
svymean(~FIRST, subset(HOMEdesign, elementary_secondary == 2))

# ALWAYS: OVERALL
svymean(~ALWAYS, HOMEdesign)
# overall, excluding kindergarten
svymean(~ALWAYS, subset(HOMEdesign, ALLGRADEX > 0))
# elementary school
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary == 1))
# elementary school, excluding kindergarten
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary == 1 & ALLGRADEX > 0))
# secondary school
svymean(~ALWAYS, subset(HOMEdesign, elementary_secondary == 2))

# Percent always homeschooled, by grade
round(((svytable(~ALLGRADEX, subset(HOMEdesign, ALWAYS == 1))) / 
         svytable(~ALLGRADEX, HOMEdesign)*100), digits = 1)
# -----

# CALCULATING RETENTION RATE

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

# DECISION: Method 2 is preferable to Method 1

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

# FIRST YEAR TURNOVER, ELEMENTARY V SECONDARY
# Creating table, first-year turnover for all students combined
SecondTable <- round(svytable(~(SECOND == 1 & TOTAL < 3) + ALLGRADEX, HOMEdesign))
sum(SecondTable[2, 2:13])  / sum(YearsTable[1, 1:12])
# second-year hsers in grades 1-7 v first-year hsers in grades K-6
sum(SecondTable[2, 2:8]) / sum(YearsTable[1, 1:7])
# second-year hsers in grades 8-12 v first-year hsers in grades 7-11
sum(SecondTable[2, 9:13]) / sum(YearsTable[1, 8:12])

# -----

# HOW MANY FAMILIES HAVE BEEN HOMESCHOOLING ONLY A FEW YEARS?

# TOTAL first year homeschooling
round(sum((HOME$TOTAL == 1) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second year homeschooling
round(sum((HOME$TOTAL < 3) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second AND third year homeschooling
round(sum((HOME$TOTAL < 4) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# TOTAL first AND second AND third AND fourth year homeschooling
round(sum((HOME$TOTAL < 5) * HOME$FPWT)/sum(HOME$FPWT), digits = 3)

# -----

# IS SECONDARY SCHOOL A DIFFERENT SET OF KIDS?

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

# What percent of homeschooled high school students were not homeschooled
# before high school, when including homeschool returners??

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

# -----

# How long have students been homeschooled, by grade level?

grade_1 <- subset(HOME, ALLGRADEX == 1)
wpct(grade_1$TOTAL, weight=grade_1$FPWT, na.rm=TRUE)

grade_2 <- subset(HOME, ALLGRADEX == 2)
wpct(grade_2$TOTAL, weight=grade_2$FPWT, na.rm=TRUE)

grade_3 <- subset(HOME, ALLGRADEX == 3)
wpct(grade_3$TOTAL, weight=grade_3$FPWT, na.rm=TRUE)

grade_4 <- subset(HOME, ALLGRADEX == 4)
wpct(grade_4$TOTAL, weight=grade_4$FPWT, na.rm=TRUE)

grade_5 <- subset(HOME, ALLGRADEX == 5)
wpct(grade_5$TOTAL, weight=grade_5$FPWT, na.rm=TRUE)

grade_6 <- subset(HOME, ALLGRADEX == 6)
wpct(grade_6$TOTAL, weight=grade_6$FPWT, na.rm=TRUE)

grade_7 <- subset(HOME, ALLGRADEX == 7)
wpct(grade_7$TOTAL, weight=grade_7$FPWT, na.rm=TRUE)

grade_8 <- subset(HOME, ALLGRADEX == 8)
wpct(grade_8$TOTAL, weight=grade_8$FPWT, na.rm=TRUE)

grade_9 <- subset(HOME, ALLGRADEX == 9)
wpct(grade_9$TOTAL, weight=grade_9$FPWT, na.rm=TRUE)

grade_10 <- subset(HOME, ALLGRADEX == 10)
wpct(grade_10$TOTAL, weight=grade_10$FPWT, na.rm=TRUE)

grade_11 <- subset(HOME, ALLGRADEX == 11)
wpct(grade_11$TOTAL, weight=grade_11$FPWT, na.rm=TRUE)

grade_12 <- subset(HOME, ALLGRADEX == 12)
wpct(grade_12$TOTAL, weight=grade_12$FPWT, na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 1))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 2))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 3))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 4))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 5))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 6))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 7))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 8))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 9))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 10))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 11))
svymean(~TOTAL, subset(HOMEdesign, ALLGRADEX == 12))
 
# -----

# END SCRIPT