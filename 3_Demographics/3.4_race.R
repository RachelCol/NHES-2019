# Student's race, school level, college, and SES

# note: This script is designed to run after 0_data_subsets script.

# note: This script combines all nonwhite students For analysis of first-year
# homeschooling broken down by race/ethnicity, see 3.8_first_v_public.

# Create subsets for comparison of homeschool grades K-6 and 7-12
EL_HS <- subset(HOME, elementary_secondary == 1)
SEC_HS <- subset(HOME, elementary_secondary == 2)

EL_HS_W <- subset(EL_HS, white_nonwhite == 1) # create a white subset
SEC_HS_W <- subset(SEC_HS, white_nonwhite == 1) # create a white subset

EL_HS_NW <- subset(EL_HS, white_nonwhite == 2) # create a nonwhite subset
SEC_HS_NW <- subset(SEC_HS, white_nonwhite == 2) # create a nonwhite subset

# -----

# HIGH SES,grades K-6, white students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 1 & SCHTYPE == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1 & white_nonwhite == 1),
         na.rm=TRUE)

# HIGH SES, grades K-6, nonwhite students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 2 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 2 & SCHTYPE == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1 & white_nonwhite == 2),
         na.rm=TRUE)

# HIGH SES, grades 7-12, white students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 1 & SCHTYPE == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2 & white_nonwhite == 1),
         na.rm=TRUE)

# HIGH SES, grades 7-12, nonwhite students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 2 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 2 & SCHTYPE == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2 & white_nonwhite == 2),
         na.rm=TRUE)

# -----

# PARENT HAS A COLLEGE DEGREE, homeschool only, white students

round(wpct(EL_HS_W$ba_no_ba, weight=EL_HS_W$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS_W$ba_no_ba, weight=SEC_HS_W$FPWT, na.rm=TRUE), digits = 3)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 1),
         na.rm=TRUE)

# PARENT HAS A COLLEGE DEGREE, homeschool only, nonwhite students

round(wpct(EL_HS_NW$ba_no_ba, weight=EL_HS_NW$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS_NW$ba_no_ba, weight=SEC_HS_NW$FPWT, na.rm=TRUE), digits = 3)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 2),
         na.rm=TRUE)

# -----

# IN/NEAR POVERTY, homeschool only, white students

round(wpct(EL_HS_W$poverty, weight=EL_HS_W$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS_W$poverty, weight=SEC_HS_W$FPWT, na.rm=TRUE), digits = 3)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 1),
         na.rm=TRUE)

# IN/NEAR POVERTY, homeschool only, nonwhite students

round(wpct(EL_HS_NW$poverty, weight=EL_HS_NW$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS_NW$poverty, weight=SEC_HS_NW$FPWT, na.rm=TRUE), digits = 3)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 2),
         na.rm=TRUE)

# -----

# HOMESCHOOL elementary school, in/near poverty by race
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 1))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 2))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 3))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 4))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 5))

# PUBLIC SCHOOL elementary school, in/near poverty by race
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 &
                               elementary_secondary == 1 & RACEETH == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                               elementary_secondary == 1 & RACEETH == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                               elementary_secondary == 1 & RACEETH == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                               elementary_secondary == 1 & RACEETH == 4))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                               elementary_secondary == 1 & RACEETH == 5))

# HOMESCHOOL secondary school, in/near poverty by race
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 1))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 2))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 3))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 4))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 5))

# PUBLIC SCHOOL secondary school, in/near poverty by race
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 &
                                 elementary_secondary == 2 & RACEETH == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                                 elementary_secondary == 2 & RACEETH == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                                 elementary_secondary == 2 & RACEETH == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                                 elementary_secondary == 2 & RACEETH == 4))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & 
                                 elementary_secondary == 2 & RACEETH == 5))

svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 1 & RACEETH == 1 & SCHTYPE == 1))
svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 2 & RACEETH == 1 & SCHTYPE == 1))

svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 1 & RACEETH == 1 & SCHTYPE == 2))
svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 2 & RACEETH == 1 & SCHTYPE == 2))

svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 1 & RACEETH == 1 & SCHTYPE == 4))
svymean(~(poverty < 3), subset(PFIdesign, 
                               elementary_secondary == 2 & RACEETH == 1 & SCHTYPE == 4))

svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1 & RACEETH == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2 & RACEETH == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 1 & RACEETH == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 2 & RACEETH == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 1 & RACEETH == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 2 & RACEETH == 1),
         na.rm=TRUE)

# PARENT EDUCATION LEVEL, WHITE CHILDREN, ALL EDUCATIONAL OPTIONS

svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 1 & RACEETH == 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 2 & RACEETH == 1))

svymean(~(ba_no_ba == 1), subset(PFIdesign, 
                                 elementary_secondary == 1 & RACEETH == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, 
                                 elementary_secondary == 2 & RACEETH == 1))

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & RACEETH == 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, RACEETH == 1),
         na.rm=TRUE)

# -----

# 3. POVERTY AND EDUCATION, NONWHITE CHILDREN ONLY

# ALL NONWHITE, COLLEGE DEGREE BY GRADE LEVEL

svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 1 & RACEETH > 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 2 & RACEETH > 1))

svymean(~(ba_no_ba == 1), subset(PFIdesign, 
                                 elementary_secondary == 1 & RACEETH > 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, 
                                 elementary_secondary == 2 & RACEETH > 1))

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & RACEETH > 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, RACEETH > 1),
         na.rm=TRUE)

# C. Elementary v. secondary, by race/ethnicity

# White homeschooled students
wpct(EL_HS_W$ba_no_ba, weight=EL_HS_W$FPWT, na.rm=TRUE) 
wpct(SEC_HS_W$ba_no_ba, weight=SEC_HS_W$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 1), na.rm=TRUE)

# Nonwhite homeschooled students
wpct(EL_HS_M$ba_no_ba, weight=EL_HS_M$FPWT, na.rm=TRUE) 
wpct(SEC_HS_M$ba_no_ba, weight=SEC_HS_M$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 2), na.rm=TRUE)

# C. Elementary v. secondary, by race/ethnicity

# White homeschooled students
wpct(EL_HS_W$poverty, weight=EL_HS_W$FPWT, na.rm=TRUE) 
wpct(SEC_HS_W$poverty, weight=SEC_HS_W$FPWT, na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 1), na.rm=TRUE)

# Nonwhite homeschooled students
wpct(EL_HS_M$poverty, weight=EL_HS_M$FPWT, na.rm=TRUE) 
wpct(SEC_HS_M$poverty, weight=SEC_HS_M$FPWT, na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, white_nonwhite == 2), na.rm=TRUE)

round(wpct(EL_HS_W$poverty, weight=EL_HS_W$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS_W$poverty, weight=SEC_HS_W$FPWT, na.rm=TRUE), digits = 3)

# -----

# POOR/NEAR-POOR, by race/ethnicity and educational option 

# HOMESCHOOL, PERCENT IN/NEAR POVERTY
white <- as.data.frame(svymean(~poverty < 3, subset(HOMEdesign, RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~poverty < 3, subset(HOMEdesign, RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~poverty < 3, subset(HOMEdesign, RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~poverty < 3, subset(HOMEdesign, RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~poverty < 3, subset(HOMEdesign, RACEETH == 5), na.rm=TRUE))
raceHo <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PUBLIC SCHOOL, PERCENT IN/NEAR POVERTY
white <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 5), na.rm=TRUE))
racePu <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PRIVATE SCHOOL, PERCENT IN/NEAR POVERTY
white <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 5), na.rm=TRUE))
racePr <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# VIRTUAL SCHOOL, PERCENT IN/NEAR POVERTY
white <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 5), na.rm=TRUE))
raceVi <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# create table using items created above
PovertyChart <- rbind(raceHo, racePu, racePr, raceVi)
PovertyChart <- PovertyChart*100
colnames(PovertyChart) <- c("White", "Black", "Hispanic", "Asian/PI", "Other/Mixed")
rownames(PovertyChart) <- c("Homeschool", "Public", "Private", "Virtual")
PovertyChart

# -----

# COLLEGE DEGREE, by race/ethnicity and educational option 

# HOMESCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 5), na.rm=TRUE))
raceHo <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PUBLIC SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 5), na.rm=TRUE))
racePu <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PRIVATE SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 5), na.rm=TRUE))
racePr <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# VIRTUAL SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 1), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 3), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 4), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 5), na.rm=TRUE))
raceVi <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# create table using items created above
CollegeChart <- rbind(raceHo, racePu, racePr, raceVi)
CollegeChart <- CollegeChart*100
colnames(CollegeChart) <- c("White", "Black", "Hispanic", "Asian/PI", "Other/Mixed")
rownames(CollegeChart) <- c("Homeschool", "Public", "Private", "Virtual")
CollegeChart

# -----

# COLLEGE DEGREE, by race/ethnicity and educational option, GRADES 7-12 ONLY

# HOMESCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 1 & elementary_secondary == 2), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 2 & elementary_secondary == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 3 & elementary_secondary == 2), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 4 & elementary_secondary == 2), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(HOMEdesign, RACEETH == 5 & elementary_secondary == 2), na.rm=TRUE))
raceHo <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PUBLIC SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1 & elementary_secondary == 2), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2 & elementary_secondary == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3 & elementary_secondary == 2), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 4 & elementary_secondary == 2), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & RACEETH == 5 & elementary_secondary == 2), na.rm=TRUE))
racePu <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# PRIVATE SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1 & elementary_secondary == 2), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2 & elementary_secondary == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3 & elementary_secondary == 2), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 4 & elementary_secondary == 2), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 2 & RACEETH == 5 & elementary_secondary == 2), na.rm=TRUE))
racePr <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# VIRTUAL SCHOOL, PERCENT WITH A COLLEGE DEGREE
white <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 1 & elementary_secondary == 2), na.rm=TRUE))
black <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 2 & elementary_secondary == 2), na.rm=TRUE))
hispan <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 3 & elementary_secondary == 2), na.rm=TRUE))
asian <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 4 & elementary_secondary == 2), na.rm=TRUE))
other <- as.data.frame(svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 4 & RACEETH == 5 & elementary_secondary == 2), na.rm=TRUE))
raceVi <- round(c(white[2,1], black[2,1], hispan[2,1], asian[2,1], other[2,1]), digits = 3)

# create table using items created above
CollegeChartSec <- rbind(raceHo, racePu, racePr, raceVi)
CollegeChartSec <- CollegeChartSec*100
colnames(CollegeChartSec) <- c("White", "Black", "Hispanic", "Asian/PI", "Other/Mixed")
rownames(CollegeChartSec) <- c("Homeschool", "Public", "Private", "Virtual")
CollegeChartSec

# end script