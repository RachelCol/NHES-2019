# Student's race, school level, college, and SES
# note: This racial breakdown did not make it into the paper

# note: This script is designed to run after 0_data_subsets script.

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

# end script