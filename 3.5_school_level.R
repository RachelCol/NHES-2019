# DEMOGRAPHICS, ELEMENTARY v. SECONDARY
# This scripts makes comparisons within homeschooling.

# note: This script is designed to run after 0_data_subsets script.

# note: This script complements 3.1_demographics. This script covers
# the same demographic categories, but makes comparisons within
# homeschooling between grades K-6 and grades 7-12.

# note: This script does not address the SES variable.


# TO CHANGE
# -- remove SES, that's in the SES document
# -- label everything consistently
# -- use the same order as the general demographics document

# ...

# Child's race, homeschool elementary v. secondary

wpct(EL_HS$white_nonwhite, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$white_nonwhite, weight=SEC_HS$FPWT, na.rm=TRUE)

svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(RACEETH == 2), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 2), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(RACEETH == 3), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 3), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(RACEETH == 4), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 4), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(RACEETH == 5), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 5), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svyttest((white_nonwhite == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((white_nonwhite == 2) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# CHILD'S RACE, ELEMENTARY v SECONDARY, ALL SCHOOL OPTIONS
# NOT A LOT OF STATISTICAL SIGNIFICANCE, BUT STILL IMPORTANT
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))

svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))

svyttest((RACEETH == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

# CHILD'S RACE, BY HOMESCHOOL STATUS
# THIS SECTION IS IN THE APPENDICES
svymean(~(RACEETH == 1), subset(HOMEdesign, 
                                elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(HOMEdesign, 
                                elementary_secondary == 2))

svymean(~(RACEETH == 1), subset(HOMEdesign, ALWAYS == 1 & 
                                  elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                  elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(HOMEdesign, FIRST == 1 & 
                                  elementary_secondary == 1))

svymean(~(RACEETH == 1), subset(HOMEdesign, ALWAYS == 1 & 
                                  elementary_secondary == 2))
svymean(~(RACEETH == 1), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                  elementary_secondary == 2))
svymean(~(RACEETH == 1), subset(HOMEdesign, FIRST == 1 & 
                                  elementary_secondary == 2))

svyttest((RACEETH == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 0 & FIRST == 0),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1),
         na.rm=TRUE)

# POVERTY STATUS, WHITE CHILDREN, ALL EDUCATIONAL OPTIONS
# IN APPENDICES ONLY

svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1 & RACEETH == 1))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2 & RACEETH == 1))

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
# THIS IS USED IN THE PAPER

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

# POOR OR NEAR POOR, ELEMENTARY v SECONDARY, ALL SCHOOL OPTIONS

svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))

svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))

svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

# POOR OR NEAR POOR, BY HOMESCHOOL STATUS
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 1))
svymean(~(poverty < 3), subset(HOMEdesign, 
                               elementary_secondary == 2))

svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & 
                                 elementary_secondary == 1))
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                 elementary_secondary == 1))
svymean(~(poverty < 3), subset(HOMEdesign, FIRST == 1 & 
                                 elementary_secondary == 1))

svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & 
                                 elementary_secondary == 2))
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                 elementary_secondary == 2))
svymean(~(poverty < 3), subset(HOMEdesign, FIRST == 1 & 
                                 elementary_secondary == 2))

svyttest((poverty < 3) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 0 & FIRST == 0),
         na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1),
         na.rm=TRUE)

svyttest((poverty < 3) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# ... 

# Parent has a bachelor's degree, elementary v. secondary

svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))

svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))


svyttest((ba_no_ba == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ home_private, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ home_virtual, 
         subset(PFIdesign, elementary_secondary == 2),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# DEGREE OR NO DEGREE, BY HOMESCHOOL STATUS

svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, 
                                 elementary_secondary == 2))

svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & 
                                   elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                   elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, FIRST == 1 & 
                                   elementary_secondary == 1))

svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & 
                                   elementary_secondary == 2))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 0 & FIRST == 0 & 
                                   elementary_secondary == 2))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, FIRST == 1 & 
                                   elementary_secondary == 2))

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 0 & FIRST == 0),
         na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1),
         na.rm=TRUE)

# Parent has a bachelor's degree, white homeschooled students

EL_HS_W <- subset(EL_HS, white_nonwhite == 1) # create a white subset
SEC_HS_W <- subset(SEC_HS, white_nonwhite == 1) # create a white subset

wpct(EL_HS_W$ba_no_ba, weight=EL_HS_W$FPWT, na.rm=TRUE) 
wpct(SEC_HS_W$ba_no_ba, weight=SEC_HS_W$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 1),
         na.rm=TRUE)

# Parent has a bachelor's degree, minority homeschooled students

EL_HS_M <- subset(EL_HS, white_nonwhite == 2) # create a minority subset
SEC_HS_M <- subset(SEC_HS, white_nonwhite == 2) # create a minority subset

wpct(EL_HS_M$ba_no_ba, weight=EL_HS_M$FPWT, na.rm=TRUE) 
wpct(SEC_HS_M$ba_no_ba, weight=SEC_HS_M$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 2),
         na.rm=TRUE)

# ... 

# Household structure, homeschool elementary v. secondary

wpct(EL_HS$two_parent_or_single, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$two_parent_or_single, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((two_parent_or_single == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ... 

# Households with a SAHP, homeschool elementary v. secondary

wpct(EL_HS$sahp, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$sahp, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((sahp == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ... 

# Household Income
# NO SIGNIFICANT DIFFERENCE

wpct(EL_HS$poverty, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$poverty, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((poverty == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 2) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 3) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 4) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# DEMOGRAPHIC DIFFERENCES THAT ARE STATISTICALLY SIGNIFICANT
# ELEMENTARY V. SECONDARY

svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 3 & elementary_secondary == 2))

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# bachelor's degree
svymean(~(ba_no_ba == 1), subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))
# create comparison variable, run t-test; need to use COMBINEDdesign because
# the variable "FIRST" does not exist in the PFI data set.
COMBINEDdesign <- update(COMBINEDdesign,  homeFirst_public = 
                           ifelse(SCHTYPE == 3 & FIRST == 1, "homeFirst", 
                                  ifelse(SCHTYPE == 1, "public", NA)))
svyttest((ba_no_ba == 1) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 2),
         na.rm=TRUE)


# END COMPARISON BY SCHOOL LEVEL