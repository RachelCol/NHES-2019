# DEMOGRAPHICS, ELEMENTARY v. SECONDARY
# This scripts makes comparisons within homeschooling only.

# note: This script is designed to run after 0_data_subsets script.

# note: This script complements 3.1_demographics. This script covers
# the same demographic categories, but makes comparisons within
# homeschooling between grades K-6 and grades 7-12.

# note: This script does not address the SES variable.

# TABLE OF CONTENTS: 
# -- 1. CHILD'S RACE/ETHNICITY
#       -- A. elementary v. secondary, all school types
#       -- B. homeschool status & elementary v. secondary
# -- 2. POOR OR NEAR-POOR
#       -- A. elementary v. secondary, all school types
#       -- B. homeschool status & elementary v. secondary
# -- 3. PARENT HAS A COLLEGE DEGREE
#       -- A. elementary v. secondary, all school types
#       -- B. homeschool status & elementary v. secondary
# -- 4. HOUSEHOLD STRUCTURE

# TO CHANGE
# -- remove SES, that's in the SES document
# -- label everything consistently
# -- use the same order as the general demographics document

# ...

# 1. CHILD'S RACE/ETHNICITY

# A. Elementary v. secondary, all school types
wpct(EL_HS$white_nonwhite, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$white_nonwhite, weight=SEC_HS$FPWT, na.rm=TRUE)

svymean(~(RACEETH == 1), subset(HOMEdesign, elementary_secondary == 1))
svymean(~(RACEETH == 1), subset(HOMEdesign, elementary_secondary == 2))

svymean(~(RACEETH == 2), subset(HOMEdesign, elementary_secondary == 1))
svymean(~(RACEETH == 2), subset(HOMEdesign, elementary_secondary == 2))

svymean(~(RACEETH == 3), subset(HOMEdesign, elementary_secondary == 1))
svymean(~(RACEETH == 3), subset(HOMEdesign, elementary_secondary == 2))

svymean(~(RACEETH == 4), subset(HOMEdesign, elementary_secondary == 1))
svymean(~(RACEETH == 4), subset(HOMEdesign, elementary_secondary == 2))

svymean(~(RACEETH == 5), subset(HOMEdesign, elementary_secondary == 1))
svymean(~(RACEETH == 5), subset(HOMEdesign, elementary_secondary == 2))

svyttest((white_nonwhite == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# B. Elementary v. secondary, always homeschooling v. homeschool transfer 
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
         HOMEdesign, na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 1),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, ALWAYS == 0 & FIRST == 0),
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1),
         na.rm=TRUE)

# -----

# 2. POOR OR NEAR POOR

# Elementary v. secondary, all school options
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

# Elementary v. secondary, always homeschooling v. homeschool transfer
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

# -----

# 3. PARENT HAS A BACHELOR'S DEGREE

# A. Elementary v. secondary, all school types

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

# B. Elementary v. secondary, always homeschooling v. homeschool transfer

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

# -----

# 4. Household structure, homeschool elementary v. secondary

wpct(EL_HS$two_parent_or_single, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$two_parent_or_single, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((two_parent_or_single == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# Households with a SAHP, homeschool elementary v. secondary

wpct(EL_HS$sahp, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$sahp, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((sahp == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# END COMPARISON BY SCHOOL LEVEL