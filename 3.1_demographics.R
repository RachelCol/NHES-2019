# DEMOGRAPHICS
# This script looks at race, education, poverty, family structure, and more,
# comparing homeschool, public school, private school, and virtual school

# The statistics generated here did not ultimately make it into my paper;
# instead, I created an SES variable and made that the focus for the paper.

# note: This script is designed to run after 0_data_subsets script.

PUBLIC <- subset(PFI, SCHTYPE == 1)
PRIVATE <- subset(PFI, SCHTYPE == 2)
VIRTUAL <- subset(PFI, SCHTYPE == 4)
# note: "HOME" already exists, no need to create it

# ...

# Child's Race

round(wpct(HOME$RACEETH, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$RACEETH, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$RACEETH, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$RACEETH, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((RACEETH == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 4) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 5) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 4) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 5) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 4) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((RACEETH == 5) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# ... 

# Parents' highest level of education

round(wpct(HOME$PARGRADEX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$PARGRADEX, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$PARGRADEX, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$PARGRADEX, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((PARGRADEX == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 4) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 5) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 4) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 5) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 4) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PARGRADEX == 5) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# ...

# Household Structure

# Two-parents or single parent

round(wpct(HOME$HHPARN19_BRD, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$HHPARN19_BRD, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$HHPARN19_BRD, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$HHPARN19_BRD, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((HHPARN19_BRD == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# -----

# Number of siblings

round(wpct(HOME$NUMSIBSX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$NUMSIBSX, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$NUMSIBSX, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$NUMSIBSX, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$NUMSIBSX > 3, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$NUMSIBSX > 3, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$NUMSIBSX > 3, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$NUMSIBSX > 3, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((NUMSIBSX == 0) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX > 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 0) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX > 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 0) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((NUMSIBSX > 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# ...

# Family Income & Poverty Level

round(wpct(HOME$food_stamps, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$food_stamps, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$food_stamps, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$food_stamps, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$wic, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$wic, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$wic, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$wic, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$tanf, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$tanf, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$tanf, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$tanf, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$medicaid, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$medicaid, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$medicaid, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$medicaid, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$chip, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$chip, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$chip, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$chip, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$sec8, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$sec8, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$sec8, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$sec8, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HOME$welfare, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$welfare, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$welfare, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$welfare, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

# Family poverty status 
# note: This variable is created in 0_data_subsets, using income and household size.
round(wpct(HOME$poverty, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$poverty, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$poverty, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$poverty, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((poverty == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((poverty == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((PFIdesign == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# ...

# Household Employment

# Two-parent families, employment, home v. public
# (1) both work full time; 
# (2) both work, some part-time; 
# (3) one works while the other does not; 
# (4) both not employed

round(wpct(HOME$two_parent_work, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$two_parent_work, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$two_parent_work, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$two_parent_work, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((two_parent_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 4) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 4) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((two_parent_work == 4) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# Single-parent families, employment, home v. public
# (1) full time work; 
# (2) part-time work; 
# (3) not employed

round(wpct(HOME$one_parent_work, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$one_parent_work, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$one_parent_work, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$one_parent_work, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((one_parent_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# Households where at least one parent is self-employed (1 = yes, 2 = no)

round(wpct(HOME$self_employed, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$self_employed, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$self_employed, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$self_employed, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((self_employed == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((self_employed == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((self_employed == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# Households with a stay at home parent (1 = yes, 2 = no)

round(wpct(HOME$sahp, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PUBLIC$sahp, weight=PUBLIC$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PRIVATE$sahp, weight=PRIVATE$FPWT, na.rm=TRUE), digits = 3)
round(wpct(VIRTUAL$sahp, weight=VIRTUAL$FPWT, na.rm=TRUE), digits = 3)

svyttest((sahp == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((sahp == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((sahp == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# Households by women's involvement in the workforce
# 1 = full-time;
# 2 = part-time;
# 3 = not employed

round(wpct(HOME$women_work, weight=HOME$FPWT, na.rm=TRUE), digits=3)
round(wpct(PUBLIC$women_work, weight=PUBLIC$FPWT, na.rm=TRUE), digits=3)
round(wpct(PRIVATE$women_work, weight=PRIVATE$FPWT, na.rm=TRUE), digits=3)
round(wpct(VIRTUAL$women_work, weight=VIRTUAL$FPWT, na.rm=TRUE), digits=3)

svyttest((women_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# Households by men's involvement in the workforce
# 1 = full-time;
# 2 = part-time; 
# 3 = not employed

round(wpct(HOME$men_work, weight=HOME$FPWT, na.rm=TRUE), digits=3)
round(wpct(PUBLIC$men_work, weight=PUBLIC$FPWT, na.rm=TRUE), digits=3)
round(wpct(PRIVATE$men_work, weight=PRIVATE$FPWT, na.rm=TRUE), digits=3)
round(wpct(VIRTUAL$men_work, weight=VIRTUAL$FPWT, na.rm=TRUE), digits=3)

svyttest((men_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 1) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 2) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 3) ~ home_private, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 1) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 2) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 3) ~ home_virtual, 
         PFIdesign,
         na.rm=TRUE)

# END overall demographic analysis