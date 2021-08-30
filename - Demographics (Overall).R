# DEMOGRAPHICS OVERALL

# Homeschooled children compared with public school children, grades 1-12

# Create data subsets for weighted percents
HOME <- subset(PFI, SCHTYPE == 3)
PUBLIC <- subset(PFI, SCHTYPE == 1)

# ...

# Child's Race

wpct(HOME$RACEETH, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$RACEETH, weight=PUBLIC$FPWT, na.rm=TRUE)

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

# ... 

# Parents' highest level of education

wpct(HOME$PARGRADEX, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$PARGRADEX, weight=PUBLIC$FPWT, na.rm=TRUE)

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

# ...

# Household Structure

# Two-parents or single parent

wpct(HOME$HHPARN19_BRD, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$HHPARN19_BRD, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((HHPARN19_BRD == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((HHPARN19_BRD == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Number of siblings

wpct(HOME$NUMSIBSX, weight=HOME$FPWT, na.rm=TRUE)
wpct(HOME$NUMSIBSX > 3, weight=HOME$FPWT, na.rm=TRUE)

wpct(PUBLIC$NUMSIBSX, weight=PUBLIC$FPWT, na.rm=TRUE)
wpct(PUBLIC$NUMSIBSX > 3, weight=PUBLIC$FPWT, na.rm=TRUE)

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

# ...

# Family Income 

wpct(HOME$income, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$income, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((income == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((income == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((income == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((income == 4) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# ...

# Household Employment

# Two-parent families, employment, home v. public
# (1) both work full time; (2) both work, some part-time; (3) one works; (4) both not employed

wpct(HOME$two_parent_work, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$two_parent_work, weight=PUBLIC$FPWT, na.rm=TRUE)

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

# Single-parent families, employment, home v. public
# (1) full time work; (2) part-time work; (3) not employed

wpct(HOME$one_parent_work, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$one_parent_work, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((one_parent_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((one_parent_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Households where at least one parent has part-time work (1 = yes, 2 = no)

wpct(HOME$part_time, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$part_time, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((part_time == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Households where at least one parent is self-employed (1 = yes, 2 = no)

wpct(HOME$self_employed, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$self_employed, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((self_employed == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Households with a stay at home parent (1 = yes, 2 = no)

wpct(HOME$sahp, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$sahp, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((sahp == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Households by women's involvement in the workforce
# (1 = full-time, 2 = part-time, 3 = not employed)

wpct(HOME$women_work, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$women_work, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((women_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((women_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# Households by men's involvement in the workforce
# (1 = full-time, 2 = part-time, 3 = not employed)

wpct(HOME$men_work, weight=HOME$FPWT, na.rm=TRUE)
wpct(PUBLIC$men_work, weight=PUBLIC$FPWT, na.rm=TRUE)

svyttest((men_work == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

svyttest((men_work == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# END overall demographic analysis