# DEMOGRAPHICS: SES 

# note: This script is designed to run after 0_data_subsets script.

# TABLE OF CONTENTS
# -- 1. HOMESCHOOL v. PUBLIC SCHOOL: 
#        -- overall; 
#        -- K-6; 
#        -- 7-12; 
#        -- kindergarten
# -- 2. HOMESCHOOL grade level comparisons:
#        -- K-6 v. 7-12
#        -- K-5 v. 6-8 v. 9-12
# -- 3. HOMESCHOOL legnth comparisons:
#        -- always v. first year transf v. some years transf

# -----

# 1. HOMESCHOOL v. PUBLIC SCHOOL

# OVERALL, homeschool v. public school
part <- subset(PFI, SCHTYPE == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, SCHTYPE == 3)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)
svyttest((SES == 2) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)
svyttest((SES == 3) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# GRADES K-6, homeschool v. public school
part <- subset(PFI, SCHTYPE == 1 & elementary_secondary==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, SCHTYPE == 3 & elementary_secondary==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary==1),
         na.rm=TRUE)
svyttest((SES == 2) ~ home_public, 
         subset(PFIdesign, elementary_secondary==1),
         na.rm=TRUE)
svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary==1),
         na.rm=TRUE)

# GRADES 7-12, homeschool v. public school
part <- subset(PFI, SCHTYPE == 1 & elementary_secondary==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, SCHTYPE == 3 & elementary_secondary==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 1) ~ home_public, 
         subset(PFIdesign, elementary_secondary==2),
         na.rm=TRUE)
svyttest((SES == 2) ~ home_public, 
         subset(PFIdesign, elementary_secondary==2),
         na.rm=TRUE)
svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary==2),
         na.rm=TRUE)

# KINDERGARTEN, homeschool v. public school
KGT <- subset(HOME, FIRST == 1 & ALLGRADEX == 0)
round(wpct(KGT$SES, weight=KGT$FPWT, na.rm= TRUE), digits = 3)
PSK <- subset(PFI, ALLGRADEX == 0 & SCHTYPE == 1)
round(wpct(PSK$SES, weight=PSK$FPWT, na.rm= TRUE), digits = 3)

svyttest(SES == 1 ~ home_public, 
         subset(PFIdesign, ALLGRADEX == 0),
         na.rm=TRUE)
svyttest(SES == 2 ~ home_public, 
         subset(PFIdesign, ALLGRADEX == 0),
         na.rm=TRUE)
svyttest(SES == 3 ~ home_public, 
         subset(PFIdesign, ALLGRADEX == 0),
         na.rm=TRUE)

# -----

# 2. HOMESCHOOL COMPARISONS BY SCHOOL LEVEL

# HOMESCHOOL, grades K-6 v. 7-12
part <- subset(PFI, SCHTYPE == 3 & elementary_secondary==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, SCHTYPE == 3 & elementary_secondary==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((SES == 2) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((SES == 3) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# HOMESCHOOL K-5, 6-8, 9-12
part <- subset(HOME, SCHLEVEL==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SCHLEVEL==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SCHLEVEL==3)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# checking coefficient of variation for each calculation:
cv(svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 1)))
cv(svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 1)))
cv(svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 1)))
cv(svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 2)))
cv(svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 2)))
cv(svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 2)))
cv(svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 3)))
cv(svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 3)))
cv(svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 3)))

# by way of comparison, SES of public school students by school level:
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==3)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# -----

# 3. ALWAYS v. FIRST YEAR TRANSFER v. SOME YEARS TRANSFERS

part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first-year transfers only
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS == 1) # always homeschoolers includes kindergarten
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS == 0 & FIRST == 0) # transfers not in their first year
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

HOMEdesign <- update(HOMEdesign, # note: first = first year transfer
                     firstT_always = ifelse(FIRST == 1 & ALLGRADEX > 0, "first", 
                                           ifelse(ALWAYS == 1, "always", NA)))

svyttest((SES == 1) ~ firstT_always, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((SES == 2) ~ firstT_always, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((SES == 3) ~ firstT_always, 
         HOMEdesign,
         na.rm=TRUE)

# END SES exploration script