# DEMOGRAPHICS: SES EXPLORATION

# note: This script is designed to run after 0_data_subsets script.

# Create subsets to enable comparison of homeschooled elementary and secondary
EL_HS <- subset(HOME, elementary_secondary == 1)
SEC_HS <- subset(HOME, elementary_secondary == 2)

# -----

# Compare socio-economic status, homeschool v. public school

round(wpct(PFI$SES, weight=PFI$FPWT, na.rm=TRUE), digits = 3)

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

# -----

# Compare socio-economic status, homeschool only, grades K-6 v. 7-12

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

# -----

# Compare socio-economic status, homeschool v. public school GRADES K-6

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

# -----

# Compare socio-economic status, homeschool v. public school GRADES 7-12

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

# -----

# Compare socio-economic status, homeschool v. public school K-5, 6-8, 9-12

part <- subset(HOME, SCHLEVEL==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SCHLEVEL==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SCHLEVEL==3)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# checking coefficient of variation for each calculation:
svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 1))
svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 1))
svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 1))
svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 2))
svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 2))
svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 2))
svymean(~SES==1, subset(HOMEdesign, SCHLEVEL == 3))
svymean(~SES==2, subset(HOMEdesign, SCHLEVEL == 3))
svymean(~SES==3, subset(HOMEdesign, SCHLEVEL == 3))

# by way of comparison, SES of public school students by school level:
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, SCHTYPE == 3 & SCHLEVEL==3)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# -----

# Compare socio-economic status, homeschool only, always v. first v. some years
# i.e., this is a comparison by how long families have been homeschooling
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first-year transfers only
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS == 1) # always homeschoolers includes kindergarten
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, ALWAYS == 0 & FIRST == 0) # transfers not in their first year
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

HOMEdesign <- update(HOMEdesign,  
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

# -----

# SES by whether a family is homeschooling due to a disability
part <- subset(HOME, disability == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether a family has religious reasons for homeschooling
part <- subset(HOME, partRELGON == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, partRELGON != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether families are homeschooling due to academics
part <- subset(HOME, partDISSATX == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, partDISSATX != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether a family is homeschooling due to the school environment
part <- subset(HOME, partSAFETYX == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, partSAFETYX != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether the family has two parents or a single parent
part <- subset(HOME, two_parent_or_single == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, two_parent_or_single == 2 & SCHTYPE == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# END SES exploration script