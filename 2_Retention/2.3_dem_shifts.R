# DEMOGRAPHIC SHIFTS related to the retention rate

# note: This script looks at shifts in FIRST YEAR TRANSFERS only

# note: This script is designed to run after 0_data_subsets script.

# CREATE comparison object for comparing first-year homeschool transfers
# with children attending public school.
COMBINEDdesign <- update(COMBINEDdesign,  homeFirst_public = 
                           ifelse(SCHTYPE == 3 & FIRST == 1 & ALWAYS != 1, "homeFirst", 
                                  ifelse(SCHTYPE == 1, "public", NA)))
# note: This comaprison object uses the combined data set COMBINEDdesign because 
# the variable "FIRST" does not exist in the PFI data set.

# -----

# FIRST YEAR HOMESCHOOL TRANSFER v. PUBLIC SCHOOL

# Question: how similar are children who transfer to homeschooling from a 
# school to children in public school who remain in public school?

# OVERALL, first-year homeschool transfer v. public school
svymean(~(SES == 1), subset(HOMEdesign, FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1))

svymean(~(SES == 2), subset(HOMEdesign, FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1))

svymean(~(SES == 3), subset(HOMEdesign, FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1))

svyttest((SES == 1) ~ homeFirst_public, 
         COMBINEDdesign,
         na.rm=TRUE)
svyttest((SES == 2) ~ homeFirst_public, 
         COMBINEDdesign,
         na.rm=TRUE)
svyttest((SES == 3) ~ homeFirst_public, 
         COMBINEDdesign,
         na.rm=TRUE)

# GRADES K-6, first-year homeschool transfer v. public school
svymean(~(SES == 1), subset(HOMEdesign, elementary_secondary == 1 & FIRST == 1 & ALLGRADEX != 0))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1 & ALLGRADEX != 0))

svymean(~(SES == 2), subset(HOMEdesign, elementary_secondary == 1 & FIRST == 1 & ALLGRADEX != 0))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1 & ALLGRADEX != 0))

svymean(~(SES == 3), subset(HOMEdesign, elementary_secondary == 1 & FIRST == 1 & ALLGRADEX != 0))
svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1 & ALLGRADEX != 0))

# Use comparison object created above 
svyttest((SES == 1) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 1),
         na.rm=TRUE)
svyttest((SES == 2) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 1),
         na.rm=TRUE)
svyttest((SES == 3) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 1),
         na.rm=TRUE)

# GRADES 7-12, first-year homeschool transfer v. public school
svymean(~(SES == 1), subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

svymean(~(SES == 2), subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

svymean(~(SES == 3), subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1 & ALWAYS != 1))
svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))

# Use comparison object created above
svyttest((SES == 1) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 2),
         na.rm=TRUE)
svyttest((SES == 2) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 2),
         na.rm=TRUE)
svyttest((SES == 3) ~ homeFirst_public, 
         subset(COMBINEDdesign, elementary_secondary == 2),
         na.rm=TRUE)

# -----

# HOMESCHOOL ONLY, first-year homsechool transfers
# COMPARE FIRST YEAR ELEMENTARY AND FIRST YEAR SECONDARY STUDENTS

ELM <- subset(HOME, FIRST == 1 & elementary_secondary == 1 & ALLGRADEX > 0)
SEC <- subset(HOME, FIRST == 1 & elementary_secondary == 2)

# Low SES or no
round(wpct(ELM$SES, weight=ELM$FPWT, na.rm= TRUE), digits = 3)
round(wpct(SEC$SES, weight=SEC$FPWT, na.rm= TRUE), digits = 3)

svyttest(SES == 1 ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1 & ALLGRADEX != 0),
         na.rm=TRUE)
svyttest(SES == 2 ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1 & ALLGRADEX != 0),
         na.rm=TRUE)
svyttest(SES == 3 ~ elementary_secondary, 
         subset(HOMEdesign, FIRST == 1 & ALLGRADEX != 0),
         na.rm=TRUE)

# END SCRIPT