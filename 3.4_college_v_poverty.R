# POVERTY v. EDUCATION

# Question: Do homeschooling families have lower incomes relative to their education?
# Answer: Yes

# note: This script is designed to run after 0_data_subsets script.

# Create tables with poverty status and parent education levels, number in each
HOMEtable <- svytable(~poverty+PARGRADEX, HOMEdesign)
PFItable <- svytable(~poverty+PARGRADEX, PFIdesign)
# Of each education level, what percent are in poverty, near poverty, or not?
# note: The percents sum vertically in each table
PFIpercent <- cbind(round(PFItable[, 1] / sum(PFItable[, 1]), digits = 3), 
                    round(PFItable[, 2] / sum(PFItable[, 2]), digits = 3), 
                    round(PFItable[, 3] / sum(PFItable[, 3]), digits = 3), 
                    round(PFItable[, 4] / sum(PFItable[, 4]), digits = 3), 
                    round(PFItable[, 5] / sum(PFItable[, 5]), digits = 3))
HOMEpercent <- cbind(round(HOMEtable[, 1] / sum(HOMEtable[, 1]), digits = 3), 
                     round(HOMEtable[, 2] / sum(HOMEtable[, 2]), digits = 3), 
                     round(HOMEtable[, 3] / sum(HOMEtable[, 3]), digits = 3), 
                     round(HOMEtable[, 4] / sum(HOMEtable[, 4]), digits = 3), 
                     round(HOMEtable[, 5] / sum(HOMEtable[, 5]), digits = 3))
# Subtract the overall PFI table from the homeschool table. Now, each indvidual 
# entry is negative if there is a higher level of poverty, relative to parental
# education, for families that homeschool.
HOMEpercent - PFIpercent 
# Finding: Homeschooling families with college-educated parents are less likely
# than other families with college-educated parents to not be in or near poverty.

# -----

# COLLEGE DEGREE v. POVERTY

# What percent of parents are in or near poverty overall, 
# homeschool v. public school?
svymean(~poverty < 3, HOMEdesign)
svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1))

# What percent of parents with a college degree are in or near poverty, 
# homeschool v. public school?
svymean(~poverty < 3, subset(HOMEdesign, ba_no_ba == 1))
svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & ba_no_ba == 1))
svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1),
         na.rm=TRUE)

# What percent of parents who do NOT have a college degree are in or near 
# poverty, homeschool v. public school?
svymean(~poverty < 3, subset(HOMEdesign, ba_no_ba != 1))
svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & ba_no_ba != 1))

# What percent of parents with a college degree are in or near poverty, 
# homeschool v. public school, GRADES K-6
svymean(~poverty < 3, subset(HOMEdesign, ba_no_ba == 1 & elementary_secondary == 1))
svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & ba_no_ba == 1 & elementary_secondary == 1))
svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1 & elementary_secondary == 1),
         na.rm=TRUE)

# What percent of parents with a college degree are in or near poverty, 
# homeschool v. public school, GRADES 7-12
svymean(~poverty < 3, subset(HOMEdesign, ba_no_ba == 1 & elementary_secondary == 2))
svymean(~poverty < 3, subset(PFIdesign, SCHTYPE == 1 & ba_no_ba == 1 & elementary_secondary == 2))
svyttest((poverty < 3) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1 & elementary_secondary == 1),
         na.rm=TRUE)

# Reverse the question: 

# What percent of parents in or near poverty have a college degree,
# homeschool v. public school?
svymean(~ba_no_ba == 1, subset(HOMEdesign, poverty < 3))
svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1 & poverty < 3))
svyttest((ba_no_ba == 1) ~ home_public, 
         subset(PFIdesign, poverty < 3),
         na.rm=TRUE)

# ----- 

# IN OR NEAR POVERTY, by parents' education level
# homeschool v. public school v. private school v. virtual school

svymean(~(poverty < 3), subset(PFIdesign, PARGRADEX == 1)) # combined
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & PARGRADEX == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & PARGRADEX == 1))

svymean(~(poverty < 3), subset(PFIdesign, PARGRADEX == 2)) # combined
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & PARGRADEX == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & PARGRADEX == 2))

svymean(~(poverty < 3), subset(PFIdesign, PARGRADEX == 3)) # combined
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & PARGRADEX == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & PARGRADEX == 3))

svymean(~(poverty < 3), subset(PFIdesign, PARGRADEX == 4)) # combined
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & PARGRADEX == 4))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 4))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & PARGRADEX == 4))

svymean(~(poverty < 3), subset(PFIdesign, PARGRADEX == 5)) # combined
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 3 & PARGRADEX == 5))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 5))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & PARGRADEX == 5))

# END SECTON ON EDUCATION V. POVERTY LEVEL