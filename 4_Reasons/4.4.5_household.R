# Reasons for Homeschooling
# by household structure (two parents v. one parent)

# note: At the end there is analysis of low-income families without a 
# college degree homeschooling children in grades 7-12.

# note: This script is designed to run after 0_data_subsets script.

# Create subsets to enable comparison 
HS_TW <- subset(HOME, two_parent_or_single == 1)
HS_SI <- subset(HOME, two_parent_or_single == 2)

# -----

# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?

# All homeschooled students
wpct(HOME$HSSAFETYX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSSAFETYX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSSAFETYX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?

# All homeschooled students
wpct(HOME$HSDISSATX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSDISSATX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSDISSATX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?



# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?

# All homeschooled students
wpct(HOME$HSMORAL, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students

wpct(HS_TW$HSMORAL, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSMORAL, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# DISABILITIES COMBINED

# All homeschooled students
wpct(HOME$disability, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$disability, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$disability, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?

# All homeschooled students
wpct(HOME$HSDISABLX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSDISABLX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSDISABLX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?

# All homeschooled students
wpct(HOME$HSILLX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSILLX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSILLX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 

# All homeschooled students
wpct(HOME$HSSPCLNDX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSSPCLNDX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSSPCLNDX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?

# All homeschooled students
wpct(HOME$HSALTX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSALTX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSALTX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?

# All homeschooled students
wpct(HOME$HSFMLY, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSFMLY, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSFMLY, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 

# All homeschooled students
wpct(HOME$HSOTHERX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSOTHERX, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSOTHERX, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 

# All homeschooled students
wpct(HOME$HSBULLY, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
wpct(HS_TW$HSBULLY, weight=HS_TW$FPWT, na.rm=TRUE)
wpct(HS_SI$HSBULLY, weight=HS_SI$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# -----

# Most important reason: HSMOSTX

# All homeschooled students
wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE)

# Two parents v. single parent homeschooled students
round(wpct(HS_TW$HSMOSTX, weight=HS_TW$FPWT, na.rm=TRUE), digits=3)
round(wpct(HS_SI$HSMOSTX, weight=HS_SI$FPWT, na.rm=TRUE), digits=3)

svyttest((HSMOSTX == 1) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 10 | HSMOSTX == 11) ~ two_parent_or_single, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# -----

# Why do parents without a college degree and living below the poverty line 
# choose to homeschool children in grades 7-12?

# What percent of low-income non-college-educated families homeschooling 
# children in grades 7-12 are single-parent families?
svymean(~two_parent_or_single == 2, subset(HOMEdesign, poverty == 1 & 
                                             elementary_secondary == 2 & ba_no_ba == 2))
# What percent of low-income non-college-educated families with children in 
# public school in grades 7-12 are single-parent families?
svymean(~two_parent_or_single == 2, subset(PFIdesign, SCHTYPE == 1 & poverty == 1 & 
                                             elementary_secondary == 2 & ba_no_ba == 2))
# What percent of OTHER homeschooling families are single-parent families?
svymean(~two_parent_or_single == 2, subset(HOMEdesign, poverty != 1 | 
                                             elementary_secondary != 2 | ba_no_ba != 2))
# What percent of OTHER public school families are single-parent families?
svymean(~two_parent_or_single == 2, subset(PFIdesign, SCHTYPE == 1 & poverty != 1 | 
                                             elementary_secondary != 2 | ba_no_ba != 2))

# Create design object
HOMEdesign <- update(HOMEdesign,  desp_else = ifelse(poverty == 1 & 
                                                       elementary_secondary == 2 & 
                                                       ba_no_ba == 2, "desp", "else"))

# Percent that have academic motivations
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & 
                                elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 1 | 
                                elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)

svyttest((HSDISSATX==1) ~ desp_else, 
         HOMEdesign,
         na.rm=TRUE)

# What ARE these parents' motivations?
svymean(~HSSAFETYX==1, subset(HOMEdesign, poverty == 1 & 
                                elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty == 1 & 
                               elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMORAL==1, subset(HOMEdesign, poverty == 1 & 
                              elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, poverty == 1 & 
                                 elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSALTX==1, subset(HOMEdesign, poverty == 1 & 
                             elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSFMLY==1, subset(HOMEdesign, poverty == 1 & 
                             elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)

# What did non-college educated parents living in poverty and homeschooling 
# children in grades 7-12 say was their most important reason for homeschooling?
desp <- subset(HOME, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2)
round(wpct(desp$HSMOSTX, weight=desp$FPWT, na.rm=TRUE), digits = 3)

# What did OTHER parents say was their most important reason for homeschooling?
notdesp <- subset(HOME, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1)
round(wpct(notdesp$HSMOSTX, weight=notdesp$FPWT, na.rm=TRUE), digits = 3)

# Most important reason is morals: create design object and run a t-test
svymean(~HSMOSTX==4, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMOSTX==4, subset(HOMEdesign, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  desp_else = 
                       ifelse(poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2, 
                              "desp", "else"))
svyttest((HSMOSTX==4) ~ desp_else, 
         HOMEdesign, na.rm=TRUE)


# END Reasons for Homeschooling