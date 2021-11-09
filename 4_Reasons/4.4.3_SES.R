# Reasons for Homeschooling
# By poverty level

# This script is in the process of being cleaned up.

# note: This script is designed to run after 0_data_subsets script.


# Create subsets to enable comparison 

HS_P <- subset(HOME, SCHTYPE == 3 & poverty < 3)
HS_N <- subset(HOME, SCHTYPE == 3 & poverty == 3)

HOMEdesign <- update(HOMEdesign,  poor_nonpoor = ifelse(poverty < 3, "poor", 
                                                        ifelse(poverty == 3, "nonpoor", NA)))


# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?
wpct(HS_P$HSSAFETYX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSSAFETYX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?
wpct(HS_P$HSDISSATX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSDISSATX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?
wpct(HS_P$HSRELGON, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSRELGON, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?
wpct(HS_P$HSMORAL, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSMORAL, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# COMBINED DISABILITY
# combines HSDISABLX, HSILLX, and HSSPCLNDSX
wpct(HS_P$disability, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$disability, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?
wpct(HS_P$HSDISABLX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSDISABLX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?
wpct(HS_P$HSILLX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSILLX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 
wpct(HS_P$HSSPCLNDX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSSPCLNDX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?
wpct(HS_P$HSALTX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSALTX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?
wpct(HS_P$HSFMLY, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSFMLY, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 
wpct(HS_P$HSOTHERX, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSOTHERX, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 
wpct(HS_P$HSBULLY, weight=HS_P$FPWT, na.rm=TRUE)
wpct(HS_N$HSBULLY, weight=HS_N$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)


# Most important reason: HSMOSTX
round(wpct(HS_P$HSMOSTX, weight=HS_P$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_N$HSMOSTX, weight=HS_N$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMOSTX == 1) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ poor_nonpoor, 
         HOMEdesign,
         na.rm=TRUE)


# poor non college educated secondary homeschoolers' academic motivations
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  desp_else = ifelse(poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2, "desp", "else"))
svyttest((HSDISSATX==1) ~ desp_else, 
         HOMEdesign,
         na.rm=TRUE)

# What ARE these parents' motivations?
svymean(~HSSAFETYX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMORAL==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSALTX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSFMLY==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)

desp <- subset(HOME, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2)
round(wpct(desp$HSMOSTX, weight=desp$FPWT, na.rm=TRUE), digits = 3)

notdesp <- subset(HOME, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1)
round(wpct(notdesp$HSMOSTX, weight=notdesp$FPWT, na.rm=TRUE), digits = 3)

svymean(~HSMOSTX==4, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMOSTX==4, subset(HOMEdesign, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  desp_else = ifelse(poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2, "desp", "else"))
svyttest((HSMOSTX==4) ~ desp_else, 
         HOMEdesign,
         na.rm=TRUE)



svytable(~HSMOSTX, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2))


# -----

# 5. SES BY REASONS FOR HOMESCHOOLING

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



# END Reasons for Homeschooling