# REASONS FOR HOMESCHOOLING

# Overall, and by school level (grades K-6 v. grades 7-12);
# All reasons for homeschooling, and most important reason.

# note: This script is designed to run after 0_data_subsets script.

# Data subsets for comparing grades K-6 with grades 7-12
EL_HS <- subset(HOME, SCHTYPE == 3 & elementary_secondary == 1)
SEC_HS <- subset(HOME, SCHTYPE == 3 & elementary_secondary == 2)

# -----

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?

# All homeschooled students
round(wpct(HOME$HSSAFETYX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSSAFETYX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSSAFETYX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSAFETYX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?

# All homeschooled students
round(wpct(HOME$HSDISSATX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSDISSATX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSDISSATX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSDISSATX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?

# All homeschooled students
round(wpct(HOME$HSRELGON, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSRELGON, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSRELGON, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSRELGON == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?

# All homeschooled students
round(wpct(HOME$HSMORAL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students

round(wpct(EL_HS$HSMORAL, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSMORAL, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMORAL == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# ALL DISABILITIES
# Combines HSDISABLX and HSILLX and HSSPCLNDX

# All homeschooled students
round(wpct(HOME$disability, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
ruound(wpct(EL_HS$disability, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$disability, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((disability == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?

# All homeschooled students
round(wpct(HOME$HSDISABLX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSDISABLX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSDISABLX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSDISABLX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?

# All homeschooled students
round(wpct(HOME$HSILLX, weight=HOME$FPWT, na.rm=TRUE))

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSILLX, weight=EL_HS$FPWT, na.rm=TRUE))
round(wpct(SEC_HS$HSILLX, weight=SEC_HS$FPWT, na.rm=TRUE))

svyttest((HSILLX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 

# All homeschooled students
round(wpct(HOME$HSSPCLNDX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSSPCLNDX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSSPCLNDX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSSPCLNDX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?

# All homeschooled students
round(wpct(HOME$HSALTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSALTX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSALTX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSALTX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSFMLY
# You want to emphasize family life together?

# All homeschooled students
round(wpct(HOME$HSFMLY, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSFMLY, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSFMLY, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSFMLY == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# HSOTHERX
# You have another reason for homeschooling your child? 

# All homeschooled students
round(wpct(HOME$HSOTHERX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSOTHERX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSOTHERX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSOTHERX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# ----

# HSBULLY
# This child was bullied at school. 

# All homeschooled students
round(wpct(HOME$HSBULLY, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSBULLY, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSBULLY, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSBULLY == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# -----

# Most important reason: HSMOSTX

# All homeschooled students
round(wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSMOSTX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSMOSTX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMOSTX == 1) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 2) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 3) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 4) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 5) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 6) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 7) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 8) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 9) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 10) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)
svyttest((HSMOSTX == 11) ~ elementary_secondary, 
         HOMEdesign, na.rm=TRUE)

# END Reasons for Homeschooling