# Reasons for Homeschooling
# Overall, and by parent education level (BA or no BA)

# Create subsets to enable comparison 

HOME <- subset(PFI, SCHTYPE == 3)

HS_BA <- subset(HOME, SCHTYPE == 3 & ba_no_ba == 1)
HS_NBA <- subset(HOME, SCHTYPE == 3 & ba_no_ba == 2)


# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?

# All homeschooled students
wpct(HOME$HSSAFETYX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSSAFETYX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSSAFETYX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?

# All homeschooled students
wpct(HOME$HSDISSATX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSDISSATX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSDISSATX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?

# All homeschooled students
wpct(HOME$HSRELGON, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSRELGON, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSRELGON, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?

# All homeschooled students
wpct(HOME$HSMORAL, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students

wpct(HS_BA$HSMORAL, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSMORAL, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?

# All homeschooled students
wpct(HOME$HSDISABLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSDISABLX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSDISABLX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?

# All homeschooled students
wpct(HOME$HSILLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSILLX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSILLX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 

# All homeschooled students
wpct(HOME$HSSPCLNDX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSSPCLNDX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSSPCLNDX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?

# All homeschooled students
wpct(HOME$HSALTX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSALTX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSALTX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?

# All homeschooled students
wpct(HOME$HSFMLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSFMLY, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSFMLY, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 

# All homeschooled students
wpct(HOME$HSOTHERX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSOTHERX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSOTHERX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 

# All homeschooled students
wpct(HOME$HSBULLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSBULLY, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSBULLY, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# Most important reason: HSMOSTX

# All homeschooled students
wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$HSMOSTX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSMOSTX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSMOSTX == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# COMBINE SPECIAL NEEDS: 
# I created the column PFI$disability in the Data Subsets document
# to combine HSDISABLX, HSILLX, and HSSPCLNDX.

# All homeschooled students
wpct(HOME$disability, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_BA$disability, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$disability, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ ba_no_ba, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# END Reasons for Homeschooling