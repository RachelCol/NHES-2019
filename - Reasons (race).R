# Reasons for Homeschooling
# Overall, and by race (white v. nonwhite)

# Create subsets to enable comparison 

HOME <- subset(PFI, SCHTYPE == 3)

HS_W <- subset(HOME, SCHTYPE == 3 & white_nonwhite == 1)
HS_M <- subset(HOME, SCHTYPE == 3 & white_nonwhite == 2)


# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?

# All homeschooled students
wpct(HOME$HSSAFETYX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSSAFETYX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSSAFETYX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?

# All homeschooled students
wpct(HOME$HSDISSATX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSDISSATX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSDISSATX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?

# All homeschooled students
wpct(HOME$HSRELGON, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSRELGON, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSRELGON, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?

# All homeschooled students
wpct(HOME$HSMORAL, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students

wpct(HS_W$HSMORAL, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSMORAL, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?

# All homeschooled students
wpct(HOME$HSDISABLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSDISABLX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSDISABLX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?

# All homeschooled students
wpct(HOME$HSILLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSILLX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSILLX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 

# All homeschooled students
wpct(HOME$HSSPCLNDX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSSPCLNDX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSSPCLNDX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?

# All homeschooled students
wpct(HOME$HSALTX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSALTX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSALTX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?

# All homeschooled students
wpct(HOME$HSFMLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSFMLY, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSFMLY, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 

# All homeschooled students
wpct(HOME$HSOTHERX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSOTHERX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSOTHERX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 

# All homeschooled students
wpct(HOME$HSBULLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSBULLY, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSBULLY, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# Most important reason: HSMOSTX

# All homeschooled students
wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(HS_W$HSMOSTX, weight=HS_W$FPWT, na.rm=TRUE)
wpct(HS_M$HSMOSTX, weight=HS_M$FPWT, na.rm=TRUE)

svyttest((HSMOSTX == 1) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ white_nonwhite, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# END Reasons for Homeschooling