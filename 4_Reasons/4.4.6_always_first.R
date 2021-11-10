# Reasons for Homeschooling
# Always homeschoolers v. First-year hoemschool transfers

# Create subsets to enable comparison 

HS_A <- subset(HOME, SCHTYPE == 3 & ALWAYS == 1) # always homeschooling
HS_F <- subset(HOME, SCHTYPE == 3 & FIRST == 1 & ALWAYS!= 1) # first-year transfer
HS_T <- subset(HOME, SCHTYPE == 3 & ALWAYS != 1) # all homeschool transfers

# Create a design object for comparison

HOMEdesign <- update(HOMEdesign,  always_first = ifelse(ALWAYS==1, "always", 
                                                      ifelse(FIRST==1, "first", NA)))

# -----

# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?
wpct(HS_A$HSSAFETYX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSSAFETYX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSSAFETYX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?
wpct(HS_A$HSDISSATX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSDISSATX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSDISSATX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?
wpct(HS_A$HSRELGON, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSRELGON, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSRELGON, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?
wpct(HS_A$HSMORAL, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSMORAL, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSMORAL, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# COMBINED DISABILITY
# combines HSDISABLX, HSILLX, and HSSPCLNDSX
wpct(HS_A$disability, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$disability, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$disability, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?
wpct(HS_A$HSDISABLX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSDISABLX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSDISABLX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?
wpct(HS_A$HSILLX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSILLX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSILLX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 
wpct(HS_A$HSSPCLNDX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSSPCLNDX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSSPCLNDX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?
wpct(HS_A$HSALTX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSALTX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSALTX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?
wpct(HS_A$HSFMLY, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSFMLY, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSFMLY, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 
wpct(HS_A$HSOTHERX, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSOTHERX, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSOTHERX, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 
wpct(HS_A$HSBULLY, weight=HS_A$FPWT, na.rm=TRUE)
wpct(HS_F$HSBULLY, weight=HS_F$FPWT, na.rm=TRUE)
wpct(HS_T$HSBULLY, weight=HS_T$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# Most important reason: HSMOSTX

# All homeschooled students
round(wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Always v. first year transfers v. all transfers
round(wpct(HS_A$HSMOSTX, weight=HS_A$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_F$HSMOSTX, weight=HS_F$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_T$HSMOSTX, weight=HS_T$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMOSTX == 1) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~(HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7), subset(HOMEdesign, ALWAYS == 1), na.rm=T)
svymean(~(HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7), subset(HOMEdesign, FIRST == 1 & ALWAYS != 1), na.rm=T)

# END Reasons for Homeschooling