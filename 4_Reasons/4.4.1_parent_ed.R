# REASONS FOR HOMESCHOOLING
# by parent education level (BA or no BA)

# note: This script is designed to run after 0_data_subsets script.

# Create subsets to enable comparison 

HS_BA <- subset(HOME, ba_no_ba == 1)
HS_NBA <- subset(HOME, ba_no_ba == 2)

# -----

# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?
wpct(HS_BA$HSSAFETYX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSSAFETYX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?
wpct(HS_BA$HSDISSATX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSDISSATX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?
wpct(HS_BA$HSRELGON, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSRELGON, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?
wpct(HS_BA$HSMORAL, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSMORAL, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# COMBINED DISABILITY
# combines HSDISABLX, HSILLX, and HSSPCLNDSX
wpct(HS_BA$disability, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$disability, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?
wpct(HS_BA$HSDISABLX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSDISABLX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?
wpct(HS_BA$HSILLX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSILLX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 
wpct(HS_BA$HSSPCLNDX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSSPCLNDX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?
wpct(HS_BA$HSALTX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSALTX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?
wpct(HS_BA$HSFMLY, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSFMLY, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 
wpct(HS_BA$HSOTHERX, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSOTHERX, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 
wpct(HS_BA$HSBULLY, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$HSBULLY, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# Most important reason: HSMOSTX
round(wpct(HS_BA$HSMOSTX, weight=HS_BA$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_NBA$HSMOSTX, weight=HS_NBA$FPWT, na.rm=TRUE), digits = 3)
            
svyttest((HSMOSTX == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)


# COMBINE SPECIAL NEEDS: 
# I created the column PFI$disability in the Data Subsets document
# to combine HSDISABLX, HSILLX, and HSSPCLNDX.
wpct(HS_BA$disability, weight=HS_BA$FPWT, na.rm=TRUE)
wpct(HS_NBA$disability, weight=HS_NBA$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# END Reasons for Homeschooling