# REASONS FOR HOMESCHOOLING
# by in/near poverty v. over 200% of the poverty level

# note: This script is designed to run after 0_data_subsets script.

# Create subsets to enable comparison 

HS_HS <- subset(HOME, SES == 3)
HS_LMS <- subset(HOME, SES < 3)

HS_M <- subset(HOME, SES == 2)
HS_L <- subset(HOME, SES == 1)

# create design object for comparison

HOMEdesign <- update(HOMEdesign,  highS_mlS = ifelse(SES == 3, "highS", "mlS"))

# -----

# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?
wpct(HS_LMS$HSSAFETYX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSSAFETYX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSSAFETYX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSSAFETYX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?
wpct(HS_LMS$HSDISSATX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSDISSATX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSDISSATX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSDISSATX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?
wpct(HS_LMS$HSRELGON, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSRELGON, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSRELGON, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSRELGON, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?
wpct(HS_LMS$HSMORAL, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSMORAL, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSMORAL, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSMORAL, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# COMBINED DISABILITY
# combines HSDISABLX, HSILLX, and HSSPCLNDSX
wpct(HS_LMS$disability, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$disability, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$disability, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$disability, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?
wpct(HS_LMS$HSDISABLX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSDISABLX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSDISABLX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSDISABLX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?
wpct(HS_LMS$HSILLX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSILLX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSILLX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSILLX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 
wpct(HS_LMS$HSSPCLNDX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSSPCLNDX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSSPCLNDX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSSPCLNDX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?
wpct(HS_LMS$HSALTX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSALTX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSALTX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSALTX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?
wpct(HS_LMS$HSFMLY, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSFMLY, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSFMLY, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSFMLY, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 
wpct(HS_LMS$HSOTHERX, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSOTHERX, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSOTHERX, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSOTHERX, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 
wpct(HS_LMS$HSBULLY, weight=HS_LMS$FPWT, na.rm=TRUE)
wpct(HS_HS$HSBULLY, weight=HS_HS$FPWT, na.rm=TRUE)

wpct(HS_M$HSBULLY, weight=HS_M$FPWT, na.rm=TRUE)
wpct(HS_L$HSBULLY, weight=HS_L$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# Most important reason: HSMOSTX
round(wpct(HS_LMS$HSMOSTX, weight=HS_LMS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_HS$HSMOSTX, weight=HS_HS$FPWT, na.rm=TRUE), digits = 3)

round(wpct(HS_M$HSMOSTX, weight=HS_M$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HS_L$HSMOSTX, weight=HS_L$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMOSTX == 1) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ highS_mlS, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# Percent in each SES group, by each reason for homeschooling

# SES by whether a family is homeschooling due to a disability
part <- subset(HOME, disability == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether a family has religious reasons for homeschooling
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HStRELGON != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether families are homeschooling due to academics
part <- subset(HOME, HSDISSATX == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSDISSATX != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether a family is homeschooling due to the school environment
part <- subset(HOME, HSSAFETYX == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSSAFETYX != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# SES by whether the family has two parents or a single parent
part <- subset(HOME, two_parent_or_single == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, two_parent_or_single == 2 & SCHTYPE == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# END script