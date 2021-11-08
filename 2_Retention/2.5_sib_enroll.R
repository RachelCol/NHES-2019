# HOMESCHOOLED CHILDREN with siblings in school

# note: This script is designed to run after 0_data_subsets script.

# note: This script uses two variables, "sibHS" and "sibENRL", that were
# created in the 0_data_subsets document specifically for this analysis.

# -----

# PERCENT of homeschooled children with a homeschooled sibling:
svymean(~sibHS, HOMEdesign)
# PERCENT of homeschooled children with  sibling that attends school:
svymean(~sibENRL, HOMEdesign)

# -----

# ALWAYS v. SOME YEARS TRANFER v. FIRST YEAR TRANSFER
svymean(~sibENRL, subset(HOMEdesign, ALWAYS == 1))
svymean(~sibENRL, subset(HOMEdesign, ALWAYS != 1))
svymean(~sibENRL, subset(HOMEdesign, FIRST != 1 & ALWAYS != 1))
svymean(~sibENRL, subset(HOMEdesign, FIRST == 1 & ALWAYS == 0))
# create comparison variables
HOMEdesign <- update(HOMEdesign,  always_first = ifelse(ALWAYS == 1, "always", 
                                                        ifelse(FIRST == 1, "first", NA)))
HOMEdesign <- update(HOMEdesign,  always_transf = ifelse(ALWAYS == 1, "always", "first"))
# run t-tests
svyttest(sibENRL ~ always_first, 
         HOMEdesign,
         na.rm=TRUE)
svyttest(sibENRL ~ always_transf, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# GRADES K-6 v. 7-12
svymean(~sibENRL, subset(HOMEdesign, elementary_secondary == 1))
svymean(~sibENRL, subset(HOMEdesign, elementary_secondary == 2))
# run t-tests
svyttest(sibENRL ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# Sibling enrolled in school. first year homeschooled students in grades 7-12
svymean(~sibENRL, subset(HOMEdesign, FIRST == 1 & elementary_secondary == 2))

# -----

# by poverty level
svymean(~sibENRL, subset(HOMEdesign, poverty == 1))
svymean(~sibENRL, subset(HOMEdesign, poverty != 1))
# create comparison variable
HOMEdesign <- update(HOMEdesign,  poverty_no = ifelse(poverty == 1, "poverty", "no"))
# run t-test
svyttest(sibENRL ~ poverty_no, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# by parent education
svymean(~sibENRL, subset(HOMEdesign, ba_no_ba == 1))
svymean(~sibENRL, subset(HOMEdesign, ba_no_ba == 2))
# run t-test
svyttest(sibENRL ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# by SES
svymean(~sibENRL, subset(HOMEdesign, SES == 1))
svymean(~sibENRL, subset(HOMEdesign, SES != 1))
svymean(~sibENRL, subset(HOMEdesign, SES == 2))
svymean(~sibENRL, subset(HOMEdesign, SES == 3))
# create comparison variable
HOMEdesign <- update(HOMEdesign,  lowSES_no = ifelse(SES == 1, "lowSES", "no"))
# run t-test
svyttest(sibENRL ~ lowSES_no, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# by reasons for homeschooling
svymean(~sibENRL, subset(HOMEdesign, HSRELGON == 1)) # religious instruction
svymean(~sibENRL, subset(HOMEdesign, HSDISABLX == 1)) # disability

# -----

# CREATE DATA SET for regression, SIBLINGS ENROLLED IN SCHOOL

SIB <- HOME
# turn relevant columns into integers
SIB$ALWAYS <- as.integer(SIB$ALWAYS)
SIB$DISABILITY <- as.integer(SIB$DISABILITY)
SIB$SES <- as.integer(SIB$SES)
SIB$elementary_secondary <- as.integer(SIB$elementary_secondary)
SIB$sibENRL <- as.integer(SIB$sibENRL)
SIB$FIRST <- as.integer(SIB$FIRST)

# CREATE design object from new data set
SIBdesign <- svrepdesign(
  data = SIB, 
  repweights = subset(SIB, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(SIBdesign)

# RUN REGRESSIONS

# always v. transfer, low SES v middle/high, elementary v. secondary
summary(svyglm((sibENRL) ~ ALWAYS + (SES==1) + elementary_secondary, 
               family=quasibinomial, SIBdesign))
# only always and SES have a statistically significant effect

# always v. transfer, low SES v. middle/high
summary(svyglm((sibENRL) ~ ALWAYS + (SES==1), 
               family=quasibinomial, SIBdesign))
# both variables have a statistically significant effect

# checking whether there is an interaction: the answer is no
summary(svyglm((sibENRL) ~ ALWAYS + (SES==1) + ALWAYS*(SES==1), 
               family=quasibinomial, SIBdesign))
# because there is no interaction, discard this regression

# create function
invlogit <- function(x) {1/(1+exp(-x))}

# for a regression using ALWAYS and SES==1:
# effect of ALWAYS, yes or no
invlogit(-1.02-1.34*1) - invlogit(-1.02-1.34*0)
# effect of SES==1, yes or no
invlogit(-1.02+1.32*1) - invlogit(-1.02+1.32*0)

# END of siblings enrolled script