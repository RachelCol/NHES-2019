# ACADEMIC REASONS FOR HOMESCHOOLING by demographic

# note: This script is designed to run after 0_data_subsets script.

# -----

# by bachelor's degree
svymean(~HSDISSATX==1, subset(HOMEdesign, ba_no_ba == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, ba_no_ba == 2), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

# by poverty
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 1), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ (poverty==1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 3), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 3), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ (poverty==3), 
         HOMEdesign,
         na.rm=TRUE)

# by SES
svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 2), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 3), na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 3), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, SES != 3), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ (SES==3), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, SES != 1), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ (SES==1), 
         HOMEdesign,
         na.rm=TRUE)

# by grade level
svymean(~HSDISSATX==1, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# by race
svymean(~HSDISSATX==1, subset(HOMEdesign, white_nonwhite == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, white_nonwhite == 2), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ white_nonwhite, 
         HOMEdesign,
         na.rm=TRUE)

# combining several categories

# low income, not college educated, grades 7-12
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & PARGRADEX<4 & elementary_secondary==2))
# what percent of the sample is this?
svymean(~(poverty == 1 & PARGRADEX<4 & elementary_secondary==2), HOMEdesign)
# what is the coefficeint of variation?
cv(svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & PARGRADEX<4 & elementary_secondary==2)))
# just low income and elementary v. secondary
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary==2))
# just low SES and elementary v. secondary
svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 1 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES == 1 & elementary_secondary==2))
# just no college degree and elementary v. secondary
svymean(~HSDISSATX==1, subset(HOMEdesign, PARGRADEX<4 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, PARGRADEX<4 & elementary_secondary==2))

# -----

# CREATE REGRESSION OBJECT

ACA <- HOME
# turn ACAevant columns into integers
ACA$ALWAYS <- as.integer(ACA$ALWAYS)
ACA$DISABILITY <- as.integer(ACA$DISABILITY)
ACA$SES <- as.integer(ACA$SES)
ACA$elementary_secondary <- as.integer(ACA$elementary_secondary)
ACA$FIRST <- as.integer(ACA$FIRST)
ACA$PARGRADEX <- as.integer(ACA$PARGRADEX)
ACA$HSRELGON <- as.integer(ACA$HSRELGON)
ACA$poverty <- as.integer(ACA$poverty)

# CREATE design object from new data set
ACAdesign <- svrepdesign(
  data = ACA, 
  repweights = subset(ACA, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(ACAdesign)

# run regressions

summary(svyglm((DISABILITY) ~ (poverty==3) + (PARGRADEX>3) + (ALWAYS == 1) + (HSRELGON == 1) + 
                 (poverty==3)*(PARGRADEX>3) + (ALWAYS == 1)*(HSRELGON == 1), 
               family=quasibinomial, ACAdesign))

summary(svyglm((DISABILITY) ~ (ALWAYS == 1) + (HSRELGON == 1) + (ALWAYS == 1)*(HSRELGON == 1), 
               family=quasibinomial, ACAdesign))

summary(svyglm((DISABILITY == 1) ~ (ALWAYS == 1) + (HSRELGON == 1), 
               family=quasibinomial, ACAdesign))
# create function
invlogit <- function(x) {1/(1+exp(-x))}

# for a regression using PARGRADEX > 3: 
invlogit (-0.15 + 0.96*1) - invlogit (-0.15 + 0.96*0)

# END script