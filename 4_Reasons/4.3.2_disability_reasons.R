# DISABILITY and reasons for homeschooling

# note: This script is designed to run after 0_data_subsets script.

# -----

# Total number of reasons for homeschooling selected, by disability or no
svymean(~TOTAL, subset(HOMEdesign, disability == 1))
svymean(~TOTAL, subset(HOMEdesign, disability != 1))

# Percent selecting disability, by length of time homeschooling
svymean(~disability == 1, subset(HOMEdesign, FIRST == 1))
svymean(~disability == 1, subset(HOMEdesign, TOTAL > 3))
svymean(~disability == 1, subset(HOMEdesign, ALWAYS == 1))

# by grade level
svymean(~disability==1, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)

# percent of children homeschooled due to a disability that are in K-6, and 7-12
svymean(~elementary_secondary == 1, subset(HOMEdesign, disability==1), na.rm=TRUE)
svymean(~elementary_secondary == 2, subset(HOMEdesign, disability==1), na.rm=TRUE)

# percent of kids homeschooled due to a disability that are white
svymean(~white_nonwhite == 1, subset(HOMEdesign, disability==1), na.rm=TRUE)

# families homeschooling due to a disability are less likely to religion
svymean(~HSRELGON == 1, subset(HOMEdesign, disability == 1))
svymean(~HSRELGON == 1, subset(HOMEdesign, disability == 2))
svyttest((HSRELGON==1) ~ disability, 
         HOMEdesign,
         na.rm=TRUE)

# families homeschooling due to a disability are less likely to nontraditional
svymean(~HSALTX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSALTX == 1, subset(HOMEdesign, disability == 2))
svyttest((HSALTX == 1) ~ disability, 
         HOMEdesign,
         na.rm=TRUE)

# Disabilities, by whether parents have religious reasons for homeschooling
svymean(~disability == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

svyttest((disability == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# by ALWAYS v. TRANSFER

HOMEdesign <- update(HOMEdesign,  first_always = ifelse(ALWAYS==1, "always", ifelse(FIRST==1, "first", NA)))
HOMEdesign <- update(HOMEdesign,  first_some = ifelse(FIRST==1 & ALWAYS!=1, "first", ifelse(ALWAYS!=1 & FIRST!=1, "some", NA)))
HOMEdesign <- update(HOMEdesign,  always_some = ifelse(ALWAYS==1, "always", ifelse(FIRST!=1, "some", NA)))

# Disability is A reason for homeschooling
svymean(~disability == 1, subset(HOMEdesign, FIRST == 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, FIRST == 1 & ALWAYS != 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, FIRST != 1 & ALWAYS != 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)

svyttest((disability == 1) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((disability == 1) ~ first_some, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((disability == 1) ~ always_some, 
         HOMEdesign,
         na.rm=TRUE)

# Disability is MOST IMPORTANT reason for homeschooling
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, FIRST == 1), na.rm=TRUE)
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, FIRST == 1 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, FIRST != 1 & ALWAYS != 1), na.rm=TRUE)
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ first_some, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ always_some, 
         HOMEdesign,
         na.rm=TRUE)

# -----

# CREATE DATA SET for regression, DISABILITIES AS A MOTIVATION

DIS <- HOME
# turn relevant columns into integers
DIS$ALWAYS <- as.integer(DIS$ALWAYS)
DIS$DISABILITY <- as.integer(DIS$disability)
DIS$SES <- as.integer(DIS$SES)
DIS$elementary_secondary <- as.integer(DIS$elementary_secondary)
DIS$sibENRL <- as.integer(DIS$sibENRL)
DIS$FIRST <- as.integer(DIS$FIRST)
DIS$PARGRADEX <- as.integer(DIS$PARGRADEX)
DIS$poverty <- as.integer(DIS$poverty)

# CREATE design object from new data set
DISdesign <- svrepdesign(
  data = DIS, 
  repweights = subset(DIS, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(DISdesign)

# run regressions
summary(svyglm((DISABILITY) ~ (poverty==3) + (PARGRADEX>3) + (ALWAYS == 1), 
               family=quasibinomial, DISdesign))

summary(svyglm((DISABILITY) ~ (poverty==3) + (PARGRADEX>3) + (ALWAYS == 1), 
               family=quasibinomial, DISdesign))

summary(svyglm((DISABILITY) ~ (PARGRADEX>3), 
               family=quasibinomial, DISdesign))

summary(svyglm((DISABILITY) ~ (poverty==3), 
               family=quasibinomial, DISdesign))

# new regressions with ALWAYS and HSRELGON

summary(svyglm((DISABILITY) ~ (poverty==3) + (PARGRADEX>3) + (ALWAYS == 1) + (HSRELGON == 1) + 
                 (poverty==3)*(PARGRADEX>3) + (ALWAYS == 1)*(HSRELGON == 1), 
               family=quasibinomial, DISdesign))

summary(svyglm((DISABILITY) ~ (ALWAYS == 1) + (HSRELGON == 1) + (ALWAYS == 1)*(HSRELGON == 1), 
               family=quasibinomial, DISdesign))

summary(svyglm((DISABILITY == 1) ~ (ALWAYS == 1) + (HSRELGON == 1), 
               family=quasibinomial, DISdesign))

invlogit <- function(x) {1/(1+exp(-x))}

# effect of ALWAYS and HSRELGON
invlogit(-0.22 - 0.98*1 - 0.74*1) - invlogit(-0.22 - 0.98*0 - 0.74*0)

# double checking: 
svymean(~disability == 1, subset(HOMEdesign, ALWAYS == 1 & HSRELGON == 1))
svymean(~disability == 1, subset(HOMEdesign, ALWAYS != 1 & HSRELGON != 1))

# -----

# TYPE OF DISABILITY

# Does this child have specific disabilities? 

# having any condition
svymean(~condition==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~condition==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((condition==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having an intellectual disability (mental retardation)
svymean(~HDINTDIS==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDINTDIS==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDINTDIS==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a speech impairment
svymean(~HDSPEECHX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDSPEECHX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDSPEECHX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a serious emotional disturbance
svymean(~HDDISTRBX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDISTRBX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDDISTRBX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having deafness or hearing impairment
svymean(~HDDEAFIMX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDEAFIMX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDDEAFIMX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a blindness or visual impairment
svymean(~HDBLINDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDBLINDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDBLINDX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having an orthopedic impairment 
svymean(~HDORTHOX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDORTHOX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDORTHOX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having autism
svymean(~HDAUTISMX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDAUTISMX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDAUTISMX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having pervasive developmental disorder (PDD)
svymean(~HDPDDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDPDDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDPDDX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having ADD or ADHD 
svymean(~HDADDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDADDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDADDX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a specific learning disability
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDLEARNX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# the above, broken down by SES
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 1), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 2), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 3), na.rm=TRUE)

# having a developmental delay
svymean(~HDDELAYX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDELAYX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDDELAYX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a traumatic brain injury
svymean(~HDTRBRAIN==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDTRBRAIN==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDTRBRAIN==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having other health impairment longer than 6mos
svymean(~HDOTHERX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDOTHERX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDOTHERX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# END SCRIPT