# Clean data, create data sets, load libraries

# Binary columns created in data subset document:
# - home_public -> homeschool (1) or public school (2)
# - elementary_secondary -> grades 1-6 (1) or grades 7-12 (2)
# - ba_no_ba -> BA (1) or no BA (2)
# - white_nonwhite -> white (1) or non-white (2)
# - two_parent_or_single -> two-parent household (1) or single parent (2)
# - english_or_no -> English spoken in the home (1) or no (2)


# PACKAGES

# NOTE: need to check which of these packages I actually need.

# install.packages("tables")
# install.packages("haven")
# install.packages("survey")
# install.packages("srvyr")
# install.packages("dplyr")
# install.packages("jtools")
# install.packages("remotes")
# install.packages("Hmisc")
# install.packages("foreign")
# install.packages("weights")
# install.packages("tibble")
# install.packages("ggplot2")
# install.packages("xlsx")
# install.packages("tidyr")
# remotes::install_github("carlganz/svrepmisc")

library("tables")
library("haven")
library("survey")
library("srvyr")
library("dplyr")
library("jtools")
library("remotes")
library("Hmisc") 
library("foreign")
library("weights")
library("svrepmisc")
library("tibble")
library("ggplot2")
library("xlsx")
library("tidyr")

load(file = "pfi_pu_pert.RData")

# PFI data: Replace all -1s with NAs, change grade level numbers
pfi_pu_pert[pfi_pu_pert == -1] <- NA
pfi_pu_pert$ALLGRADEX <- c(pfi_pu_pert$ALLGRADEX -3)
pfi_pu_pert$ALLGRADEX[pfi_pu_pert$ALLGRADEX == -1] <- 0

PFI <- pfi_pu_pert

# Create a column with: 1 = Elementary; 2 = Middle; 3 = High School
PFI$SCHLEVEL <- ifelse(PFI$ALLGRADEX == 0 | 
                         PFI$ALLGRADEX == 1 | 
                         PFI$ALLGRADEX == 2 | 
                         PFI$ALLGRADEX == 3 | 
                         PFI$ALLGRADEX == 4 | 
                         PFI$ALLGRADEX == 5, 1, 
                       ifelse(PFI$ALLGRADEX == 6 | 
                                PFI$ALLGRADEX == 7 | 
                                PFI$ALLGRADEX == 8, 2, 
                              ifelse(PFI$ALLGRADEX == 9 | 
                                       PFI$ALLGRADEX == 10 | 
                                       PFI$ALLGRADEX == 11 | 
                                       PFI$ALLGRADEX == 12, 3, NA)))
# check work using table (should be: 6230, 3812, 6404)
table(PFI$SCHLEVEL)
# note that this column (SCHLEVEL) does not include any NAs

# Create a column with: 1 = Public; 2 = Home; 3 = Private; 4 = Virtual
PFI$SCHTYPE <- ifelse(PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (
                          PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK)), 3, 
                      ifelse(PFI$EDCINTK12 == 1, 4, 
                             ifelse((PFI$EDCCAT == 1 | 
                                       PFI$EDCREL == 1 | 
                                       PFI$EDCPRI == 1), 2, 
                                    ifelse(PFI$EDCPUB == 1, 1, 5))))
# Note: SCHTYPE == 5 needs to be eliminated. 5 is all "yes" responses to 
    # "EDCINTCOL" or "EDCCOL" and that did not also answer "yes" to other 
    # school types. (EDCINTCOL and EDCCOL are online or in-person college.)
# Remove all rows where SCHTYPE == 5.
PFI <- PFI[ which(PFI$SCHTYPE < 5), ]

# check work using table (should be: 13882, 1736, 496, 250)
table(PFI$SCHTYPE)

# ON DETERMINING which students are homeschooled (lines 48 to 54):
# PFI$EDCHSFL == 1: 
#      1 = 13874, 2 = 1732, 3 = 532, 4 = 232
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1: 
#      1 = 13874, 2 = 1734, 3 = 519, 4 = 240
# PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK):
#      1 = 13882, 2 = 1736, 3 = 496, 4 = 250

# Check grade level counts (should be grades 0 through 12)
table(PFI$ALLGRADEX)

# Add a "counts" column
PFI$countn <- c(1)

# Remove children under age 5 or over age 18
table(PFI$AGE2018)
PFI <- PFI[!(PFI$AGE2018 == 3 | PFI$AGE2018 == 4 | PFI$AGE2018 == 19 | PFI$AGE2018 == 20),]
table(PFI$AGE2018)

# Check grade level counts (should be grades 1 through 12)
table(PFI$ALLGRADEX)

# Create column with: homeschool (1) or public school (2)
PFI$home_public <- ifelse(PFI$SCHTYPE == 3, 1, 
                          ifelse(PFI$SCHTYPE == 1, 2, NA))

# Create column with: homeschool (1) or private school (2)
PFI$home_private <- ifelse(PFI$SCHTYPE == 3, 1, 
                          ifelse(PFI$SCHTYPE == 2, 2, NA))

# Create column with: homeschool (1) or virtual school (2)
PFI$home_virtual <- ifelse(PFI$SCHTYPE == 3, 1, 
                          ifelse(PFI$SCHTYPE == 4, 2, NA))

# Create column with: white (1) or non-white (2)
PFI$white_nonwhite <- ifelse(PFI$RACEETH == 1, 1, 2)

# Create column with: BA (1) or no BA (2)
PFI$ba_no_ba <- ifelse(PFI$PARGRADEX > 3, 1, 2)

# Create column with: grades K-6 (1) or grades 7-12 (2)
PFI$elementary_secondary <- ifelse(PFI$ALLGRADEX < 7, 1, 2)

# Create column with: two-parent household (1) or single parent (2)
PFI$two_parent_or_single <- ifelse(PFI$HHPARN19_BRD == 1, 1, 2)

# Create collection of columns on parents' employment status

# First, create columns for each parent, with: 
   # (1) full time (at least 40hrs); 
   # (2) part-time (less than 40hrs); 
   # (3) not in the workforce.
PFI$full_part1 <- ifelse((PFI$P1HRSWK >= 40), 1, 
                   ifelse((PFI$P1HRSWK < 40), 2, 3))
PFI$full_part2 <- ifelse((PFI$P2HRSWK >= 40), 1, 
                   ifelse((PFI$P2HRSWK < 40), 2, 3))
# Lastly, turn NAs (i.e. not in the workforce) into 3s.
PFI[c(839, 840)][is.na(PFI[c(839, 840)])] <- 3
# NOTE: DO NOT add any new columns before this entry! That would break this!!
# Columns full_part1 and full_part2 should be columns 839 and 840.

which(colnames(PFI)=="full_part1")
which(colnames(PFI)=="full_part2")

# Create a column for two-parent households' employment status
# (1) both work full time; (2) both work, some part-time; (3) one works; (4) both not employed
PFI$two_parent_work <- ifelse((PFI$two_parent_or_single == 2), NA, # excludes single parents
                        ifelse((PFI$full_part1 == 1 & PFI$full_part2 == 1), 1, # both full time
                          ifelse((PFI$full_part1 < 3 & PFI$full_part2 < 3), 2, # both in workforce
                            ifelse((PFI$full_part1 == 3 & PFI$full_part2 == 3), 4, 3))))

# Create a column for single-parent households' employment status
# (1) full time work; (2) part-time work; (3) not employed
PFI$one_parent_work <- ifelse((PFI$two_parent_or_single == 1), NA, # excludes two-parent households
                         ifelse((PFI$full_part1 == 1 | PFI$full_part2 == 1), 1, # full time
                           ifelse((PFI$full_part1 < 3 | PFI$full_part2 < 3), 2, 3))) # part time
                           
# Households with any part-time work (1) or no part-time work (2)
PFI$part_time <- ifelse((PFI$full_part1 == 2 | PFI$full_part2 == 2), 1, 2)

# Households where at least one parent is self-employed (1) or no parent is self employed (2)
PFI$self_employed <- ifelse((PFI$P1EMPL == 2 | PFI$P1EMPL == 2), 1, 2)

# Households with a stay at home parent (1) or no stay at home parent (2)
PFI$sahp <- ifelse((PFI$P1EMPL == 5 | PFI$P2EMPL == 5), 1, 2)

# Percent of women in the workforce (1 = full-time, 2 = part-time, 3 = not employed)
PFI$women_work <- ifelse((PFI$P1SEX == 2 & PFI$full_part1 == 1), 1,
                    ifelse((PFI$P2SEX == 2 & PFI$full_part2 == 1), 1, 
                      ifelse((PFI$P1SEX == 2 & PFI$full_part1 == 2), 2,
                         ifelse((PFI$P2SEX == 2 & PFI$full_part2 == 2), 2, 3))))

# Percent of men in the workforce (1 = full-time, 2 = part-time, 3 = not employed)
PFI$men_work <- ifelse((PFI$P1SEX == 1 & PFI$full_part1 == 1), 1,
                   ifelse((PFI$P2SEX == 1 & PFI$full_part2 == 1), 1, 
                     ifelse((PFI$P1SEX == 1 & PFI$full_part1 == 2), 2,
                        ifelse((PFI$P2SEX == 1 & PFI$full_part2 == 2), 2, 3))))

# Create a column of households with two men (1), two women (2), or not same-sex (3)
PFI$queer <- ifelse((PFI$P1SEX == 1 & PFI$P2SEX == 1), 1, 
                    ifelse((PFI$P1SEX == 2 & PFI$P2SEX == 2), 2, 3))

# Create column with: under 20K (1); 20K-50K (2); 50K-100K (3); over 100K (4)
PFI$income <- ifelse((PFI$TTLHHINC == 1 | 
                      PFI$TTLHHINC == 2), 
                     1, 
                  ifelse((PFI$TTLHHINC == 3 | 
                          PFI$TTLHHINC == 4 | 
                          PFI$TTLHHINC == 5), 
                         2, 
                       ifelse((PFI$TTLHHINC == 6 | 
                               PFI$TTLHHINC == 7 | 
                               PFI$TTLHHINC == 8), 
                              3,
                            ifelse((PFI$TTLHHINC == 9 | 
                                    PFI$TTLHHINC == 10 | 
                                    PFI$TTLHHINC == 11 | 
                                    PFI$TTLHHINC == 12), 
                                   4, NA))))

# Create a column for household poverty status (1 = poverty, 2 = near-poverty, 3 = no poverty)
# combines both TTLHHINC and HHTOTALXX (see note below)
PFI$poverty <- ifelse((PFI$HHTOTALXX == 2 | PFI$HHTOTALXX == 3) & 
                        (PFI$TTLHHINC == 1 | PFI$TTLHHINC == 2), 1, 
               ifelse((PFI$HHTOTALXX == 4 | PFI$HHTOTALXX == 5 | PFI$HHTOTALXX == 6) & 
                        (PFI$TTLHHINC == 1 | PFI$TTLHHINC == 2 | PFI$TTLHHINC == 3), 1, 
               ifelse((PFI$HHTOTALXX == 7 | PFI$HHTOTALXX == 8) & 
                        (PFI$TTLHHINC == 1 | PFI$TTLHHINC == 2 | 
                             PFI$TTLHHINC == 3 | PFI$TTLHHINC == 4), 1,
               ifelse((PFI$HHTOTALXX == 9 | PFI$HHTOTALXX == 10) & 
                        (PFI$TTLHHINC == 1 | PFI$TTLHHINC == 2 | 
                             PFI$TTLHHINC == 3 | PFI$TTLHHINC == 4 | PFI$TTLHHINC == 5), 1,
                  ifelse((PFI$HHTOTALXX == 2 | PFI$HHTOTALXX == 3) & 
                          (PFI$TTLHHINC == 3 | PFI$TTLHHINC == 4), 2, 
                  ifelse((PFI$HHTOTALXX == 4 | PFI$HHTOTALXX == 5 | PFI$HHTOTALXX == 6) & 
                          (PFI$TTLHHINC == 4 | PFI$TTLHHINC == 5 | PFI$TTLHHINC == 6), 2, 
                  ifelse((PFI$HHTOTALXX == 7 | PFI$HHTOTALXX == 8) & 
                          (PFI$TTLHHINC == 5 | PFI$TTLHHINC == 6 | PFI$TTLHHINC == 7), 2,
                  ifelse((PFI$HHTOTALXX == 9 | PFI$HHTOTALXX == 10) & 
                          (PFI$TTLHHINC == 6 | PFI$TTLHHINC == 7 | PFI$TTLHHINC == 8), 2,
                                   3))))))))

# From the NCES: Poverty is defined as follows by household size: 
# (1) if household size is 2 or 3 and income categories TTLHHINC are 1-2 (less than or equal to $20,000); or 
# (2) if household size is 4, 5, or 6 and income categories TTLHHINC are 1-3 (less than or equal to $30,000); or 
# (3) if household size is 7 or 8 and income categories TTLHHINC are 1-4 (less than or equal to $40,000); or 
# (4) if household size is 9 or more and income categories TTLHHINC are 1-5 (less than or equal to $50,000).

# My addition: Near poverty as defined by household size:
# (1) if household size is 2 or 3 and income categories TTLHHINC are 3-4 (less than or equal to $40,000); or 
# (2) if household size is 4, 5, or 6 and income categories TTLHHINC are 4-6 (less than or equal to $60,000); or 
# (3) if household size is 7 or 8 and income categories TTLHHINC are 5-7 (less than or equal to $75,000); or 
# (4) if household size is 9 or more and income categories TTLHHINC are 6-8 (less than or equal to $100,000).

# Create column with: English spoken at home (1) or no (2)
PFI$english_or_no <- ifelse(PFI$HHENGLISH == 1, 1, 2)

# Create a column with: food stamps in past 12 mos (1) or no (2)
PFI$food_stamps <- ifelse(PFI$HFOODST == 1, 1, 2)
PFI$wic <- ifelse(PFI$HWIC == 1, 1, 2)
PFI$tanf <- ifelse(PFI$HWELFTANST == 1, 1, 2)
PFI$medicaid <- ifelse(PFI$HMEDICAID == 1, 1, 2)
PFI$chip <- ifelse(PFI$HCHIP == 1, 1, 2)
PFI$sec8 <- ifelse(PFI$HSECN8 == 1, 1, 2)

PFI$welfare <- ifelse(PFI$food_stamps == 1 | PFI$wic == 1 | PFI$tanf == 1 | 
                         PFI$medicaid == 1 | PFI$chip == 1 | PFI$sec8 == 1, 1, 2)

# Create column that combines all special needs responses: 
# HSDISABLX, HSILLX, HSSPCLNDX
PFI$disability <- ifelse((PFI$HSDISABLX == 1), 1, 
                     ifelse((PFI$HSILLX == 1), 1,
                         ifelse((PFI$HSSPCLNDX == 1), 1, 2)))

# CREATING a measure of socio-economic status:
# parent education levels, 1-5
# family poverty levels, 1-3
# Add those numbers together, giving us 2-8
# Then divide into three: 2-4, 5-6, 7-8

# To be low SES someone has to be (a) under 100% of poverty and 
# completed vocational/technical school or some college max OR (b) 
# under 200% of poverty and completed high school max OR (3) over 
# 200% of poverty, but never completed high school.
# To be middle SES someone has to be (a) under the 200% of poverty 
# line and have a college degree OR (b) over 100% of poverty and
# have voc/tech or some college OR (c) be above 200% of poverty
# and have a high school diploma or equivalent.
# To be high SES someone has to be (a) between 100% and 200% of
# poverty and have a graduate or professional degree OR (b) be above
# the 200% of poverty level and have at least a college degree.

PFI$SESraw <- PFI$PARGRADEX + PFI$poverty
PFI$SES <- ifelse(PFI$SESraw < 5, 1, 
                  ifelse(PFI$SESraw == 5 | PFI$SESraw == 6, 2,
                         ifelse(PFI$SESraw > 6, 3, NA)))

PFI$grade_range <- ifelse(PFI$ALLGRADEX == 1 | PFI$ALLGRADEX == 2 | PFI$ALLGRADEX == 3, 1, 
                           ifelse(PFI$ALLGRADEX == 4 | PFI$ALLGRADEX == 5 | PFI$ALLGRADEX == 6, 2, 
                                  ifelse(PFI$ALLGRADEX == 7 | PFI$ALLGRADEX == 8 | PFI$ALLGRADEX == 9, 3,
                                         ifelse(PFI$ALLGRADEX == 10 | PFI$ALLGRADEX == 11 | PFI$ALLGRADEX == 12, 4,
                                                NA))))

PFI$grade_range2 <- ifelse(PFI$ALLGRADEX == 1 | PFI$ALLGRADEX == 2, 1, 
                          ifelse(PFI$ALLGRADEX == 3 | PFI$ALLGRADEX == 4, 2,
                              ifelse(PFI$ALLGRADEX == 5 | PFI$ALLGRADEX == 6, 3, 
                                 ifelse(PFI$ALLGRADEX == 7 | PFI$ALLGRADEX == 8, 4,
                                        ifelse(PFI$ALLGRADEX == 9 | PFI$ALLGRADEX == 10, 5,
                                            ifelse(PFI$ALLGRADEX == 11 | PFI$ALLGRADEX == 12, 6,
                                               NA))))))

PFI$condition <- ifelse((PFI$HDINTDIS == 1 | PFI$HDSPEECHX == 1 | PFI$HDDISTRBX == 1 | 
                        PFI$HDDEAFIMX == 1 | PFI$HDDEAFIMX == 1 | PFI$HDORTHOX == 1 | 
                        PFI$HDAUTISMX == 1 | PFI$HDPDDX == 1 | PFI$HDADDX == 1 | 
                        PFI$HDADDX == 1 | PFI$HDDELAYX == 1 | PFI$HDTRBRAIN == 1 | 
                        PFI$HDOTHERX == 1), 1, 0) 

# May want to add a column comparing virtual v. non-virtual

# For general PFI data file, input details into survey package
PFIdesign <- svrepdesign(
  data = PFI, 
  repweights = subset(PFI, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(PFIdesign)

# END creation of PFI data set and PFIdesign object


# START HOME DATA SET CREATION

HOME <- subset(PFI, SCHTYPE == 3)

# replace NAs with 0 ("child not homeschooled")
HOME$HOMEKX[is.na(HOME$HOMEKX)] <- 0
HOME$HOME1[is.na(HOME$HOME1)] <- 0
HOME$HOME2[is.na(HOME$HOME2)] <- 0
HOME$HOME3[is.na(HOME$HOME3)] <- 0
HOME$HOME4[is.na(HOME$HOME4)] <- 0
HOME$HOME5[is.na(HOME$HOME5)] <- 0
HOME$HOME6[is.na(HOME$HOME6)] <- 0
HOME$HOME7[is.na(HOME$HOME7)] <- 0
HOME$HOME8[is.na(HOME$HOME8)] <- 0
HOME$HOME9[is.na(HOME$HOME9)] <- 0
HOME$HOME10[is.na(HOME$HOME10)] <- 0
HOME$HOME11[is.na(HOME$HOME11)] <- 0
HOME$HOME12[is.na(HOME$HOME12)] <- 0

# replace 2s with 0 ("child not homeschooled")
HOME$HOMEKX[HOME$HOMEKX == 2] <- 0
HOME$HOME1[HOME$HOME1 == 2] <- 0
HOME$HOME2[HOME$HOME2 == 2] <- 0
HOME$HOME3[HOME$HOME3 == 2] <- 0
HOME$HOME4[HOME$HOME4 == 2] <- 0
HOME$HOME5[HOME$HOME5 == 2] <- 0
HOME$HOME6[HOME$HOME6 == 2] <- 0
HOME$HOME7[HOME$HOME7 == 2] <- 0
HOME$HOME8[HOME$HOME8 == 2] <- 0
HOME$HOME9[HOME$HOME9 == 2] <- 0
HOME$HOME10[HOME$HOME10 == 2] <- 0
HOME$HOME11[HOME$HOME11 == 2] <- 0
HOME$HOME12[HOME$HOME12 == 2] <- 0

# create new column with total number of years homeschooled
HOME$TOTAL <- HOME$HOMEKX + 
  HOME$HOME1 + 
  HOME$HOME2 + 
  HOME$HOME3 + 
  HOME$HOME4 + 
  HOME$HOME5 + 
  HOME$HOME6 + 
  HOME$HOME7 + 
  HOME$HOME8 + 
  HOME$HOME9 + 
  HOME$HOME10 + 
  HOME$HOME11 + 
  HOME$HOME12

table(HOME$TOTAL)
wpct(HOME$TOTAL, weight=HOME$FPWT, na.rm=TRUE)

# Create new column to find students who were always homeschooled
# 1 = always homeschooled, 0 = not always homeschooled
HOME$ALWAYS <- ifelse((HOME$TOTAL == (HOME$ALLGRADEX + 1)), 1, 0)

# Create a new column for first-year homeschooled students
# 1 = first year homeschooling, 0 = not first year
HOME$FIRST <- ifelse(HOME$TOTAL == 1, 1, 0)

# WERE THEY HOMESCHOOLED CONSEQUTIVE YEARS? ADD COLUMNS!

# First year homeschooling, after a gap (includes only grades 2-12)
HOME$FIRSTgap <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 0 & HOME$TOTAL > 1, 1, 
                        ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 0 & HOME$TOTAL > 1, 1, 
                               ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 0 & HOME$TOTAL > 1, 1, 
                                      ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 0 & HOME$TOTAL > 1, 1, 
                                             ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 0 & HOME$TOTAL > 1, 1, 
                                                    ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 0 & HOME$TOTAL > 1, 1, 
                                                           ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 0 & HOME$TOTAL > 1, 1, 
                                                                  ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 0 & HOME$TOTAL > 1, 1, 
                                                                         ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 0 & HOME$TOTAL > 1, 1, 
                                                                                ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 0 & HOME$TOTAL > 1, 1, 
                                                                                       ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 0 & HOME$TOTAL > 1, 1, 
                                                                                              0)))))))))))



# Second consecutive year homeschooling
HOME$SECOND <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 0, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 0, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                                              ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                                     ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                                            ifelse(HOME$ALLGRADEX == 1 & HOME$HOMEKX == 1, 1, 
                                                                                                   0))))))))))))

# Third consecutive year homeschooling
HOME$THIRD <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 0, 1, 
                     ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                            ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                                   ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                          ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                                 ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                        ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                               ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                                      ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                             ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                                    ifelse(HOME$ALLGRADEX == 2 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1,
                                                                                           0)))))))))))

# Fourth consecutive year homeschooling
HOME$FOURTH <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 0, 1, 
                      ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                             ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                                    ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                           ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                                  ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1,
                                                         ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                                ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                                       ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                              ifelse(HOME$ALLGRADEX == 3 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1,
                                                                                     0))))))))))

# Fifth consecutive year homeschooling
HOME$FIFTH <- ifelse(HOME$ALLGRADEX == 12 & HOME$HOME11 == 1 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 0, 1, 
                     ifelse(HOME$ALLGRADEX == 11 & HOME$HOME10 == 1 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 0, 1, 
                            ifelse(HOME$ALLGRADEX == 10 & HOME$HOME9 == 1 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 0, 1, 
                                   ifelse(HOME$ALLGRADEX == 9 & HOME$HOME8 == 1 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 0, 1, 
                                          ifelse(HOME$ALLGRADEX == 8 & HOME$HOME7 == 1 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 0, 1, 
                                                 ifelse(HOME$ALLGRADEX == 7 & HOME$HOME6 == 1 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 0, 1, 
                                                        ifelse(HOME$ALLGRADEX == 6 & HOME$HOME5 == 1 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 0, 1, 
                                                               ifelse(HOME$ALLGRADEX == 5 & HOME$HOME4 == 1 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 0, 1, 
                                                                      ifelse(HOME$ALLGRADEX == 4 & HOME$HOME3 == 1 & HOME$HOME2 == 1 & HOME$HOME1 == 1 & HOME$HOMEKX == 1, 1, 
                                                                             0)))))))))

# What percent homeschooled, returned to school, & came back to homeschooling?
# In columns HOMEKX through HOME12: 
#   -- 1 means "was homeschooled that year"
#   -- 2 means "was not homeschooled that year"
#   -- NA means "child has not reached that grade yet"
HSKids <- subset(PFI, SCHTYPE == 3)
YearsHomeschooled <- HSKids[, 58:70]
# turn all "not homeschooled" (2) before the first "was homeschooled" (1) into (0)
# this way all (2)s will mean "was not homeschooled, in between years of homeschooling"
YearsHomeschooled$HOMEKX[YearsHomeschooled$HOMEKX == 2] <- 0
YearsHomeschooled$HOME1[YearsHomeschooled$HOME1 == 2 & YearsHomeschooled$HOMEKX == 0] <- 0
YearsHomeschooled$HOME2[YearsHomeschooled$HOME2 == 2 & YearsHomeschooled$HOME1 == 0] <- 0
YearsHomeschooled$HOME3[YearsHomeschooled$HOME3 == 2 & YearsHomeschooled$HOME2 == 0] <- 0
YearsHomeschooled$HOME4[YearsHomeschooled$HOME4 == 2 & YearsHomeschooled$HOME3 == 0] <- 0
YearsHomeschooled$HOME5[YearsHomeschooled$HOME5 == 2 & YearsHomeschooled$HOME4 == 0] <- 0
YearsHomeschooled$HOME6[YearsHomeschooled$HOME6 == 2 & YearsHomeschooled$HOME5 == 0] <- 0
YearsHomeschooled$HOME7[YearsHomeschooled$HOME7 == 2 & YearsHomeschooled$HOME6 == 0] <- 0
YearsHomeschooled$HOME8[YearsHomeschooled$HOME8 == 2 & YearsHomeschooled$HOME7 == 0] <- 0
YearsHomeschooled$HOME9[YearsHomeschooled$HOME9 == 2 & YearsHomeschooled$HOME8 == 0] <- 0
YearsHomeschooled$HOME10[YearsHomeschooled$HOME10 == 2 & YearsHomeschooled$HOME9 == 0] <- 0
YearsHomeschooled$HOME11[YearsHomeschooled$HOME11 == 2 & YearsHomeschooled$HOME10 == 0] <- 0
# create vector with "TRUE" if there are any 2s and "FALSE" if there aren't
returners <- apply(YearsHomeschooled, 1, function(r) any(r %in% c(2)))
# create new column with 1s for returners, 0s for not returners
HOME$returner <- ifelse(returners == TRUE, 1, 0)

HOME$sibHS <- ifelse(HOME$CHENRL1 == 1 & !is.na(HOME$CHENRL1), 1,
                     ifelse(HOME$CHENRL2 == 1 & !is.na(HOME$CHENRL2), 1,
                            ifelse(HOME$CHENRL3 == 1 & !is.na(HOME$CHENRL3), 1, 
                                   ifelse(HOME$CHENRL4 == 1 & !is.na(HOME$CHENRL4), 1, 0))))

HOME$sibENRL <- ifelse(HOME$CHENRL1 == 2 & !is.na(HOME$CHENRL1), 1,
                       ifelse(HOME$CHENRL2 == 2 & !is.na(HOME$CHENRL2), 1,
                              ifelse(HOME$CHENRL3 == 2 & !is.na(HOME$CHENRL3), 1, 
                                     ifelse(HOME$CHENRL4 == 2 & !is.na(HOME$CHENRL4), 1, 0))))

HOME$sibBOTH <- ifelse(HOME$sibHS == 1 & HOME$sibENRL == 1, 1, 0)

# CREATE COLUMN WITH TOTAL NUMBER OF REASONS for homeschooling
# this correctly turned 2s to 0s in rows affiliated with reasons
HOME[, 71:79][HOME[, 71:79] == 2] <- 0 
# this correctly adds the reasons for homeschooling together
HOME$numberR <- rowSums(HOME[, 71:79])
# change the 0s back to 2s so as not to break any code
HOME[, 71:79][HOME[, 71:79] == 0] <- 2 

# HAVE CREATED ALL NEW VALUES AT THIS POINT
# if need to create new values, add here, before creating design object!

# CREATE SURVEY PACKAGE DESIGN OBJECT

HOMEdesign <- svrepdesign(
  data = HOME, 
  repweights = subset(HOME, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(HOMEdesign)

# END HOME DATA SET DESIGN CREATION

# MERGE DATA SETS for certain functions

# COMBINED <- merge(PFI, HOME, all = TRUE, sort = TRUE)

COMBINEDdesign <- svrepdesign(
  data = COMBINED, 
  repweights = subset(COMBINED, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(COMBINEDdesign)

# END DATA SET MERGER CREATION