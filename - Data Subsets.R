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
                   ifelse((PFI$P1HRSWK >= 0), 2, 3))
PFI$full_part2 <- ifelse((PFI$P2HRSWK >= 40), 1, 
                   ifelse((PFI$P2HRSWK >= 20), 2, 3))
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

# Create a column for household poverty status 
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

# May want to add a column comparing virtual v. non-virtual

# For general PFI data file, input details into survey package
PFIdesign <- svrepdesign(
  data = PFI, 
  repweights = subset(PFI, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(PFIdesign)

# END creation of PFI data set and PFIdesign object