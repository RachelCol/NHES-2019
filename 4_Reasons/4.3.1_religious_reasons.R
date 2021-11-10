# RELIGIOUS REASON HOMESCHOOLERS v. OTHER HOMESCHOOLERS

# note: This script is designed to run after 0_data_subsets script.

# Create comparison subsets:
REL <- subset(HOME, HSRELGON == 1)
NOR <- subset(HOME, HSRELGON != 1)
# Create new comparison objects:
HOMEdesign <- update(HOMEdesign,  rel_no = ifelse(HSRELGON==1, "rel", "no"))
PFIdesign <- update(PFIdesign,  rel_ps = ifelse(HSRELGON==1 & SCHTYPE == 3, "rel", 
                                                ifelse(SCHTYPE == 1, "ps", NA)))

# What percent of religious people selected moral reasons for homeschooling?
svymean(~HSMORAL == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)

# SES: Homeschooling families with religious motivations or no
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)
svyttest((SES==2) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)
svyttest((SES==3) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

# How likely were they to select each other reason for homeschooling?

svymean(~HSSAFETYX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSSAFETYX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSSAFETYX==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

svymean(~HSMORAL==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSMORAL==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSMORAL==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

svymean(~disability==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((disability==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

svymean(~HSALTX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSALTX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSALTX==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

svymean(~HSFMLY==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSFMLY==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSFMLY==1) ~ rel_no, 
         HOMEdesign, na.rm=TRUE)

# How many reasons for homeschooling did they choose?

svymean(~TOTAL, subset(HOMEdesign, HSRELGON==1))
svymean(~TOTAL, subset(HOMEdesign, HSRELGON!=1))

svymean(~TOTAL > 3, subset(HOMEdesign, HSRELGON==1))
svymean(~TOTAL > 3, subset(HOMEdesign, HSRELGON!=1))

# 1. RELIGIOUS REASONS. Who are these kids? How do reasons overlap?

# do they have siblings in school?
svymean(~sibENRL, HOMEdesign)

svymean(~sibHS, subset(HOMEdesign, HSRELGON==1))
svymean(~sibHS, subset(HOMEdesign, HSRELGON!=1))
svymean(~sibHS, subset(HOMEdesign, HSMOSTX==3))

svymean(~sibENRL, subset(HOMEdesign, HSRELGON!=1))
svymean(~sibENRL, subset(HOMEdesign, HSRELGON==1))
svymean(~sibENRL, subset(HOMEdesign, HSMOSTX==3))

# how many use curriculum from religious publishers? 
svymean(~HSINTREL==1, subset(HOMEdesign, HSRELGON==1), na.rm=TRUE)
svymean(~HSINTREL==1, subset(HOMEdesign, HSRELGON!=1), na.rm=TRUE)

# do they have more siblings?
svymean(~NUMSIBSX, subset(HOMEdesign, HSRELGON==1))
svymean(~NUMSIBSX, subset(HOMEdesign, HSRELGON!=1))

# percent of elementary or secondary homeschoolers have religious reasons?
svymean(~HSRELGON==1, subset(HOMEdesign, elementary_secondary==1))
svymean(~HSRELGON==1, subset(HOMEdesign, elementary_secondary==2))

# Demographics of families wiht religious reasons for homeschooling

# RACE: what percent of kids who select this reason are white?
svymean(~white_nonwhite == 1, subset(PFIdesign, SCHTYPE == 1), na.rm=TRUE) # public school
svymean(~white_nonwhite == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~white_nonwhite == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((white_nonwhite == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# POVERTY: what percent of kids who select this reason are low income?
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((poverty < 3) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# COLLEGE: what percent who select this reason have college parent?
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1), na.rm=TRUE) # public school
svyttest((ba_no_ba == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ rel_ps, 
         PFIdesign,
         na.rm=TRUE)

# ALWAYS homeschooling v. homeschool TRANSFERS

# What percent of hsers with religious motivations have always homeschooled? 
svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((ALWAYS == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# What percent of homeschooling families are first year homeschool transfers?
svymean(~FIRST == 1 & ALWAYS != 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~FIRST == 1 & ALWAYS != 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((FIRST == 1 & ALWAYS != 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# What percent of always homeschooling families have religious motivations? 
svymean(~HSRELGON == 1, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)
svymean(~HSRELGON == 1, subset(HOMEdesign, ALWAYS != 1), na.rm=TRUE)
svyttest((HSRELGON == 1) ~ ALWAYS, 
         HOMEdesign,
         na.rm=TRUE)

# Question: Are they higher SES, or just whiter?

# create comparison objects: 
PFIdesign <- update(PFIdesign, whitehsrel_whiteps = 
                      ifelse(white_nonwhite == 1 & SCHTYPE == 1, "whiteps", 
                             ifelse(white_nonwhite == 1 & SCHTYPE == 3 & HSRELGON == 1, "whitehsrel", NA)))
PFIdesign <- update(PFIdesign, nonwhitehsrel_nonwhiteps = 
                      ifelse(white_nonwhite == 2 & SCHTYPE == 1, "nonwhiteps", 
                             ifelse(white_nonwhite == 2 & SCHTYPE == 3 & HSRELGON == 1, "nonwhitehsrel", NA)))

# POVERTY: what percent of kids who select this reason are low income?
# WHITE CHILDREN only
svymean(~poverty < 3, subset(PFIdesign, white_nonwhite == 1 & SCHTYPE == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, white_nonwhite == 1 & HSRELGON == 1), na.rm=TRUE)
svyttest((poverty < 3) ~ whitehsrel_whiteps, 
         PFIdesign,
         na.rm=TRUE)

# COLLEGE: what percent who select this reason have college parent?
# WHITE CHILDREN only
svymean(~ba_no_ba == 1, subset(PFIdesign, white_nonwhite == 1 & SCHTYPE == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 1), na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ whitehsrel_whiteps, 
         PFIdesign,
         na.rm=TRUE)

# POVERTY: what percent of kids who select this reason are low income?
# NONWHITE CHILDREN only
svymean(~poverty < 3, subset(PFIdesign, white_nonwhite == 2 & SCHTYPE == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 2), na.rm=TRUE)
svyttest((poverty < 3) ~ nonwhitehsrel_nonwhiteps, 
         PFIdesign,
         na.rm=TRUE)

# COLLEGE: what percent who select this reason have college parent?
# NONWHITE CHILDREN only
svymean(~ba_no_ba == 1, subset(PFIdesign, white_nonwhite == 2 & SCHTYPE == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 2), na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ nonwhitehsrel_nonwhiteps, 
         PFIdesign,
         na.rm=TRUE)

# LEVEL: elementary school v secondary school
svymean(~elementary_secondary == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~elementary_secondary == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((elementary_secondary == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# Likelihood of also choosing X other reason 

svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSALTX == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~disability == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((disability == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSDISSATX == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSDISSATX == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# answering more questions...

# what percent of first-year hsers have religious reason (excluding K)?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, FIRST == 1 & ALLGRADEX > 0), na.rm=TRUE)
# what percent of always-hsers have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)

# create comparison object, run t-test
HOMEdesign <- update(HOMEdesign, first_always = ifelse(FIRST==1, "first",
                                                       ifelse(ALWAYS==1, "always", NA)))
svyttest((HSRELGON == 1) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)

# kindergarten, what percent are homeschooled for religious reasons?
svymean(~HSRELGON == 1, subset(HOMEdesign, 
                               ALLGRADEX == 0), na.rm=TRUE)
svymean(~HSRELGON == 1, subset(HOMEdesign, 
                               ALLGRADEX != 0), na.rm=TRUE)
svyttest((HSRELGON == 1) ~ (ALLGRADEX == 0), 
         HOMEdesign,
         na.rm=TRUE)

# grades 1-3, what percent have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALLGRADEX == 1 | ALLGRADEX == 2 | ALLGRADEX == 3), na.rm=TRUE)
# grades 4-6, what percent have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALLGRADEX == 4 | ALLGRADEX == 5 | ALLGRADEX == 6), na.rm=TRUE)
# grades 7-9, what percent have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALLGRADEX == 7 | ALLGRADEX == 8 | ALLGRADEX == 9), na.rm=TRUE)
# grades 10-12, what percent have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALLGRADEX == 10 | ALLGRADEX == 11 | ALLGRADEX == 12), na.rm=TRUE)

# What percent of disadvantaged parents have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, elementary_secondary == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, disability == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)

# disadvantaged hsers (first year, poverty, no college, secondary grades)
# comparing white and nonwhite
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 1 & elementary_secondary == 2 & 
                 FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 2 & elementary_secondary == 2 & 
                 FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)

# religious reasons by race
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 1), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, RACEETH == 1), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, RACEETH == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, RACEETH == 3), na.rm=TRUE)

# religious reasons by BA
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ba_no_ba == 1), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ba_no_ba == 2), na.rm=TRUE)

# what percent of religious hsers are in grades K-6 v. 7-12?
svymean(~elementary_secondary == 1, subset(HOMEdesign, 
                                           HSRELGON == 1), na.rm=TRUE)
svymean(~elementary_secondary == 2, subset(HOMEdesign, 
                                           HSRELGON == 1), na.rm=TRUE)
svyttest((HSRELGON == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# average number of years a student has been homeschooled
svymean(~TOTAL, HOMEdesign, na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~TOTAL, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, HSRELGON == 1 & elementary_secondary == 2), na.rm=TRUE)
svymean(~TOTAL, subset(HOMEdesign, HSRELGON != 1 & elementary_secondary == 2), na.rm=TRUE)

# Do religious homeschooling families that homeschool in secondary school
# have different demographic factors?
svymean(~poverty == 3, subset(HOMEdesign, 
                              elementary_secondary == 1 & ALWAYS == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~poverty == 3, subset(HOMEdesign, 
                              elementary_secondary == 2 & ALWAYS == 1 & HSRELGON == 1), na.rm=TRUE)
svyttest((HSRELGON == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# White children and college degrees, religious motivations
svymean(~ba_no_ba == 1, 
        subset(HOMEdesign, white_nonwhite == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, 
        subset(HOMEdesign, white_nonwhite == 1 & HSRELGON != 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, 
        subset(PFIdesign, white_nonwhite == 1 & SCHTYPE == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, 
                     relw_now = ifelse(white_nonwhite==1 & HSRELGON==1, "relw", 
                                       ifelse(white_nonwhite==1 & HSRELGON!=1, "now", NA)))
PFIdesign <- update(PFIdesign, 
                    relhomw_pubw = ifelse(white_nonwhite==1 & HSRELGON==1 & SCHTYPE==3, "relhomw",
                                          ifelse(white_nonwhite==1 & SCHTYPE==1, "pubw", NA)))
PFIdesign <- update(PFIdesign, 
                    nonrelhomw_pubw = ifelse(white_nonwhite==1 & HSRELGON==2 & SCHTYPE==3, "relhomw",
                                             ifelse(white_nonwhite==1 & SCHTYPE==1, "pubw", NA)))
svyttest((ba_no_ba == 1) ~ relw_now, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ relhomw_pubw, 
         PFIdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ nonrelhomw_pubw, 
         PFIdesign,
         na.rm=TRUE)

# Nonwhite children and college degrees, religious motivations
svymean(~ba_no_ba == 1, 
        subset(HOMEdesign, white_nonwhite == 2 & HSRELGON == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, 
        subset(HOMEdesign, white_nonwhite == 2 & HSRELGON != 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, 
        subset(PFIdesign, white_nonwhite == 2 & SCHTYPE == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, 
                     relnw_nonw = ifelse(white_nonwhite!=1 & HSRELGON==1, "relnw", 
                                         ifelse(white_nonwhite!=1 & HSRELGON!=1, "nonw", NA)))
PFIdesign <- update(PFIdesign, 
                    relhomnw_pubnw = ifelse(white_nonwhite!=1 & HSRELGON==1 & SCHTYPE==3, "relhomnw",
                                            ifelse(white_nonwhite!=1 & SCHTYPE==1, "pubnw", NA)))
PFIdesign <- update(PFIdesign, 
                    nonrelhomnw_pubnw = ifelse(white_nonwhite!=1 & HSRELGON==2 & SCHTYPE==3, "relhomnw",
                                               ifelse(white_nonwhite!=1 & SCHTYPE==1, "pubnw", NA)))
svyttest((ba_no_ba == 1) ~ relnw_nonw, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ relhomnw_pubnw, 
         PFIdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ nonrelhomnw_pubnw, 
         PFIdesign,
         na.rm=TRUE)

# College degree and race, homeschooling families with religious motivations
svymean(~ba_no_ba == 1, subset(HOMEdesign, white_nonwhite == 1 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, white_nonwhite == 2 & HSRELGON == 1))

# nonwhite, elementary v secondary
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 1 & 
                                 white_nonwhite == 2 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 2 & 
                                 white_nonwhite == 2 & HSRELGON == 1))
# white, elementary v secondary
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 1 & 
                                 white_nonwhite == 1 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 2 & 
                                 white_nonwhite == 1 & HSRELGON == 1))

# Religious homeschooling and SES 
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

# -----

# CREATE GGPLOT on religious v. non-religious homeschoolers by grade level

# What percent of children hsed for religious reasons are in each grade range?
svymean(~ALLGRADEX<6, subset(HOMEdesign, HSRELGON==1))
svymean(~ALLGRADEX<6, subset(HOMEdesign, HSRELGON!=1))
svymean(~ALLGRADEX==6 | ALLGRADEX==7 | ALLGRADEX == 8, subset(HOMEdesign, HSRELGON==1))
svymean(~ALLGRADEX==6 | ALLGRADEX==7 | ALLGRADEX == 8, subset(HOMEdesign, HSRELGON!=1))
svymean(~ALLGRADEX>8, subset(HOMEdesign, HSRELGON==1))
svymean(~ALLGRADEX>8, subset(HOMEdesign, HSRELGON!=1))

# what percent of each school level has religious motivations?
svymean(~HSRELGON==1, subset(HOMEdesign, ALLGRADEX<6)) # 65.5%
svymean(~HSRELGON!=1, subset(HOMEdesign, ALLGRADEX<6)) # 34.5%
svymean(~HSRELGON==1, subset(HOMEdesign, ALLGRADEX==6 | ALLGRADEX==7 | ALLGRADEX == 8)) # 58.3%
svymean(~HSRELGON!=1, subset(HOMEdesign, ALLGRADEX==6 | ALLGRADEX==7 | ALLGRADEX == 8)) # 41.7#
svymean(~HSRELGON==1, subset(HOMEdesign, ALLGRADEX>8)) # 42.9%
svymean(~HSRELGON!=1, subset(HOMEdesign, ALLGRADEX>8)) # 57.1%

# CREATE data set for chart
grades <- c(rep("Grades K-6" , 2) , rep("Grades 6-8" , 2) , rep("Grades 9-12" , 2) )
motivations <- rep(c("religious reasons" , "no religious reasons") , 3)
percent <- c(65.5, 34.5, 58.3, 41.7, 42.9, 57.1)
data <- data.frame(grades,motivations,percent)

data$grades <- factor(data$grades,levels = c("Grades K-6", "Grades 6-8", "Grades 9-12"))

# Stacked + percent BARPLOT

pdf(file="charts/religious_reasons.pdf")

ggplot(data, aes(fill=motivations, y=percent, x=grades)) + 
  geom_bar(position="fill", stat="identity") + 
  ggtitle("Percent of homeschooling parents who give \n religious reasons for homeschooling") +
  xlab("Children's grade level") + ylab("Percent") + 
  scale_fill_discrete(name = "")

dev.off()

# END chart creation

# -----

# CREATE REGRESSION OBJECT

REL <- HOME
# turn relevant columns into integers
REL$ALWAYS <- as.integer(REL$ALWAYS)
REL$DISABILITY <- as.integer(REL$DISABILITY)
REL$SES <- as.integer(REL$SES)
REL$elementary_secondary <- as.integer(REL$elementary_secondary)
REL$FIRST <- as.integer(REL$FIRST)
REL$PARGRADEX <- as.integer(REL$PARGRADEX)
REL$HSRELGON <- as.integer(REL$HSRELGON)
REL$poverty <- as.integer(REL$poverty)

# CREATE design object from new data set
RELdesign <- svrepdesign(
  data = REL, 
  repweights = subset(REL, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(RELdesign)

# run regressions

summary(svyglm((HSRELGON==1) ~ PARGRADEX + poverty, 
               family=quasibinomial, RELdesign)) # THIS ONE!

summary(svyglm((HSRELGON==1) ~ (PARGRADEX>3) + (poverty==3), 
               family=quasibinomial, RELdesign))

summary(svyglm((HSRELGON==1) ~ PARGRADEX, 
               family=quasibinomial, RELdesign))

summary(svyglm((HSRELGON==1) ~ (PARGRADEX>3), 
               family=quasibinomial, RELdesign))

# create function
invlogit <- function(x) {1/(1+exp(-x))}

# for a regression using PARGRADEX > 3: 
invlogit (-0.15 + 0.96*1) - invlogit (-0.15 + 0.96*0)

# double checking
svymean(~HSRELGON==1, subset(HOMEdesign, PARGRADEX > 3))
svymean(~HSRELGON==1, subset(HOMEdesign, PARGRADEX < 4))

# -----

# ADDITIONAL CALCULATIONS

# College educated parents without religious motivations for homeschooling
# are less likely to be in or near poverty than college educated parents 
# with religious motivations for homeschooling:
svymean(~poverty==3, subset(HOMEdesign, HSRELGON == 1 & PARGRADEX>3))
svymean(~poverty==3, subset(HOMEdesign, HSRELGON != 1 & PARGRADEX>3))

# THIS IS IMPORTANT
svymean(~sahp==1, subset(HOMEdesign, HSRELGON==1 & PARGRADEX>3), na.rm=TRUE)
svymean(~sahp==1, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX>3), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX>3), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX>3), na.rm=TRUE)

svymean(~sahp==1, subset(HOMEdesign, HSRELGON==1 & PARGRADEX<4), na.rm=TRUE)
svymean(~sahp==1, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX<4), na.rm=TRUE)

svymean(~sahp==1, subset(HOMEdesign, HSRELGON==1), na.rm=TRUE)
svymean(~sahp==1, subset(HOMEdesign, HSRELGON!=1), na.rm=TRUE)

svymean(~HSRELGON==1, subset(HOMEdesign, poverty==3), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty!=3), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty==2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty!=2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty==1), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty!=1), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX==1), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX==1), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX==2), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX==2), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX==3), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX==3), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX==4), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX==4), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX==5), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX==5), na.rm=TRUE)

svymean(~poverty!=3, subset(HOMEdesign, HSRELGON==1 & PARGRADEX>3), na.rm=TRUE)
svymean(~poverty!=3, subset(HOMEdesign, HSRELGON!=1 & PARGRADEX>3), na.rm=TRUE)
svymean(~poverty!=3, subset(PFIdesign, SCHTYPE==1 & PARGRADEX>3), na.rm=TRUE)

svymean(~sibENRL==1, HOMEdesign)
svymean(~sibENRL==1, subset(HOMEdesign, HSRELGON==1))
svymean(~sibENRL==1, subset(HOMEdesign, HSRELGON!=1))

svymean(~elementary_secondary==1, subset(HOMEdesign, HSRELGON==1))
svymean(~elementary_secondary==1, subset(HOMEdesign, HSRELGON!=1))

svymean(~PARGRADEX>3, subset(HOMEdesign, HSRELGON==1))
svymean(~PARGRADEX>3, subset(HOMEdesign, HSRELGON!=1))
svymean(~PARGRADEX>3, subset(PFIdesign, SCHTYPE==1))

svymean(~PARGRADEX==1, subset(HOMEdesign, HSRELGON==1))
svymean(~PARGRADEX==1, subset(HOMEdesign, HSRELGON!=1))
svymean(~PARGRADEX==1, subset(PFIdesign, SCHTYPE==1))

svymean(~ALWAYS==1, subset(HOMEdesign, HSRELGON==1 & elementary_secondary==1))
svymean(~ALWAYS==1, subset(HOMEdesign, HSRELGON!=1 & elementary_secondary==1))

svymean(~ALWAYS==1, subset(HOMEdesign, HSRELGON==1 & elementary_secondary==2))
svymean(~ALWAYS==1, subset(HOMEdesign, HSRELGON!=1 & elementary_secondary==2))

# END SCRIPT