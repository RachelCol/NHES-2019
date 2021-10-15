# Reasons for Homeschooling
# Overall, and by school level (elementary v. secondary)

EL_HS <- subset(HOME, SCHTYPE == 3 & elementary_secondary == 1)
SEC_HS <- subset(HOME, SCHTYPE == 3 & elementary_secondary == 2)

#New homedesign objects

HOMEdesign <- update(HOMEdesign,  rel_no = ifelse(HSRELGON==1, "rel", "no"))


# Reasons for homeschooling

# HSSAFETYX 
# You are concerned about the school environment, such as 
# safety, drugs, or negative peer pressure?

# All homeschooled students
wpct(HOME$HSSAFETYX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSSAFETYX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSSAFETYX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSSAFETYX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISSATX
# You are dissatisfied with the academic instruction 
# at other schools?

# All homeschooled students
wpct(HOME$HSDISSATX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSDISSATX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSDISSATX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSDISSATX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSRELGON
# You prefer to teach this child at home so that you can 
# provide religious instruction?

# All homeschooled students
wpct(HOME$HSRELGON, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSRELGON, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSRELGON, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSRELGON == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSMORAL
# You prefer to teach this child at home so that you can 
# provide moral instruction?

# All homeschooled students
wpct(HOME$HSMORAL, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students

wpct(EL_HS$HSMORAL, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSMORAL, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSMORAL == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ALL DISABILITIES
# Combines HSDISABLX and HSILLX and HSSPCLNDX

# All homeschooled students
wpct(HOME$disability, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$disability, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$disability, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSDISABLX
# This child has a physical or mental health problem 
# that has lasted six months or more?

# All homeschooled students
wpct(HOME$HSDISABLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSDISABLX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSDISABLX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSDISABLX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSILLX
# This child has a temporary illness that prevents him or her 
# from going to school?

# All homeschooled students
wpct(HOME$HSILLX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSILLX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSILLX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSILLX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSSPCLNDX
# This child has other special needs that you feel the school 
# can’t or won’t meet? 

# All homeschooled students
wpct(HOME$HSSPCLNDX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSSPCLNDX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSSPCLNDX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSSPCLNDX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSALTX
# You are interested in a nontraditional approach 
# to children’s education?

# All homeschooled students
wpct(HOME$HSALTX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSALTX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSALTX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSALTX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSFMLY
# You want to emphasize family life together?

# All homeschooled students
wpct(HOME$HSFMLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSFMLY, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSFMLY, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSFMLY == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSOTHERX
# You have another reason for homeschooling your child? 

# All homeschooled students
wpct(HOME$HSOTHERX, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSOTHERX, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSOTHERX, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSOTHERX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# HSBULLY
# This child was bullied at school. 

# All homeschooled students
wpct(HOME$HSBULLY, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$HSBULLY, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$HSBULLY, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((HSBULLY == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)



# Most important reason: HSMOSTX

# All homeschooled students
round(wpct(HOME$HSMOSTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# Elementary v. secondary homeschooled students
round(wpct(EL_HS$HSMOSTX, weight=EL_HS$FPWT, na.rm=TRUE), digits = 3)
round(wpct(SEC_HS$HSMOSTX, weight=SEC_HS$FPWT, na.rm=TRUE), digits = 3)

svyttest((HSMOSTX == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 2) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 3) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 4) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 5) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 6) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 7) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 8) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 9) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 10) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((HSMOSTX == 11) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# COMBINE SPECIAL NEEDS: 
# I created the column PFI$disability in the Data Subsets document
# to combine HSDISABLX, HSILLX, and HSSPCLNDX.

# All homeschooled students
wpct(HOME$disability, weight=HOME$FPWT, na.rm=TRUE)

# Elementary v. secondary homeschooled students
wpct(EL_HS$disability, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$disability, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((disability == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)


# Average number of reasons selected for homeschooling
# question: how much do religious and moral reasons for homeschooling overlap?

# average number of reasons selected
sum(HOME[, 71:80] == 1)/sum(HOME$countn == 1)

# excluding "other"
sum(HOME[, 71:79] == 1)/sum(HOME$countn == 1)

# Average number of reasons, by subset
sum(HOME[, 71:80] == 1 & HOME$HSRELGON == 1)/sum(HOME$countn == 1 & HOME$HSRELGON == 1)
sum(HOME[, 71:80] == 1 & HOME$HSRELGON != 1)/sum(HOME$countn == 1 & HOME$HSRELGON != 1)
sum(HOME[, 71:80] == 1 & HOME$HSMORAL == 1)/sum(HOME$countn == 1 & HOME$HSMORAL == 1)
sum(HOME[, 71:80] == 1 & HOME$FIRST == 1)/sum(HOME$countn == 1 & HOME$FIRST == 1)
sum(HOME[, 71:80] == 1 & HOME$FIRST == 0 & HOME$ALWAYS == 0)/sum(HOME$countn == 1 & HOME$FIRST == 0 & HOME$ALWAYS == 0)
sum(HOME[, 71:80] == 1 & HOME$ALWAYS == 1)/sum(HOME$countn == 1 & HOME$ALWAYS == 1)
sum(HOME[, 71:80] == 1 & HOME$disability == 1)/sum(HOME$countn == 1 & HOME$disability == 1)
sum(HOME[, 71:80] == 1 & HOME$ba_no_ba == 1)/sum(HOME$countn == 1 & HOME$ba_no_ba == 1)
sum(HOME[, 71:80] == 1 & HOME$poverty == 1)/sum(HOME$countn == 1 & HOME$poverty == 1)
sum(HOME[, 71:80] == 1 & HOME$poverty == 3)/sum(HOME$countn == 1 & HOME$poverty == 3)
sum(HOME[, 71:80] == 1 & HOME$HSALTX == 1)/sum(HOME$countn == 1 & HOME$HSALTX == 1)
sum(HOME[, 71:80] == 1 & HOME$HSALTX != 1)/sum(HOME$countn == 1 & HOME$HSALTX != 1)
sum(HOME[, 71:80] == 1 & HOME$elementary_secondary == 1)/sum(HOME$countn == 1 & HOME$elementary_secondary == 1)
sum(HOME[, 71:80] == 1 & HOME$elementary_secondary != 1)/sum(HOME$countn == 1 & HOME$elementary_secondary != 1)

# excluding "other"
sum(HOME[, 71:79] == 1 & HOME$HSRELGON == 1)/sum(HOME$countn == 1 & HOME$HSRELGON == 1)
sum(HOME[, 71:79] == 1 & HOME$HSRELGON != 1)/sum(HOME$countn == 1 & HOME$HSRELGON != 1)



# WHY do religious homeschoolers select more reasons?
svymean(~numberR, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)

svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1 & HSRELGON == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1 & HSRELGON == 2), na.rm=TRUE)

svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary != 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1 & HSRELGON == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary != 1 & HSRELGON == 2), na.rm=TRUE)

svymean(~numberR, subset(HOMEdesign, two_parent_or_single == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, two_parent_or_single != 1), na.rm=TRUE)


# create a chart of the number of reasons, religious homeschoolers
number <- subset(HOME, HSRELGON==1)
counts <- (round(wpct(number$numberR, weight=number$FPWT, na.rm=TRUE), digits = 3)*100)
barplot(counts, main="Number of Reasons for Homeschooling \n (parents with religious reasons for homeschooling)", ylab="percent of respondents")

# create a chart of the number of reasons, NONreligious homeschoolers
number <- subset(HOME, HSRELGON!=1)
counts <- (round(wpct(number$numberR, weight=number$FPWT, na.rm=TRUE), digits = 3)*100)
barplot(counts, main="Number of Reasons for Homeschooling \n (parents without religious reasons for homeschooling)", ylab="percent of respondents")

model <- lm((numberR) ~ 
              (elementary_secondary==2) + 
              (ba_no_ba!=1) +
              (HSRELGON == 0), 
            data=HOME)
summary(model)

# comparisons between religious and nonreligious homeschoolers
svymean(~SES==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~SES==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((SES==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)
svymean(~SES==2, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~SES==2, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((SES==2) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)
svymean(~SES==3, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~SES==3, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((SES==3) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSSAFETYX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSSAFETYX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSSAFETYX==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSMORAL==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSMORAL==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSMORAL==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~disability==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((disability==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSALTX==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSALTX==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSALTX==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSFMLY==1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSFMLY==1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSFMLY==1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# what reasons do homeschoolers who only choose 1 or 2 reasons give?
svymean(~HSFMLY==1, subset(HOMEdesign, numberR == 1), na.rm=TRUE)

numberR1 <- subset(HOME, numberR==1)
wpct(numberR1$HSSAFETYX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSDISSATX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSRELGON, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$disability, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSALTX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSFMLY, weight=numberR1$FPWT, na.rm=TRUE)

# ACADEMIC REASONS FOR HOMESCHOOLING by demographic

svymean(~HSDISSATX==1, subset(HOMEdesign, ba_no_ba == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, ba_no_ba == 2), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ ba_no_ba, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  poor_else = ifelse(poverty==1, "poor", "else"))
svyttest((HSDISSATX==1) ~ poor_else, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSDISSATX==1, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)
svyttest((HSDISSATX==1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# poor non college educated secondary homeschoolers' academic motivations
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSDISSATX==1, subset(HOMEdesign, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  desp_else = ifelse(poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2, "desp", "else"))
svyttest((HSDISSATX==1) ~ desp_else, 
         HOMEdesign,
         na.rm=TRUE)

# What ARE these parents' motivations?
svymean(~HSSAFETYX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMORAL==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSALTX==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSFMLY==1, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)

desp <- subset(HOME, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2)
round(wpct(desp$HSMOSTX, weight=desp$FPWT, na.rm=TRUE), digits = 3)

notdesp <- subset(HOME, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1)
round(wpct(notdesp$HSMOSTX, weight=notdesp$FPWT, na.rm=TRUE), digits = 3)

svymean(~HSMOSTX==4, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSMOSTX==4, subset(HOMEdesign, poverty != 1 | elementary_secondary == 1 | ba_no_ba == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  desp_else = ifelse(poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2, "desp", "else"))
svyttest((HSMOSTX==4) ~ desp_else, 
         HOMEdesign,
         na.rm=TRUE)



svytable(~HSMOSTX, subset(HOMEdesign, poverty == 1 & elementary_secondary == 2 & ba_no_ba == 2))


svymean(~TOTAL, subset(HOMEdesign, HSRELGON==1))
svymean(~TOTAL, subset(HOMEdesign, HSRELGON!=1))

svymean(~TOTAL > 3, subset(HOMEdesign, HSRELGON==1))
svymean(~TOTAL > 3, subset(HOMEdesign, HSRELGON!=1))

svymean(~TOTAL, subset(HOMEdesign, disability == 1))
svymean(~TOTAL, subset(HOMEdesign, disability != 1))

svymean(~disability == 1, subset(HOMEdesign, FIRST == 1))
svymean(~disability == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~disability == 1, subset(HOMEdesign, TOTAL > 3))

svymean(~disability==1, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~disability==1, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)
svymean(~elementary_secondary == 1, subset(HOMEdesign, disability==1), na.rm=TRUE)
svymean(~elementary_secondary == 2, subset(HOMEdesign, disability==1), na.rm=TRUE)

svymean(~white_nonwhite == 1, subset(HOMEdesign, disability==1), na.rm=TRUE)
svymean(~elementary_secondary == 2, subset(HOMEdesign, disability==1), na.rm=TRUE)


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


svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON==1 & TOTAL == 1))
svymean(~disability == 1, subset(HOMEdesign, HSRELGON!=1 & TOTAL > 3))

svymean(~poverty < 3, subset(HOMEdesign, HSRELGON!=1 & TOTAL > 3))
svymean(~white_nonwhite == 1, subset(HOMEdesign, HSRELGON!=1 & TOTAL > 3))



# What percent of religious people selected moral reasons for homeschooling
svymean(~HSMORAL == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)


# DISABILITY -- analyzing parents homeschooling due to a disability

# Homeschooling due to disability, compared with length of time homeschooling
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, FIRST == 1), na.rm=TRUE)
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, FIRST == 0 & ALWAYS == 0), na.rm=TRUE)
svymean(~HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)

HOMEdesign <- update(HOMEdesign,  first_always = ifelse(FIRST==1, "first", ifelse(ALWAYS==1, "always", NA)))
HOMEdesign <- update(HOMEdesign,  first_some = ifelse(FIRST==1, "first", ifelse(ALWAYS==0 & FIRST==0, "some", NA)))
HOMEdesign <- update(HOMEdesign,  always_some = ifelse(ALWAYS==1, "always", ifelse(ALWAYS==0 & FIRST==0, "some", NA)))

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ first_some, 
         HOMEdesign,
         na.rm=TRUE)

svyttest((HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) ~ always_some, 
         HOMEdesign,
         na.rm=TRUE)

# Disabilities, by whether parents have religious reasons for homeschooling
svymean(~disability == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

svyttest((disability == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# CREATE a table showing what percent also selected each thing

# note: this section needs to be rerun to create the items in the rbind below
# category <- HOME$HSOTHERX
# saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# oth <- as.data.frame(svymean(~HSOTHERX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
# Other <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1], oth[2,1]), digits = 3)

# create table using items created above (have to run the above many times)
# ReasonsChart <- rbind(Safety, Academics, Religion, Moral, Disability, AltEd, Family, Other)
# ReasonsPercent <- ReasonsChart*100

# write.csv(ReasonsPercent,"/Users/Rachel/R-Projects/NHES-2019/ReasonsPercent.csv", row.names = TRUE)
# colnames(ReasonsPercent) <- c("Safety", "Academics", "Religion", "Moral", "Disability", "AltEd", "Family", "Other")

# END TABLE on what percent also selected other options


# 1. RELIGIOUS REASONS. Who are these kids? How do reasons overlap?

# create comparison object (those who did or did not select this reason)
HOMEdesign <- update(HOMEdesign,  rel_no = ifelse(HSRELGON==1, "rel", "no"))
PFIdesign <- update(PFIdesign,  rel_ps = ifelse(HSRELGON==1 & SCHTYPE == 3, "rel", 
                                                ifelse(SCHTYPE == 1, "ps", NA)))

# do they have siblings in school?
svymean(~sibENRL, HOMEdesign)

svymean(~sibHS, subset(HOMEdesign, HSRELGON==1))
svymean(~sibENRL, subset(HOMEdesign, HSRELGON==1))

svymean(~sibHS, subset(HOMEdesign, HSMOSTX==3))
svymean(~sibENRL, subset(HOMEdesign, HSMOSTX==3))

# how many use curriculum from religious publishers? 
svymean(~HSINTREL==1, subset(HOMEdesign, HSRELGON==1), na.rm=TRUE)
svymean(~HSINTREL==1, subset(HOMEdesign, HSRELGON!=1), na.rm=TRUE)

# do they have more siblings?
svymean(~NUMSIBSX, subset(HOMEdesign, HSRELGON==1))
svymean(~NUMSIBSX, subset(HOMEdesign, HSRELGON!=1))

# percent of elementary or secondary homeschoolers that are religious
svymean(~HSRELGON==1, subset(HOMEdesign, elementary_secondary==1))
svymean(~HSRELGON==1, subset(HOMEdesign, elementary_secondary==2))





# demo: RACE: what percent of kids who select this reason are white?
svymean(~white_nonwhite == 1, subset(PFIdesign, SCHTYPE == 1), na.rm=TRUE) # public school
svymean(~white_nonwhite == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~white_nonwhite == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((white_nonwhite == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# demo: POVERTY: what percent of kids who select this reason are low income?
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((poverty < 3) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# demo: COLLEGE: what percent who select this reason have college parent?
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(PFIdesign, SCHTYPE == 1), na.rm=TRUE) # public school
svyttest((ba_no_ba == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ rel_ps, 
         PFIdesign,
         na.rm=TRUE)

# demo: LENGTH: always v. first year homeschoolers
svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((ALWAYS == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)
svymean(~FIRST == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~FIRST == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((FIRST == 1) ~ rel_no, 
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


# demo: POVERTY: what percent of kids who select this reason are low income?
# WHITE CHILDREN only
svymean(~poverty < 3, subset(PFIdesign, white_nonwhite == 1 & SCHTYPE == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, white_nonwhite == 1 & HSRELGON == 1), na.rm=TRUE)
svyttest((poverty < 3) ~ whitehsrel_whiteps, 
         PFIdesign,
         na.rm=TRUE)

# demo: COLLEGE: what percent who select this reason have college parent?
# WHITE CHILDREN only
svymean(~ba_no_ba == 1, subset(PFIdesign, white_nonwhite == 1 & SCHTYPE == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 1), na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ whitehsrel_whiteps, 
         PFIdesign,
         na.rm=TRUE)

# demo: POVERTY: what percent of kids who select this reason are low income?
# NONWHITE CHILDREN only
svymean(~poverty < 3, subset(PFIdesign, white_nonwhite == 2 & SCHTYPE == 1), na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 2), na.rm=TRUE)
svyttest((poverty < 3) ~ nonwhitehsrel_nonwhiteps, 
         PFIdesign,
         na.rm=TRUE)

# demo: COLLEGE: what percent who select this reason have college parent?
# NONWHITE CHILDREN only
svymean(~ba_no_ba == 1, subset(PFIdesign, white_nonwhite == 2 & SCHTYPE == 1), na.rm=TRUE)
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON == 1 & white_nonwhite == 2), na.rm=TRUE)
svyttest((ba_no_ba == 1) ~ nonwhitehsrel_nonwhiteps, 
         PFIdesign,
         na.rm=TRUE)

# correcting for race
# white public schoolers: 54.2% have a BA
# white religious homeschoolers: 61.4% have a BA
# black public schoolers: 33.7% have a BA
# black religious homeschoolers: 60.1% have a BA
# religious homeschoolers are 74.3% white
54.2*(.743) + 33.7*(.337)


# back to the rest of the analysis

# demo: LEVEL: elemetnary school v secondary school
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

# Likelihood of also choosing X other reason 
svymean(~disability == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~disability == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((disability == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# Likelihood of also choosing X other reason 
svymean(~HSDISSATX == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSDISSATX == 1, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)
svyttest((HSDISSATX == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# answering more questions...

# what percent of first-year hsers have religious reason (excluding K)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, FIRST == 1 & ALLGRADEX > 0), na.rm=TRUE)
# what percent of always-hsers have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, first_always = ifelse(FIRST==1, "first",
                                                       ifelse(ALWAYS==1, "always", NA)))
svyttest((HSRELGON == 1) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSRELGON == 1, 
        subset(HOMEdesign, FIRST == 0 & ALWAYS == 0), na.rm=TRUE)


# kindergartner, what percent are hsed for religious reasons?
svymean(~HSRELGON == 1, subset(HOMEdesign, 
                               ALLGRADEX == 0), na.rm=TRUE)
svymean(~HSRELGON == 1, subset(HOMEdesign, 
                               ALLGRADEX != 0), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, k_no = ifelse(ALLGRADEX==0, "k", "no"))
svyttest((HSRELGON == 1) ~ k_no, 
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

# What percent of really desperate parents have religious reasons?
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, elementary_secondary == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, elementary_secondary == 2 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)

svymean(~HSRELGON == 1, 
        subset(HOMEdesign, disability == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)

# really desperate hsers (first year, poverty, no college, secondary grades)
# comparing white and nonwhite
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 1 & elementary_secondary == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 2 & elementary_secondary == 1 & FIRST == 1 & poverty == 1 & ba_no_ba == 2), na.rm=TRUE)

# religious reasons by race
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 1), na.rm=TRUE)
svymean(~HSRELGON == 1, 
        subset(HOMEdesign, white_nonwhite == 2), na.rm=TRUE)

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
svymean(~TOTAL, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~TOTAL, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

svymean(~TOTAL, HOMEdesign, na.rm=TRUE)

svymean(~TOTAL, subset(HOMEdesign, HSRELGON == 1 & elementary_secondary == 2), na.rm=TRUE)
svymean(~TOTAL, subset(HOMEdesign, HSRELGON != 1 & elementary_secondary == 2), na.rm=TRUE)

# do religious homeschoolers who homeschool in secondary school
# have different demographic factors?
svymean(~poverty == 3, subset(HOMEdesign, 
                               elementary_secondary == 1 & ALWAYS == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~poverty == 3, subset(HOMEdesign, 
                               elementary_secondary == 2 & ALWAYS == 1 & HSRELGON == 1), na.rm=TRUE)
svyttest((HSRELGON == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)




# white children and college degrees, religion
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

# nonwhite children and college degrees, religion
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


# END ANALYSIS of religious homeschoolers




# NONTRADITIONAL/ALTERNATIVE EDUCATION
# USED IN THE STUDY

table(HOME$HSALTX, HOME$HSSTYL)

ALTX <- subset(HOME, HSALTX == 1)
NOALTX <- subset(HOME, HSALTX == 2)

round(wpct(ALTX$HSSTYL, weight=ALTX$FPWT, na.rm=TRUE), digits = 3)
round(wpct(NOALTX$HSSTYL, weight=NOALTX$FPWT, na.rm=TRUE), digits = 3)

# how many that selected HSALTX use informal curriculum
svymean(~HSSTYL > 2, subset(HOMEdesign, HSALTX == 1), na.rm=TRUE)
svymean(~HSSTYL > 2, subset(HOMEdesign, HSALTX != 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, alt_no = ifelse(HSALTX == 1, "alt", "noM"))
svyttest((HSSTYL > 2) ~ alt_no, 
         HOMEdesign,
         na.rm=TRUE)

# how many that selected HSALTX as MOST IMPORTANT use informal curriculum
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX == 8), na.rm=TRUE)
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX != 8), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, altM_noM = ifelse(HSMOSTX==8, "altM", "noM"))
svyttest((HSSTYL > 2) ~ altM_noM, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSSTYL > 2, HOMEdesign, na.rm=TRUE)

# question: are religious hsers more or less likely to be alted?
svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON == 2), na.rm=TRUE)

svyttest((HSALTX == 1) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

svymean(~HSSTYL > 2, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~HSSTYL > 2, subset(HOMEdesign, HSRELGON == 2), na.rm=TRUE)

svyttest((HSSTYL > 2) ~ rel_no, 
         HOMEdesign,
         na.rm=TRUE)

# overall, informal curriculum use
svymean(~HSSTYL > 2, HOMEdesign, na.rm=TRUE)
svymean(~HSSTYL == 4, HOMEdesign, na.rm=TRUE)

svymean(~ba_no_ba==1, subset(HOMEdesign, HSSTYL > 2), na.rm=TRUE)
svymean(~HSRELGON==1, subset(HOMEdesign, HSSTYL > 2), na.rm=TRUE)

# this one IS statistically significant
# parents who said religion is most important: informal curriculum
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX == 3), na.rm=TRUE)
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX != 3), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign, relM_noM = ifelse(HSMOSTX==3, "relM", "noM"))
svyttest((HSSTYL > 2) ~ relM_noM, 
         HOMEdesign,
         na.rm=TRUE)

# those who chose it as their most important reason
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX == 8), na.rm=TRUE)
svymean(~HSSTYL > 2, subset(HOMEdesign, HSMOSTX != 8), na.rm=TRUE)

# testing demographics of double alt-ed people (reason & style)
svymean(~ba_no_ba ==1, subset(HOMEdesign, HSMOSTX == 8 & HSSTYL > 2), na.rm=TRUE)
svymean(~ba_no_ba ==1, HOMEdesign, na.rm=TRUE)
svymean(~white_nonwhite ==1, subset(HOMEdesign, HSMOSTX == 8 & HSSTYL > 2), na.rm=TRUE)
svymean(~white_nonwhite ==1, HOMEdesign, na.rm=TRUE)
svymean(~poverty < 3, subset(HOMEdesign, HSMOSTX == 8 & HSSTYL > 2), na.rm=TRUE)
svymean(~poverty < 3, HOMEdesign, na.rm=TRUE)
svymean(~HSRELGON == 1, subset(HOMEdesign, HSMOSTX == 8 & HSSTYL > 2), na.rm=TRUE)

table(HOME$HSMOSTX == 8, HOME$HSSTYL > 2)

# end analysis of nontraditional/alternative education





# Nonwhite homeschoolers with religious motivations

svymean(~ba_no_ba == 1, subset(HOMEdesign, white_nonwhite == 2 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 1 & white_nonwhite == 2 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 2 & white_nonwhite == 2 & HSRELGON == 1))

svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 1 & white_nonwhite == 1 & HSRELGON == 1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, elementary_secondary == 2 & white_nonwhite == 1 & HSRELGON == 1))

svymean(~HSRELGON == 1, subset(HOMEdesign, elementary_secondary == 1 & white_nonwhite == 2))


svymean(~ba_no_ba == 1, subset(HOMEdesign, white_nonwhite == 2))


svymean(~HSSTYL > 2, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSSTYL > 2, subset(HOMEdesign, ba_no_ba == 2))


# what do we know about nonreligious homeschoolers with a BA
# who use a mostly or wholly informal curriculum?

svymean(~HSDISSATX == 1, subset(HOMEdesign, HSRELGON == 2 & ba_no_ba == 1 & HSSTYL > 2))



# what about HSMOSTX parents?
svymean(~HSSTYL > 3, 
        subset(HOMEdesign, HSMOSTX == 8), na.rm=TRUE)





# END Reasons for Homeschooling