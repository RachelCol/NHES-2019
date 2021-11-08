# HOMESCHOOLED CHILDREN WHO ARE ALSO ENROLLED IN A SCHOOL

# GOAL: Find a way to identify any child who is also enrolled
# in a school, for online or in-person classes.
# -- public school
# -- private school
# -- virtual school (actually enrolled, some or all)
# note: I am leaving college out of this entirely

# -----

# 1. CREATE NEW 'SCHOOL' VARIABLE
# At the beginning of the survey, parents are asked to indicate the type of
# school their child attends in Q2. Some respondents who selected "student is
# homeschooled" also selected public, private, or virtual school.

# Create a new "school_1" variable:
#   1 = also enrolled in public school
#   2 = also enrolled in private school
#   3 = also enrolled in virtual school
# note: This variable will combine all three private school options.

HOME$school_1 <- ifelse(HOME$EDCPUB == 1, 1,
                  ifelse(HOME$EDCCAT == 1, 2,
                  ifelse(HOME$EDCREL == 1, 2,
                  ifelse(HOME$EDCPRI == 1, 2,
                  ifelse(HOME$EDCINTK12 == 1, 3,
                        NA)))))

# note: When creating the 'school_1' variable, we could only assign one value to
# each respondent. Respondents who selected public school were coded as '1',
# even if they also selected private school; respondents who selected a private
# school were coded as '2', regardless of whether they selected virtual school.

# -----

# 2. CREATE NEW 'ENROLLED' VARIABLE
# At the end of the homeschool section, the survey asks whether the child is
# 'also enrolled in a school' (HSENRL). There is overlap between these
# respondents and those who selected a school at the beginning of the survey
# ('school_1'), but this overlap is not complete. 

# Create a new 'enrolled' variable combining 'HSENRL' and 'school_1': 
# 1 = child is also enrolled in a school (any school)
# 0 = child is NOT enrolled in a school

HOME$enrolled <- ifelse(HOME$HSENRL == 1, 1, 
                       ifelse((HOME$school == 1 | HOME$school == 2 | HOME$school == 3), 1, NA))

HOME$enrolled[is.na(HOME$enrolled)] <- 0 # turn NAs into 0s

# -----

# 3. CREATE NEW 'school_2' VARIABLE 
# If respondents answer "yes" to HSENRL (child is also enrolled in a school),
# they are directed to the next section: questions about their child's school. 
# This question includes an opt-out ("child is only homeschooled"). The 
# "school_2" variable includes all who did NOT opt out, by school type.

# The public data files do not include a variable for that question. So...

# Respondents who selected "public school" were told to "GO TO question 31"; 
# the code ID for responses to this question was 'DISTASSI.' Here we create a 
# new variable ("Q31") for all respondents who answered this question. 
HOME$Q31 <- ifelse(HOME$DISTASSI == 1 | HOME$DISTASSI == 2, 1, 2) # 19 answered

# Respondents who selected "virutal school" were told to "GO TO question 32"; 
# the code ID for repsonses to this question was 'SCHRTSCHL.' Here we create a 
# new variable ("Q31") for all respondents who answered this question. 
HOME$Q32 <- ifelse(HOME$SCHRTSCHL == 1 | HOME$SCHRTSCHL == 2, 1, 2) # 31 answered

# Respondents who selected "private school" were told to "GO TO question 34"; 
# the code ID for responses to this question was 'SNEIGHBRX.' Here we create a
# new variable ("Q34") for all respondents who answered this question. 
HOME$Q34 <- ifelse(HOME$SNEIGHBRX == 1 | HOME$SNEIGHBRX == 2, 1, 2) # 44 answered 

# Turn all NAs into 0s (otherwise they gum this up)
HOME$Q31[is.na(HOME$Q31)] <- 0
HOME$Q32[is.na(HOME$Q32)] <- 0
HOME$Q34[is.na(HOME$Q34)] <- 0

# Now we create the 'school_2' variable. To determine which option respondents
# chose, we look at whether they resumed the survey at Q31, Q32, or Q34. 

# Create new 'school_2' variable:
# 1 = child is enrolled in a public school
# 2 = child is enrolled in a private school
# 3 = child is enrolled in a virtual school

HOME$school_2 <- ifelse(HOME$Q31 == 1, 1,
               ifelse(HOME$Q32 == 1 & HOME$Q31 != 1, 3,
               ifelse(HOME$Q34 == 1 & HOME$Q32 != 1, 2, NA)))

# -----

# 4. CREATE NEW "school" VARIABLE WITH SCHOOL TYPE
# Finally, we combine these two variables ('school_1' and 'school_2') to create
# a third variable: 'school' This variable will let us know whether a child
# is also enrolled in a public, private, or virtual school.

# note: This variable is different from 'enrolled.' Some respondents who
# indicated that their child was "also enrolled in a school" selected "this
# child is only homeschooled" when then asked what type of school. The new
# 'school' variable includes only cases where we know the school type.

# start by turning NAs to 0, or they will mess it up
HOME$school_1[is.na(HOME$school_1)] <-- 0
HOME$school_2[is.na(HOME$school_2)] <-- 0

# Create a new "school" variable:
#   1 = child is enrolled in a public school
#   2 = child is enrolled in a private school
#   3 = child is enrolled in a virtual school
HOME$school <- ifelse(HOME$school_1 == 1, 1,
                  ifelse(HOME$school_2 == 1, 1,
                  ifelse(HOME$school_1 == 2, 2,
                  ifelse(HOME$school_2 == 2, 2,
                  ifelse(HOME$school_1 == 3, 3,
                  ifelse(HOME$school_2 == 3, 3,
                     NA))))))

HOME$school[is.na(HOME$school)] = 0

# END variable creation.

# UPDATE the design object to include new variables:

HOMEdesign <- svrepdesign(
   data = HOME,
   repweights = subset(HOME, select = FPWT1:FPWT80),
   weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE,
   scale=79/80)
summary(HOMEdesign)

# END creation of new variables and design object.

# -----

# HOMESCHOOLED CHILDREN ALSO ENROLLED IN A SCHOOL

# PERCENT of homeschooled children also enrolled in a school
svymean(~enrolled == 1, HOMEdesign)

# PERCENT of homeschool respondents who selected a school in Q2:
svymean(~EDCPUB == 1, HOMEdesign)
svymean(~EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1, HOMEdesign)
svymean(~EDCINTK12 == 1, HOMEdesign)
# total, accounting for overlap
round(wpct(HOME$school_1 != 0, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# note: we could have  used 'school_1' for the first three calculations, but as 
# some respondents selected more than one option, it made more sense not to.

# PERCENT of respondents selecting "child is also enrolled in a school"
svymean(~HSENRL == 1, HOMEdesign)

# PERCENT that answered questions about their child's school ('school_2'):
svymean(~school_2 == 1, HOMEdesign)
svymean(~school_2 == 2, HOMEdesign)
svymean(~school_2 == 3, HOMEdesign)
# total (no need to account for overlap, there is none)
round(wpct(HOME$school_2 != 0, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# OF THOSE those who indicated that their child was also enrolled in a school
# (at any point in the survey), what percent indicated what type of school?
svymean(~school > 0, subset(HOMEdesign, enrolled == 1))

# OF THOSE who stated the type of school, what percent selected each?
part <- subset(HOME, school > 0)
round(wpct(part$school, weight=part$FPWT, na.rm=TRUE), digits = 3)

# --- 

# RELATIONSHIP between 'school_1', 'HSENRL', and "school_2'

# note: many respondents selected a school in Q2, but did not later say that
# their child was enrolled; others did not select a school in Q2, but later
# said "yes" to "child is also enrolled in a school." 

# What percent of those who chose a school in Q2 said their child
# was "also enrolled in a school" when asked later?
svymean(~HSENRL == 1, subset(HOMEdesign, school_1 != 0), na.rm=TRUE)

# What percent of "also enrolled in a school" chose a school in Q2?
svymean(~school_1 != 0, subset(HOMEdesign, HSENRL == 1), na.rm=TRUE)

# What percent of those who said their child was "also enrolled in a school"
# answered questions about their child's school?
svymean(~school_2 != 0, subset(HOMEdesign, HSENRL == 1), na.rm=TRUE)

# TYPE OF SCHOOL the child attended
# What kind of school did those who selected a school in Q2 choose?
part <- subset(HOME, school_1 > 0)
round(wpct(part$school_1, weight=part$FPWT, na.rm=TRUE), digits = 3)
# What kind did those who answered questions about their child's school choose?
part <- subset(HOME, school_2 > 0)
round(wpct(part$school_2, weight=part$FPWT, na.rm=TRUE), digits = 3)
# takeaway: those who were enrolled in a virtual school were most likely
# to select a school in Q2, while those who were enrolled in a public school
# were most likely to answer questions about their child's school.

# of those who selected a school at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCPUB == 1))
# of those who said private school at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1))
# of those who said virtual school at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCINTK12 == 1))

# What percent of those who answered the school questions after the "also
# enrolled" question sent their children to each type of school?
attTable <- round(svytable(~school_2 + HSENRL, HOMEdesign))
attTable[2, 1] / sum(attTable[2:4, 1]) # public school
attTable[3, 1] / sum(attTable[2:4, 1]) # private school
attTable[4, 1] / sum(attTable[2:4, 1]) # virtual school

# -----

# ANALYSIS of those who said "child is enrolled" ... 
# but did not answer school questions.

# Note: Most respondents who said their child was "also enrolled in a school" 
# said "child is only homeschooled" when asked Qs about their child's school.
# HOWEVER, this likely has to do with the question design.

# What percent of those who said their child was "also enrolled in a school"
# subsequently said "child is only homeschooled"?
svymean(~school_2 == 0, subset(HOMEdesign, HSENRL == 1), na.rm=TRUE)

# Of those who selected a school at the start AND said yes to "child is also
# enrolled in a school," what % filled out the school section?
svymean(~school_2 != 0, subset(HOMEdesign, HSENRL == 1 & school_1 != 0)) # 100%

# 100% seems a bit unlikely ... let's double check that. 

# table showing school_and school_2, raw count (discard 0)
table(HOME$school_1, HOME$school_2)

# what percent of those who selected a school at the start said "also enrolled"?
svymean(~HSENRL == 1, subset(HOMEdesign, school_1 != 0))
# what % of those who selected a school at the start filled out the school section?
svymean(~school_2 != 0, subset(HOMEdesign, school_1 != 0))

# Conclusion: 100% is right! ALL who selected a school at the beginning of
# the survey AND later said their child was "also enrolled' subsequently
# filled out the section about their child's school.

# Of those who did NOT state a school at the beginning but did say HSENRL,
# what percent filled out the school questions in the next section?
svymean(~school_2 != 0, subset(HOMEdesign, HSENRL == 1 & school_1 == 0)) # 14.7%

# What percent of raw respondents bailed at "homeschool only"?
sum(HOME$HSENRL == 1 & HOME$school_2 == 0) / sum(HOME$HSENRL == 1)

# Out of those who were also enrolled in a school (all combined), how many
# filled out the section on schools?
svymean(~ also != 0, subset(HOMEdesign, all_enrolled == 1))

# What percent of all homeschooling parents filled out the school questions?
svymean(~ also != 0, HOMEdesign)

# -----

# ONLINE COURSES and homeschooled children also enrolled in a school

# WHAT percent of those who selected public or private school at the beginning
# stated in Q9 that their child takes online courses?
# 1. online courses, hs child is or is not enrolled in public school
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCPUB == 1), na.rm=TRUE)
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCPUB != 1), na.rm=TRUE)
# 2. online courses, hs child is or is not enrolled in public school
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1), na.rm=TRUE)
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCCAT != 1 & EDCREL != 1 & EDCPRI != 1), na.rm=TRUE)
# 3. online courses, hs child is or is not enrolled in virtual school
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCINTK12 == 1), na.rm=TRUE)
svymean(~HSINTNET < 4, subset(HOMEdesign, EDCINTK12 != 1), na.rm=TRUE)
# takeaway: in each category, those who selected a school in Q2 are more likely
# to say that their child takes at least some courses online.

# OF THOSE who stated in Q9 that their child takes at least some online courses 
# AND reported in Q2 that their child is enrolled in a school, did the source of
# these courses (reported in Q12) correspond with the type of school (in Q2)?
# 1. public school online courses, hs child is or is not enrolled in public school
svymean(~HSINTPUB == 1, subset(HOMEdesign, EDCPUB == 1), na.rm=TRUE)
svymean(~HSINTPUB == 1, subset(HOMEdesign, EDCPUB != 1), na.rm=TRUE)
# 2. private school online courses, hs child is or is not enrolled in public school
svymean(~HSINTPRI == 1, subset(HOMEdesign, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1), na.rm=TRUE)
svymean(~HSINTPRI == 1, subset(HOMEdesign, EDCCAT != 1 & EDCREL != 1 & EDCPRI != 1), na.rm=TRUE)
# 3. virtual school online courses, hs child is or is not enrolled in virtual school
svymean(~HSINTVRT == 1, subset(HOMEdesign, EDCINTK12 == 1), na.rm=TRUE)
svymean(~HSINTVRT == 1, subset(HOMEdesign, EDCINTK12 != 1), na.rm=TRUE)
# takeaway: the outcome is what we would expect.

# -----

# CHILDREN ENROLLED IN SCHOOL PART-TIME

# What percent of all respondents said child was homeschooled part-time?
svymean(~HMSCHARR == 2, HOMEdesign)

# WHAT PERCENT of each group reported homeschooling part-time?
# of those who selected a school in Q2:
svymean(~HMSCHARR == 2, subset(HOMEdesign, school_1 != 0)) 
# of those who answered "yes" to "also enrolled":
svymean(~HMSCHARR == 2, subset(HOMEdesign, HSENRL == 1))
# of those who answered questions about their child's school:
svymean(~HMSCHARR == 2, subset(HOMEdesign, school_2 != 0))
# of all three of the above combined:
svymean(~HMSCHARR == 2, subset(HOMEdesign, enrolled == 1))
# only those who identified a school (school_1 and school_2 combined):
svymean(~HMSCHARR == 2, subset(HOMEdesign, school == 1))

# THIS IS SUPER INTERESTING!
table(HOME$HSENRL, HOME$HSINTNET)

# -----

# ALSO ENROLLED IN A SCHOOL by demographic and homeschool length subsets

# BY SES: also enrolled (combined)
svymean(~ enrolled == 1, HOMEdesign)
svymean(~ enrolled == 1, subset(HOMEdesign, SES == 1))
svymean(~ enrolled == 1, subset(HOMEdesign, SES == 2))
svymean(~ enrolled == 1, subset(HOMEdesign, SES == 3))
svymean(~ enrolled == 1, subset(HOMEdesign, SES == 2 | SES == 3))
# create a comparison object
HOMEdesign <- update(HOMEdesign,  HighMid_Low = ifelse(SES == 3 | SES == 2, "HighMid", ifelse(SES == 1, "Low", NA)))
# run a t-test
svyttest((enrolled == 1) ~ HighMid_Low,
   HOMEdesign, na.rm=TRUE)

# GRADES K-6 v. 12: also enrolled (combined)
svymean(~ enrolled == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~ enrolled == 1, subset(HOMEdesign, elementary_secondary == 2))
# run a t-test
svyttest((enrolled == 1) ~ elementary_secondary,
   HOMEdesign, na.rm=TRUE)

# ALWAYS v. TRANSFER: also enrolled (combined)
svymean(~ enrolled == 1, subset(HOMEdesign, ALWAYS != 1), na.rm=TRUE)
svymean(~ enrolled == 1, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)
svymean(~ enrolled == 1, subset(HOMEdesign, FIRST == 1 & ALWAYS != 1), na.rm=TRUE)
# create a comparison object
HOMEdesign <- update(HOMEdesign,  always_transf = ifelse(ALWAYS == 1, "always", "transf"))
# run a t-test
svyttest((enrolled == 1) ~ always_transf,
   HOMEdesign, na.rm=TRUE)

# END also enrolled in a school