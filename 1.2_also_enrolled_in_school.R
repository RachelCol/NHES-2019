# Is the child also enrolled in a school? 
# HSENRL

# note: this code is very messy and needs cleaning up!

# "school" variable: 1, 2, or 3 for those who indicated public, private,
   # or virtual school at the beginning of the survey. 
# "HSENRL" variable: 1 for those who said their child was "also enrolled"
   # in a school. Not all indicated what kind in "school" or "also"
# "also" variable: 1, 2, or 3 for those who indicated public, private, 
   # or virtual school in the "Child's School" section, which comes after
   # the homeschool questions. 
# "enrolled" variable: combination of "school" and "also", leaving out 
   # those in "HSENRL" who did not indicate school type in "school" or "also"

# variables are: school, also, enrolled 
# fix "enrolled" for easier analysis:
which( colnames(HOME)=="enrolled" )
HOME[c(883)][is.na(HOME[c(883)])] <- 0

# percent of respondents selecting "child is also enrolled in a school"
svymean(~HSENRL == 1, HOMEdesign)

# percent of hs respondents selecting public, private, or virtual school
# at the beginning of the survey (0 = none)
svymean(~EDCPUB == 1, HOMEdesign)
svymean(~EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1, HOMEdesign)
svymean(~EDCINTK12 == 1, HOMEdesign)
# total, accounting for overlap
round(wpct(HOME$school != 0, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

svymean(~school == 1, HOMEdesign)
svymean(~school == 2, HOMEdesign)
svymean(~school == 3, HOMEdesign)

table(HOME$school)

# what percent of "also enrolled in a school" chose other school at start
svymean(~school != 0, subset(HOMEdesign, HSENRL == 1), na.rm=TRUE)
svymean(~HSENRL == 1, subset(HOMEdesign, school != 0), na.rm=TRUE)

# for some reason this won't work
svymean(~enrolled == 0, subset(HOMEdesign, HSENRL == 1), na.rm=TRUE)

# however, this does work -- what percent of HSENRL bailed?
enTable <- table(HOME$enrolled, HOME$HSENRL)
enTable[1, 1] / sum(enTable[, 1])

# this is the same number with weighting
enroTable <- round(svytable(~enrolled + HSENRL, HOMEdesign))
enroTable[1, 1] / sum(enroTable[, 1])

svymean(~also == 1, subset(HOMEdesign, EDCPUB == 1))

# WHICH WERE MOST LIKELY TO BAIL

# of public school start ppl at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCPUB == 1))
# of private school start ppl at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1))
# of virtual school start ppl at the start, what % said yes to ENRL?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCINTK12 == 1))

# what percent of those who answered school questions were each
attTable <- round(svytable(~also + HSENRL, HOMEdesign))
attTable[2, 1] / sum(attTable[2:4, 1])
attTable[3, 1] / sum(attTable[2:4, 1])
attTable[4, 1] / sum(attTable[2:4, 1])

# of HSENRL ppl who also answered ATTEND, what % of each option bailed?
# INCLUDE THOSE WHO DIDN'T INDICATE A SCHOOL AT THE START
svymean(~also == 1, subset(HOMEdesign, HSENRL == 1 & school == 1))
svymean(~also == 2, subset(HOMEdesign, HSENRL == 1 & school == 2))
svymean(~also == 3, subset(HOMEdesign, HSENRL == 1 & school == 3))

svymean(~HSENRL == 1, subset(HOMEdesign, school != 0))
svymean(~also != 0, subset(HOMEdesign, school != 0))

# of those who did not state a school at the beginning but did say HSENRL,
# what percent filled out the school questions in the next section?
svymean(~also != 0, subset(HOMEdesign, HSENRL == 1 & school == 0))

# what percent of public and private school homeschoolers from the
# beginning get online courses through that option in the hs section

# public school online courses, hs child enrolled in public school
svymean(~HSINTPUB == 1, subset(HOMEdesign, EDCPUB == 1), na.rm=TRUE)
svymean(~HSINTPUB == 1, subset(HOMEdesign, EDCPUB != 1), na.rm=TRUE)

# private school online courses, hs child enrolled in public school
svymean(~HSINTPRI == 1, subset(HOMEdesign, EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1), na.rm=TRUE)
svymean(~HSINTPRI == 1, subset(HOMEdesign, EDCCAT != 1 & EDCREL != 1 & EDCPRI != 1), na.rm=TRUE)

# are students who took classes more likely to say "not enrolled" later?
svymean(~HSENRL == 1, subset(HOMEdesign, EDCPUB == 1 & HSINTPUB == 1), na.rm=TRUE)
svymean(~HSENRL == 1, subset(HOMEdesign, EDCPUB == 1 & HSINTPUB != 1), na.rm=TRUE)
svymean(~HSENRL == 1, subset(HOMEdesign, HSINTPRI == 1 & (EDCCAT == 1 | EDCREL == 1 | EDCPRI == 1), na.rm=TRUE))
svymean(~HSENRL == 1, subset(HOMEdesign, HSINTPRI == 1 & (EDCCAT != 1 & EDCREL != 1 & EDCPRI != 1), na.rm=TRUE))

# what percent of raw respondents bailed at "homeschool only"? 
sum(HOME$HSENRL == 1 & HOME$also == 0) / sum(HOME$HSENRL == 1)


# IMPORTANT THING: full-time v. part-time homeschooling
# children homeschooled full-time v. part-time
table(HOME$HMSCHARR)
svymean(~HMSCHARR == 2, HOMEdesign)

table(HOME$HMSCHARR, HOME$school)
table(HOME$HMSCHARR, HOME$also)
table(HOME$HMSCHARR, HOME$HSENRL)

svymean(~HMSCHARR == 2, subset(HOMEdesign, school != 0))
svymean(~HMSCHARR == 2, subset(HOMEdesign, HSENRL == 1))
svymean(~HMSCHARR == 2, subset(HOMEdesign, also != 0))

svymean(~HSENRL == 1, subset(HOMEdesign, HMSCHARR == 2))
svymean(~HSENRL == 1, subset(HOMEdesign, HMSCHARR == 2))

round(svytable(~HMSCHARR == 2, subset(HOMEdesign, school == 1 | school == 2)))
round(svytable(~HMSCHARR == 2, subset(HOMEdesign, also == 1 | also == 2)))
round(svytable(~HMSCHARR == 2, subset(HOMEdesign, enrolled == 1 | enrolled == 2)))

svymean(~HMSCHARR == 2, subset(HOMEdesign, HSENRL == 1 & school < 1 & also < 1))

term <- subset(HOME, HSENRL == 1 & school < 1 & also < 1)
round(wpct(term$HMSCHARR, weight=term$FPWT, na.rm=TRUE), digits = 3)

table(HOME$enrolled)
svytable(~enrolled, HOMEdesign)
table(HOME$HMSCHARR, HOME$e)

# all homeschooled children also enrolled in any school at all:
svymean(~HMSCHARR == 2, subset(HOMEdesign, school != 0 | HSENRL == 1))
svymean(~school != 0 | HSENRL == 1, subset(HOMEdesign, HMSCHARR == 2))


# END important thing

svymean(~HSENRL == 1, subset(HOMEdesign, EDCPUB == 1 & HSINTNET == 1), na.rm=TRUE)


round(wpct(HOME$enrolled, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# all of those indicating any sort of school attendance
svymean(~ school != 0, HOMEdesign)
svymean(~ HSENRL == 1, HOMEdesign)

svymean(~ school != 0 | HSENRL == 1, HOMEdesign)

# how many filled out the section on schools?
svytotal(~ also != 0, HOMEdesign)
# how many said at the beginning that their child was enrolled in a school
# OR that their child was "also enrolled in a school"? No double counting!
svytotal(~ school != 0 | HSENRL == 1, HOMEdesign)
# divide the one by the other to find the percent that filled out school Qs
svytotal(~ also != 0, HOMEdesign) / svytotal(~ school != 0 | HSENRL == 1, HOMEdesign)


# what percent of those also enrolled in a school said what KIND of school?
svytotal(~school != 0 | also != 0, HOMEdesign)
# divide the  number that said what kind of school they were in by 
# the number total who said also enrolled in a school
svytotal(~school != 0 | also != 0, HOMEdesign)/ svytotal(~ school != 0 | HSENRL == 1, HOMEdesign)

# what kinds of schools were attended by the ones who DID say?
svymean(~enrolled == 1, HOMEdesign, na.rm=TRUE)
svymean(~enrolled == 2, HOMEdesign, na.rm=TRUE)
svymean(~enrolled == 3, HOMEdesign, na.rm=TRUE)



# compare by SES?
svymean(~ school != 0 | HSENRL == 1, HOMEdesign)
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, SES == 1))
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, SES == 2))
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, SES == 3))
HOMEdesign <- update(HOMEdesign,  high_low = ifelse(SES == 3, "high", ifelse(SES == 1, "low", NA)))
svyttest((school != 0 | HSENRL == 1) ~ high_low, 
         HOMEdesign,
         na.rm=TRUE)

# elementary v. secondary grades
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, elementary_secondary == 2))
svyttest((school != 0 | HSENRL == 1) ~ elementary_secondary, 
         HOMEdesign,
         na.rm=TRUE)

# always homeschooled v. homeschool transfers
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, FIRST == 1 & ALLGRADEX > 0), na.rm=TRUE)
svymean(~ school != 0 | HSENRL == 1, subset(HOMEdesign, ALWAYS == 1), na.rm=TRUE)
HOMEdesign <- update(HOMEdesign,  first_always = ifelse(ALLGRADEX > 0 & FIRST == 1, "first", ifelse(ALWAYS == 1, "always", NA)))
svyttest((school != 0 | HSENRL == 1) ~ first_always, 
         HOMEdesign,
         na.rm=TRUE)




# THIS IS SUPER USEFUL!
table(HOME$HSENRL, HOME$HSINTNET)


# DEFUNCT analysis

table(HOME$HSINTNET, HOME$EINTNET)

table(HOME$SCHLHRSWK)

table(HOME$SEENJOY)


sum(table(HOME$HSINTNET, HOME$SSAMSC))
sum(table(PFI$HSINTNET, PFI$SEENJOY))

sum(table(PFI$HSINTNET, PFI$DISTASSI))
sum(table(PFI$HSINTNET, PFI$SCHRTSCHL))

sum(table(PFI$HSINTNET, PFI$SNEIGHBRX))

sum(table(PFI$HSINTNET, PFI$SCCHOICE))
sum(table(PFI$HSINTNET, PFI$SPUBCHOIX))

sum(table(PFI$HSENRL, PFI$SSAMSC))
table(HOME$HSENRL)


sum(table(HOME$HSINTNET, HOME$DISTASSI))
sum(table(HOME$HSINTNET, HOME$SCHRTSCHL))
sum(table(HOME$HSINTNET, HOME$SNEIGHBRX))




table(HOME$also)



# not sure what this was for!
in_school <- subset(HOME, enrolled == 1 | enrolled == 2 | enrolled == 3)
round(wpct(in_school$enrolled, weight=in_school$FPWT, na.rm=TRUE), digits = 3)

table(HOME$enrolled)
sum(table(HOME$enrolled))

# END also enrolled in a school