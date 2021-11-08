# DETERMINING WHICH STUDENTS ARE HOMESCHOOLED

# note: This code is designed to run after the 0_data_subsets document

# CREATE data set to use with checking these numbers:
PFIcheck <- original

# -----

# In order to detmerine which children are homeschooled (and prevent the confusion
# that occurred in previous surveys), the NHES:2019 has a series of questions.

# Q2 = "Students today take part in many different types of schools and education 
# settings. What type of school does this child attend?"

# Q3 = "Did you mark Yes to "h. Homeschooled" from the list in question 2 above?"

# Q4 = Some parents decide to educate their children at home rather than send 
# them to a public or private school located in a physical building. Is this child 
# being schooled at home instead of at school for at least some classes or subjects?

# Q5: Which of the following statements best describes your homeschooling
# arrangement for this child? A: full-time, part-time, or not homeschooled

# 1. NUMBER of respondents who selected "homeschool" in Q2
sum(PFIcheck$EDCHSFL == 1) 
# number = 532

# 2. NUMBER of respondents who selected "Yes" in Q3 & Q4
sum(PFIcheck$HOMESCHLX == 1)
# number = 519

# note: there is no code for Q3, so we cannot see how many said "yes"; however,
# Q4 had 519 "yes" responses and 0 "no" responses.

# 3. NUMBER of respondents who selected either "homeschooling full-time" or 
# "homeschooling part-time" in Q5 ("not homeschooled" is also an option)

# sum for each response to Q5
sum(PFIcheck$HMSCHARR == 1) # full-time homeschool (number = 472)
sum(PFIcheck$HMSCHARR == 2) # part-time homeschool (number = 47)
sum(PFIcheck$HMSCHARR == 3) # not homeschool (number = 0)

# number who answered either "full-time" or "part-time" in Q5:
sum(PFIcheck$HMSCHARR == 1 | PFIcheck$HMSCHARR == 2) 
# number = 519

# 4. NUMBER of respondents who indicated "homeschooled" in Q5, and in Q42
# that their child does NOT attend school over 24hrs per week 

# total who said "homeschooled" in Q5, but DID attend school > 24hrs / wk
sum(ifelse(
  (PFIcheck$HOMESCHLX == 1 & 
     (PFIcheck$SCHLHRSWK == 4)), 
  1, 0))
# number = 23

# total homeschooled after removing those who attend school > 24hrs per wk
sum(PFIcheck$EDCHSFL == 1 & 
      PFIcheck$HOMESCHLX == 1 & 
      (PFIcheck$SCHLHRSWK != 4 | 
         is.na(PFIcheck$SCHLHRSWK)))
# number = 496

# 5. Number of respondents coded as homeschooled above, who were ages 5-18
# as of December 31st, 2018, eliminating any aged 3, 4, 19, or 20

# create subset out of students coded as homeschooled, from above
HOMEcheck <- subset(PFIcheck, EDCHSFL == 1 & HOMESCHLX == 1 & 
                      (SCHLHRSWK != 4 | is.na(SCHLHRSWK)))
# table to check ages of students coded as homeschooled
table(HOMEcheck$AGE2018)

# run calculation eliminating any respondents with children under 5 or over 18
sum(PFIcheck$EDCHSFL == 1 & PFIcheck$HOMESCHLX == 1 & 
      (PFIcheck$SCHLHRSWK != 4 | is.na(PFIcheck$SCHLHRSWK)) & 
      PFIcheck$AGE2018 != 3 & PFIcheck$AGE2018 != 4 & 
         PFIcheck$AGE2018 != 19 & PFIcheck$AGE2018 != 20)
# number = 490

# End script