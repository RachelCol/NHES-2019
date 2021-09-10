# DETERMINING WHICH STUEDENTS ARE HOMESCHOOLED

# Load file, name file
load(file = "pfi_pu_pert.RData")
PFIcheck <- pfi_pu_pert

# Number of respondents who selected "h" (homeschool) in Q2 (number = 532)
sum(PFIcheck$EDCHSFL == 1)

# Number of respondents who selected "e" (virtual school) on Q2 (number = 300)
sum(PFIcheck$EDCINTK12 == 1)

# Number of respondents who selected both "h" and "e" (number = 68)
sum(PFIcheck$EDCHSFL == 1 & PFIcheck$EDCINTK12 == 1)

# Number of respondents who selected "Yes" in Q3 (number = 532)
sum(PFIcheck$HOMESCHLX == 1) + 
  sum(PFIcheck$HOMESCHLX == 2)
# note: have to ask this way bc there is no data label to Q3

# Number of respondents who selected "Yes" to Q4 (number = 519)
sum(PFIcheck$HOMESCHLX == 1)

# Number of respondents who selected "No" to Q4 (number = 13)
sum(PFIcheck$HOMESCHLX == 2)

# Responses to Q5
sum(PFIcheck$HMSCHARR == 1) # full-time homeschool (number = 472)
sum(PFIcheck$HMSCHARR == 2) # part-time homeschool (number = 47)
sum(PFIcheck$HMSCHARR == 3) # not homeschool (number = 0)

# Responses to Q29, is this child also enrolled in a school
sum(PFIcheck$HSENRL == 1) # yes, 138
sum(PFIcheck$HSENRL == 2) # no, 381

# Number of respondents who indicated homeschooled above, but stated in Q42
# that their child attend school over 24hrs per week (number = 23)
sum(ifelse(
  (PFIcheck$HOMESCHLX == 1 & 
     (PFIcheck$SCHLHRSWK == 4)), 
  1, 0))

# Total after removing those who attend school > 24hrs per wk (number = 496)
sum(PFIcheck$EDCHSFL == 1 & 
      PFIcheck$HOMESCHLX == 1 & 
      (PFIcheck$SCHLHRSWK != 4 | 
         is.na(PFIcheck$SCHLHRSWK)))

# Overlap of final homeschool count with virtual school students
PFIcheck$hs <- ifelse(PFI$EDCHSFL == 1 & PFI$HOMESCHLX == 1 & (
  PFI$SCHLHRSWK != 4 | is.na(PFI$SCHLHRSWK)), 1, 0)
table(PFIcheck$hs, PFIcheck$EDCINTK12)
# 250 respondents are virtual school only; 446 indicated homeschool only; 
# 50 respondents indicated both virtual school and homeschool

# End script