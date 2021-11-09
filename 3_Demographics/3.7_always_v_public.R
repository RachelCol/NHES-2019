# ALWAYS HOMESCHOOLING FAMILIES

# THIS SCRIPT ALWAYS: always homeschooling v. all public v. all private

# Question: Are always homeschooling parents more similar to public school
#   families, or private school families?
# Question: Are always homeschooling parents less poor and more educated 
#   because they are white? 

# This document creates statistics to inform these questions.

# -----

# RACE/ETHNICITY
svymean(~RACEETH == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~RACEETH == 1, subset(PFIdesign, SCHTYPE == 1))
svymean(~RACEETH == 1, subset(PFIdesign, SCHTYPE == 2))

svymean(~RACEETH == 2, subset(HOMEdesign, ALWAYS == 1))
svymean(~RACEETH == 2, subset(PFIdesign, SCHTYPE == 1))
svymean(~RACEETH == 2, subset(PFIdesign, SCHTYPE == 2))

svymean(~RACEETH == 3, subset(HOMEdesign, ALWAYS == 1))
svymean(~RACEETH == 3, subset(PFIdesign, SCHTYPE == 1))
svymean(~RACEETH == 3, subset(PFIdesign, SCHTYPE == 2))

# -----

# COLLEGE DEGREE
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2))

# college degree, white children only
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1))

# college degree, Black children only
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 2))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2))

# college degree, Hispanic children only
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 3))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3))

# -----

# POVERTY LEVEL
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2))

# poverty level, white only
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1))

# poverty level, Black only
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 2))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2))

# poverty level, Hispanic only
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 3))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3))

# -----

# AT OR NEAR POVERTY
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2))

# at or near poverty, white only
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1))

# at or near poverty, Black only
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2))

# at or near poverty, Hispanic only
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3))

# -----

# LOW/MIDDLE SES
svymean(~(SES < 3), subset(HOMEdesign, ALWAYS == 1))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 2))

# at or near poverty, white only
svymean(~(SES < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 1))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 1))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 1))

# at or near poverty, Black only
svymean(~(SES < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 2))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 2))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 2))

# at or near poverty, Hispanic only
svymean(~(SES < 3), subset(HOMEdesign, ALWAYS == 1 & RACEETH == 3))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 1 & RACEETH == 3))
svymean(~(SES < 3), subset(PFIdesign, SCHTYPE == 2 & RACEETH == 3))

# -----

# BROKEN DOWN BY ELEMENTARY v. SECONDARY

# COLLEGE DEGREE: homeschool
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 2))
# COLLEGE DEGREE: public school
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))
# COLLEGE DEGREE: private school
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))
# COLLEGE DEGREE: virtual school
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(ba_no_ba == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))

# -----

# POVERTY LEVEL

# IN POVERTY: homeschool
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 1))
svymean(~(poverty == 1), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 2))
# IN POVERTY: public school
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))
# IN POVERTY: private school
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))
# IN POVERTY: virtual school
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(poverty == 1), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))

# AT/NEAR POVERTY: homeschool
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(HOMEdesign, ALWAYS == 1 & elementary_secondary == 2))
# AT/NEAR POVERTY: public school
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary == 2))
# AT/NEAR POVERTY: private school
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 2 & elementary_secondary == 2))
# AT/NEAR POVERTY: virtual school
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 1))
svymean(~(poverty < 3), subset(PFIdesign, SCHTYPE == 4 & elementary_secondary == 2))

# End script