# HOMESCHOOL RETURNERS

# note: This script is designed to run after 0_data_subsets script.

# note: This script uses a variable, "returner", that was created
# in the 0_data_subsets document specifically for this analysis.

# percent of homeschool returners overall, K-6, and 7-12:
svymean(~returner, HOMEdesign)
svymean(~returner, subset(HOMEdesign, elementary_secondary == 1))
svymean(~returner, subset(HOMEdesign, elementary_secondary == 2))

# percent of homeschool returners by SES:
svymean(~SES == 1, subset(HOMEdesign, returner == 1))
svymean(~SES == 2, subset(HOMEdesign, returner == 1))
svymean(~SES == 3, subset(HOMEdesign, returner == 1))

# END homeschool returner script