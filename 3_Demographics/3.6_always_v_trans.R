# ALWAYS HOMESCHOOLED v. HOMESCHOOL TRANSFERS 
# Demographic differnces, differences in reasons for homeschooling

# note: This script is designed to run after 0_data_subsets script.

ALW <- subset(HOME, ALWAYS == 1)
TRAN <- subset(HOME, ALWAYS != 1)

# -----

# White or minority
wpct(ALW$white_nonwhite, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$white_nonwhite, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((white_nonwhite == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# Bachelor's degree or no
wpct(ALW$ba_no_ba, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$ba_no_ba, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((ba_no_ba == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# Two parent or single parent
wpct(ALW$two_parent_or_single, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$two_parent_or_single, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((two_parent_or_single == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# Stay at home parent or no
wpct(ALW$sahp, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$sahp, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((sahp == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# Income below the poverty line or no
wpct(ALW$poverty, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$poverty, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((poverty == 3) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# English or no
wpct(ALW$english_or_no, weight=ALW$FPWT, na.rm= TRUE)
wpct(TRAN$english_or_no, weight=TRAN$FPWT, na.rm= TRUE)

svyttest((english_or_no == 1) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# -----

# ALWAYS HOMESCHOOLED V TRANSFER HOMESCHOOLED
# DIFFERENCES IN REASONS FOR HOMESCHOOLING

# Religious reasons for homeschooling
svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON == 1))
svymean(~ ALWAYS, subset(HOMEdesign, HSRELGON != 1))

svyttest(ALWAYS ~ (HSRELGON == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~ HSRELGON == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~ HSRELGON == 1, subset(HOMEdesign, ALWAYS != 1))

svyttest(HSRELGON == 1 ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# ALL disability (HSDISABLX, HSILLX, and HSSPCLNDX combined)
svymean(~ ALWAYS, subset(HOMEdesign, disability == 1))
svymean(~ ALWAYS, subset(HOMEdesign, disability != 1))

svyttest(ALWAYS ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~ disability == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~ disability == 1, subset(HOMEdesign, ALWAYS != 1))

svyttest(disability == 1 ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# "To emphasize family life together"
svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY == 1))
svymean(~ ALWAYS, subset(HOMEdesign, HSFMLY != 1))

svyttest(ALWAYS ~ (HSFMLY == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~ HSFMLY == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~ HSFMLY == 1, subset(HOMEdesign, ALWAYS != 1))

svyttest(HSFMLY == 1 ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

# END SCRIPT