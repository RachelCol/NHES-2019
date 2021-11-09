# NONTRADITIONAL/ALTERNATIVE EDUCATION

# note: This script is designed to run after 0_data_subsets script.

table(HOME$HSALTX, HOME$HSSTYL)

ALTX <- subset(HOME, HSALTX == 1)
NOALTX <- subset(HOME, HSALTX == 2)

# -----

# Homeschool style, by who selected HSALTX
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

# alted number one reason, informal homeschooling style
table(HOME$HSMOSTX == 8, HOME$HSSTYL > 2)

# ----

# Analysis of homeschool style: mostly or wholly informal

# low/middle SES homeschooling families with religious motivations
svymean(~HSSTYL > 2, subset(HOMEdesign, HSRELGON == 1 & SES != 3))

# Mostly or wholly informal, by SES
svymean(~HSSTYL > 2, subset(HOMEdesign, SES != 3))
svymean(~HSSTYL > 2, subset(HOMEdesign, SES == 3))
# by SES and always homeschooling
svymean(~HSSTYL > 2, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSSTYL > 2, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSSTYL > 2, subset(HOMEdesign, SES != 3 & ALWAYS != 1))
svymean(~HSSTYL > 2, subset(HOMEdesign, SES == 3 & ALWAYS != 1))

# low/middle SES, always homeschooling, no religious motivations
svymean(~HSSTYL > 2, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSRELGON != 1))

# What percent of low/middle SES always homeschooling families have 
# religious motivations? by teaching style
svymean(~HSRELGON == 1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSSTYL > 2))
svymean(~HSRELGON == 1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSSTYL < 3))

# Wholly informal, by SES
svymean(~HSSTYL == 4, subset(HOMEdesign, SES != 3))
svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 3))
# by SES and always homeschooling
svymean(~HSSTYL == 4, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSSTYL == 4, subset(HOMEdesign, SES != 3 & ALWAYS != 1))
svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 3 & ALWAYS != 1))

# What percent of HSALTX == most important have religious motivations?
svymean(~HSRELGON == 1, subset(HOMEdesign, HSMOSTX != 8))
svymean(~HSRELGON == 1, subset(HOMEdesign, HSMOSTX == 8))

# What percent of religious homeschooling families use informal learning?
svymean(~HSSTYL > 2, subset(HOMEdesign, HSRELGON != 1))
svymean(~HSSTYL > 2, subset(HOMEdesign, HSRELGON == 1))
# t-test
svyttest((HSSTYL > 2) ~ HSRELGON, 
         HOMEdesign,
         na.rm=TRUE)

# wholly informal, religious motivations or no
svymean(~HSSTYL == 4, subset(HOMEdesign, HSRELGON != 1))
svymean(~HSSTYL == 4, subset(HOMEdesign, HSRELGON == 1))
# strictly formal, religious motivations or no
svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON != 1))
svymean(~HSALTX == 1, subset(HOMEdesign, HSRELGON == 1))

# SES of wholly informal homeschooling families
svymean(~SES == 1, subset(HOMEdesign, HSSTYL == 4))
svymean(~SES == 2, subset(HOMEdesign, HSSTYL == 4))
svymean(~SES == 3, subset(HOMEdesign, HSSTYL == 4))

# percent of wholly informal that said HSALTX
svymean(~HSALTX == 1, subset(HOMEdesign, HSSTYL == 4))

# percent of wholly informal that are always, or have religious motivations
svymean(~ALWAYS == 1, subset(HOMEdesign, HSSTYL == 4))
svymean(~HSRELGON == 1, subset(HOMEdesign, HSSTYL == 4))

# religious motivations, by homeschool style (informal or formal)
svymean(~HSRELGON == 1, subset(HOMEdesign, HSSTYL > 2))
svymean(~HSRELGON == 1, subset(HOMEdesign, HSSTYL < 3))

# informal learning, first-year homeschooling transfers
svymean(~HSSTYL > 2, subset(HOMEdesign, FIRST == 1 & ALLGRADEX != 0))

# percent of wholly informal that are in elementary school
svymean(~elementary_secondary == 1, subset(HOMEdesign, HSSTYL == 4))

# percent of wholly informal that expect their child to earn a college degree
svymean(~SEFUTUREX > 4, subset(HOMEdesign, HSSTYL == 4))

# Of low/middle SES always homeschooling families using mostly or wholly
# informal learning, what percent said HSALTX?
svymean(~HSALTX == 1, subset(HOMEdesign, SES != 3 & ALWAYS == 1 & HSSTYL > 2))

# HSALTX, by always homeschooling and SES
svymean(~HSALTX == 1, subset(HOMEdesign, SES != 3 & ALWAYS == 1))
svymean(~HSALTX == 1, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~HSALTX == 1, subset(HOMEdesign, SES != 3 & ALWAYS != 1))
svymean(~HSALTX == 1, subset(HOMEdesign, SES == 3 & ALWAYS != 1))

# END SCRIPT