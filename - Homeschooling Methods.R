# Homeschooling Methods

PFIdesign <- update(PFIdesign,  home_public = ifelse(SCHTYPE==3, "home", ifelse(SCHTYPE==1, "public", NA)))

# Questions for each item: 
# -- Does it differ by SES?
# -- Does it differ by reasons for homeschooling? 
#     -- HSRELGON, disability, HSDISSATX, HSSAFETYX

# ---

# FIRST QUESTION: Who provides the instruction? HSWHOX

round(wpct(HOME$HSWHOX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, HOMEdesign))
cv(svymean(~HSWHOX == 2, HOMEdesign))
cv(svymean(~HSWHOX == 5, HOMEdesign))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, HOMEdesign))

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 1)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 2)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of hsing
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSWHOX == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX == 5, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSWHOX != 1 & HSWHOX != 2 & HSWHOX != 5, subset(HOMEdesign, elementary_secondary != 1)))

# ---

# NEXT QUESTION: Is the child enrolled in online classes? HSINTNET

# what percent of all homeschoolers take online classes?
round(wpct(HOME$HSINTNET, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, HOMEdesign))
cv(svymean(~HSINTNET == 2, HOMEdesign))
cv(svymean(~HSINTNET == 3, HOMEdesign))
cv(svymean(~HSINTNET == 4, HOMEdesign))

# what percent are enrolled full-time in a virtual school?
round(wpct(HOME$EDCINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# what percent of full-time virtual schoolers take online classes?
part <- subset(HOME, EDCINTK12 == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

# what percent of those NOT full-time virtual schoolers take online classes?
part <- subset(HOME, EDCINTK12 != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTNET == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 3, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSINTNET == 4, subset(HOMEdesign, elementary_secondary != 1)))

# ---

# NEXT QUESTION: What is their teaching style? HSSTYL

round(wpct(HOME$HSSTYL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, HOMEdesign))
cv(svymean(~HSSTYL == 2, HOMEdesign))
cv(svymean(~HSSTYL == 3, HOMEdesign))
cv(svymean(~HSSTYL == 4, HOMEdesign))

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 1)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 2)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of homeschooling
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ALWAYS == 1)))

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, ALWAYS != 1)))

# Elementary v secondary grades
part <- subset(HOME, elementary_secondary == 1) # grades K-6
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12
round(wpct(part$HSSTYL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSSTYL == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 2, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 3, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSSTYL == 4, subset(HOMEdesign, elementary_secondary != 1)))

# ---

# NEXT QUESTION: Co-ops and homeschool groups?

round(wpct(HOME$HSCOOP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSKACTIV, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSASSNX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSNATL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, HOMEdesign))
cv(svymean(~HSKACTIV == 1, HOMEdesign))
cv(svymean(~HSASSNX == 1, HOMEdesign))
cv(svymean(~HSNATL == 1, HOMEdesign))

# by SES level
part <- subset(HOME, SES == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 1)))

part <- subset(HOME, SES == 2)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 2)))

part <- subset(HOME, SES == 3)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
part <- subset(HOME, ALWAYS == 1) # always homeschooled

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ALWAYS == 1)))

part <- subset(HOME, ALWAYS != 1) # all homeschool transfers

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, ALWAYS != 1)))

# elementary v. secondary students
part <- subset(HOME, elementary_secondary == 1) # grades K-6

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, elementary_secondary == 1)))

part <- subset(HOME, elementary_secondary != 1) # grades 7-12

round(wpct(part$HSCOOP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSKACTIV, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSASSNX, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSNATL, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCOOP == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSKACTIV == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSASSNX == 1, subset(HOMEdesign, elementary_secondary != 1)))
cv(svymean(~HSNATL == 1, subset(HOMEdesign, elementary_secondary != 1)))

# ---

# NEXT QUESTION: 

#START with where they get curriculum

# public library
round(wpct(HOME$HSCLIBRX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# homeschool catalogue or publisher
round(wpct(HOME$HSCHSPUBX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# public school
round(wpct(HOME$HSCPUBLX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
# homeschool convention or other event for homeschooling families
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, HOMEdesign)

round(wpct(HOME$HSCCNVX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSCEVTX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSCFMLY, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSCLIBRX == 1, HOMEdesign))
cv(svymean(~HSCHSPUBX == 1, HOMEdesign))
cv(svymean(~HSCPUBLX == 1, HOMEdesign))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, HOMEdesign))

# by SES level

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 2))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, SES == 2))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 2)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 2)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 3))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, SES == 3))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, SES == 3)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, SES == 3)))

# by reasons for homeschooling: religion
svymean(~HSCLIBRX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, HSRELGON == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, HSRELGON == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, HSRELGON == 1)))

# by reasons for homeschooling: disability
svymean(~HSCLIBRX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, disability == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, disability == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, disability == 1)))

# by bachelor's degree or no
svymean(~HSCLIBRX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ba_no_ba == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ba_no_ba == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ba_no_ba == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ba_no_ba == 1)))

# no HS diploma
# by bachelor's degree or no
svymean(~HSCLIBRX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, PARGRADEX == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, PARGRADEX == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, PARGRADEX == 1)))

# length of time homeschooled
svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ALWAYS == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ALWAYS == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS != 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, ALWAYS != 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, ALWAYS != 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, ALWAYS != 1)))

# elementary v. secondary students
svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 1))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, elementary_secondary == 1))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 1)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, elementary_secondary == 1)))

svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 2))
svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
        subset(HOMEdesign, elementary_secondary == 2))

cv(svymean(~HSCLIBRX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCHSPUBX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCPUBLX == 1, subset(HOMEdesign, elementary_secondary == 2)))
cv(svymean(~HSCCNVX == 1 | HSCEVTX == 1 | HSCFMLY == 1, 
           subset(HOMEdesign, elementary_secondary == 2)))

# ---

# NEXT QUESTION: What types of schools or teachers provide the child's
# online courses?

# create a design object so I can test the coefficient of variance
# note that these respondents are ONLY the ones providing online courses
ONLINE <- subset(HOME, HSINTNET != 4)
ONLINEdesign <- svrepdesign(
  data = ONLINE, 
  repweights = subset(ONLINE, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(ONLINEdesign)
# end creation of survey object

# full list of options for this question
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCOL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTIND, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, ONLINEdesign), na.rm=TRUE)

# PLAYGROUND full list of options
part <- subset(HOME, HSINTNET == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)

# PLAYGROUND testing coefficient of variation
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)


# SECIFIC STATISTICAL SIGNIFICANCE CACULATIONS

# through public school, low SES v other
svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES != 1))
svyttest((HSINTPUB == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)
# through a virtual school, low SES v other
svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES != 1))
svyttest((HSINTVRT == 1) ~ (SES == 1), 
         HOMEdesign,
         na.rm=TRUE)
# through a company for purchase, high SES v other
svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 3))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES != 3))
svyttest((HSINTCMP == 1) ~ (SES == 3), 
         HOMEdesign,
         na.rm=TRUE)

# through public school, all online courses v. some online courses
svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTPUB == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)
# through virtual academy, all online courses v. some online courses
svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTVRT == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)
# through company, all online courses v. some online courses
svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3))
HOMEdesign <- update(HOMEdesign,  all_some = ifelse(HSINTNET == 1, "all", ifelse((HSINTNET == 2 | HSINTNET == 3), "some", NA)))
svyttest((HSINTCMP == 1) ~ all_some, 
         HOMEdesign,
         na.rm=TRUE)

# parents with a BA v. those without, online courses through a public school
svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX < 4))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX > 3))
svyttest((HSINTPUB == 1) ~ (PARGRADEX < 4), 
         HOMEdesign,
         na.rm=TRUE)
# parents with a BA v. those without, online courses through a private company
svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX < 4))
svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX > 3))
svyttest((HSINTCMP == 1) ~ (PARGRADEX < 4), 
         HOMEdesign,
         na.rm=TRUE)

# virtual school, disability v. no
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)
# online public school, disability v. no
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)


# online public school, disability v. no
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 3))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 3))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 3),
         na.rm=TRUE)

svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1))
svyttest((HSINTPUB == 1) ~ (disability == 1), 
         HOMEdesign,
         na.rm=TRUE)


# virtual school, disabilities or no by SES
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 1))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 1),
         na.rm=TRUE)
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 2))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 2))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 2),
         na.rm=TRUE)
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 3))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 3))
svyttest((HSINTVRT == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 3),
         na.rm=TRUE)


# THIS IS THE ONE TO KEEP
# disabilities, online courses v. all other options -- THIS IS THE MOTHERLOAD
svymean(~disability == 1, subset(ONLINEdesign, HSINTPUB == 1))
svymean(~disability == 1, subset(ONLINEdesign, HSINTVRT == 1))
HOMEdesign <- update(HOMEdesign,  pub_vir = ifelse(HSINTPUB == 1, "pub", ifelse((HSINTVRT == 1), "vir", NA)))
svyttest((disability == 1) ~ pub_vir, 
         HOMEdesign,
         na.rm=TRUE)

# online courses through child's public school, -- CLOSE to sig (0.09)
# low SES ppl with and without a disability
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & SES == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & SES == 1))
svyttest((HSINTPUB == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES == 1),
         na.rm=TRUE)

# online courses through child's public school, -- CLOSE to sig (0.07)
# low and middle SES ppl with and without a disability
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & SES != 3))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & SES != 3))
svyttest((HSINTPUB == 1) ~ (disability == 1), 
         subset(HOMEdesign, SES != 3),
         na.rm=TRUE)

# online courses through child's public school -- STATISTICAL SIGNIFICANCE!!
# people without a BA, with and without a disability
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & ba_no_ba == 2))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & ba_no_ba == 2))
svyttest((HSINTPUB == 1) ~ (disability == 1), 
         subset(HOMEdesign, ba_no_ba == 2),
         na.rm=TRUE)

# People with disability more likely to do online courses than others
svymean(~HSINTNET < 4, subset(HOMEdesign, disability == 1))
svymean(~HSINTNET < 4, subset(HOMEdesign, disability != 1))
svyttest((HSINTNET < 4) ~ (disability == 1), 
         HOMEdesign, 
         na.rm=TRUE)



svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & SES == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & SES == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & SES == 2))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & SES == 2))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & SES == 3))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & SES == 3))
# 53.7% of low ses ppl who don't have a disability use online public school
# only 18.4% of low ses people wiht a disability use online public school

svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 1))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 2))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 2))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1 & SES == 3))
svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability != 1 & SES == 3))

svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & ba_no_ba == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & ba_no_ba == 1))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1 & ba_no_ba == 2))
svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability != 1 & ba_no_ba == 2))


part <- subset(HOME, disability == 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSINTNET, weight=part$FPWT, na.rm=TRUE), digits = 3)




 
# KEEP THIS
# ALL PARENTS WITH ONLINE COURSES
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCOL, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTK12, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTIND, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTOH, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, ONLINEdesign), na.rm=TRUE)
# Respondents whose kids are in online courses for ALL classes
part <- subset(HOME, HSINTNET == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTOH, weight=part$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, subset(ONLINEdesign, HSINTNET == 1)), na.rm=TRUE)
# Respondents whose kids are in online courses for SOME classes
part <- subset(HOME, HSINTNET == 2 | HSINTNET == 3)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCOL, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTK12, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTIND, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTOH, weight=part$FPWT, na.rm=TRUE), digits = 3)
cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTCOL == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTK12 == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTIND == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)
cv(svymean(~HSINTOH == 1, subset(ONLINEdesign, HSINTNET == 2 | HSINTNET == 3)), na.rm=TRUE)

# SOURCES FOR ONLINE COURSES
# Mundane overall examination, like the others

# potentially am only looking at these:
round(wpct(HOME$HSINTPUB, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTPRI, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTVRT, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(HOME$HSINTCMP, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, ONLINEdesign), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, ONLINEdesign), na.rm=TRUE)

# BY SES LEVEL
part <- subset(HOME, SES == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 1)), na.rm=TRUE)

part <- subset(HOME, SES == 2)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 2)), na.rm=TRUE)

part <- subset(HOME, SES == 3)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, SES == 3)), na.rm=TRUE)

# BY REASONS FOR HOMESCHOOLING
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, HSRELGON == 1)), na.rm=TRUE)

part <- subset(HOME, disability == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, disability == 1)), na.rm=TRUE)

# BY PARENT EDUCATION LEVEL
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ba_no_ba == 1)), na.rm=TRUE)

part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, PARGRADEX == 1)), na.rm=TRUE)

# BY LENGTH OF TIME HOMESCHOOLING
part <- subset(HOME, ALWAYS == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ALWAYS == 1)), na.rm=TRUE)

part <- subset(HOME, ALWAYS != 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, ALWAYS != 1)), na.rm=TRUE)

# BY CHILD'S GRADE LEVEL
part <- subset(HOME, elementary_secondary == 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, elementary_secondary == 1)), na.rm=TRUE)

part <- subset(HOME, elementary_secondary != 1)
round(wpct(part$HSINTPUB, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTPRI, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTVRT, weight=part$FPWT, na.rm=TRUE), digits = 3)
round(wpct(part$HSINTCMP, weight=part$FPWT, na.rm=TRUE), digits = 3)

cv(svymean(~HSINTPUB == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTPRI == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTVRT == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)
cv(svymean(~HSINTCMP == 1, subset(ONLINEdesign, elementary_secondary != 1)), na.rm=TRUE)





# END section on homeschooling methods