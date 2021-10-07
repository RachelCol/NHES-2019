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

# START HERE!






# ---

# NEXT QUESTION: In the past month, have you visited a library? FOLIBRAYX 

round(wpct(HOME$FOLIBRAYX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, PARGRADEX != 1)
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of homeschooling
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$FOLIBRAYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# ---

# NEXT QUESTION: Do you get physical curriculum from your local public schools? HSCPUBLX

round(wpct(HOME$HSCPUBLX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, PARGRADEX != 1)
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of homeschooling
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSCPUBLX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# ---

# NEXT QUESTION: Do you get physical curriculum from a homeschool convention?

round(wpct(HOME$HSCCNVX, weight=HOME$FPWT, na.rm=TRUE), digits = 3)

# by SES level
part <- subset(HOME, SES == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 2)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: religion
part <- subset(HOME, HSRELGON == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by reasons for homeschooling: disability
part <- subset(HOME, disability == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, disability != 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# by bachelor's degree or no
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# no HS diploma
part <- subset(HOME, PARGRADEX == 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, PARGRADEX != 1)
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)

# length of time homeschooled
part <- subset(HOME, FIRST == 1 & ALLGRADEX > 0) # first year of homeschooling
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, FIRST != 1 & ALWAYS != 1) # some years hsing
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS == 1) # always homeschooled
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ALWAYS != 1) # all homeschool transfers
round(wpct(part$HSCCNVX, weight=part$FPWT, na.rm=TRUE), digits = 3)



# DIFFERENT SECTION 
# ADD TO REASONS FOR HOMESCHOOLING

# Does this child have specific disabilities? 

# having any condition
svymean(~condition==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~condition==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((condition==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having an intellectual disability (mental retardation) -- NOT significant
svymean(~HDINTDIS==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDINTDIS==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDINTDIS==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a speech impairment -- NOT significant
svymean(~HDSPEECHX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDSPEECHX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDSPEECHX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a serious emotional disturbance -- NOT significant
svymean(~HDDISTRBX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDISTRBX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDDISTRBX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having deafness or hearing impairment -- NOT significant
svymean(~HDDEAFIMX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDEAFIMX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDDEAFIMX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a blindness or visual impairment -- NOT significant
svymean(~HDBLINDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDBLINDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDBLINDX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having an orthopedic impairment -- ALMOST significant (p = 0.06)
svymean(~HDORTHOX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDORTHOX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest((HDORTHOX==1) ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having autism -- statistically significant ( < 0.05)
svymean(~HDAUTISMX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDAUTISMX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDAUTISMX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having pervasive developmental disorder (PDD) -- ALMOST sig (p = 0.1)
svymean(~HDPDDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDPDDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDPDDX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having ADD or ADHD -- NOT significant
svymean(~HDADDX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDADDX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDADDX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a specific learning disability -- statistically significant (p < 0.05)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDLEARNX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# the above, broken down by SES
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 1), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 2), na.rm=TRUE)
svymean(~HDLEARNX==1, subset(PFIdesign, SCHTYPE==3 & SES == 3), na.rm=TRUE)

# having a developmental delay -- statistically significant (p < 0.05)
svymean(~HDDELAYX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDDELAYX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDDELAYX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having a traumatic brain injury -- NOT significant
svymean(~HDTRBRAIN==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDTRBRAIN==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDTRBRAIN==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# having other health impairment longer than 6mos -- NOT significant
svymean(~HDOTHERX==1, subset(PFIdesign, SCHTYPE==3), na.rm=TRUE)
svymean(~HDOTHERX==1, subset(PFIdesign, SCHTYPE==1), na.rm=TRUE)
svyttest(HDOTHERX==1 ~ home_public, 
         PFIdesign,
         na.rm=TRUE)

# END SCRIPT