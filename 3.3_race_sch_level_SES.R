# Student's race, school level, and SES
# note: This did not make it into the paper

# Grades K-6, White students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 1 & SCHTYPE == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1 & white_nonwhite == 1),
         na.rm=TRUE)

# Grades K-6, Black students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 2 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 2 & SCHTYPE == 1 & elementary_secondary == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1 & white_nonwhite == 2),
         na.rm=TRUE)

# Grades 7-12, White students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 1 & SCHTYPE == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2 & white_nonwhite == 1),
         na.rm=TRUE)

# Grades 7-12, Black students, public school v. homeschool
part <- subset(HOME, white_nonwhite == 2 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(PFI, white_nonwhite == 2 & SCHTYPE == 1 & elementary_secondary == 2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

svyttest((SES == 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2 & white_nonwhite == 2),
         na.rm=TRUE)

# end script