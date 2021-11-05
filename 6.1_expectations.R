# Parent expectations

# This script is in the process of being updated!

# Need to go through and remove things -- update

# FUTURE EXPECTATIONS OF EDUCATIONAL ATTAINMENT
HOMEdesign <- update(PFIdesign,  home_public = ifelse(SCHTYPE == 3, "home", 
                                                      ifelse(SCHTYPE == 1, "public", NA)))


# Parent Education
# Homeschool v. Public School


# ALL RESPONDENTS
# Percent of households with a parent with a BA, homeschool v. public school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      PFIdesign,
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         PFIdesign, 
         na.rm=TRUE)

# Respondents with a child in elementary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, elementary_secondary == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1), 
         na.rm=TRUE)

# Respondents with a child in secondary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, elementary_secondary == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2), 
         na.rm=TRUE)

# ---

# WHITE CHILDREN

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, white_nonwhite == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 1),
         na.rm=TRUE)

# Respondents with a child in elementary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 1 &
               white_nonwhite == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 1 &
                  white_nonwhite == 1), 
         na.rm=TRUE)

# Respondents with a child in secondary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 2 &
               white_nonwhite == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 2 &
                  white_nonwhite == 1), 
         na.rm=TRUE)

# ---

# NONWHITE CHILDREN

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             white_nonwhite == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                white_nonwhite == 2),
         na.rm=TRUE)

# Respondents with a child in elementary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 1 &
               white_nonwhite == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 1 &
                  white_nonwhite == 2), 
         na.rm=TRUE)

# Respondents with a child in secondary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 2 &
               white_nonwhite == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 2 &
                  white_nonwhite == 2), 
         na.rm=TRUE)

# ---

# INTER-HOMESCHOOL COMPARISON

# Percent of homeschooling households with a parent with a bachelor's degree, 
# elementary v. secondary 

svyby(~(PARGRADEX > 3), 
      ~elementary_secondary, 
      subset(PFIdesign, home_public == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ elementary_secondary, 
         subset(PFIdesign, home_public == 1),
         na.rm=TRUE)

# ---

# CHILDREN IN TWO-PARENT HOUSEHOLDS

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, two_parent_or_single == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, two_parent_or_single == 1),
         na.rm=TRUE)

# Respondents with a child in elementary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 1 &
               two_parent_or_single == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 1 &
                  two_parent_or_single == 1), 
         na.rm=TRUE)

# Respondents with a child in secondary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 2 &
               two_parent_or_single == 1),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 2 &
                  two_parent_or_single == 1), 
         na.rm=TRUE)

# ---

# CHILDREN IN SINGLE PARENT HOUSEHOLDS

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             two_parent_or_single == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                two_parent_or_single == 2),
         na.rm=TRUE)

# Respondents with a child in elementary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 1 &
               two_parent_or_single == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 1 &
                  two_parent_or_single == 2), 
         na.rm=TRUE)

# Respondents with a child in secondary school

svyby(~(PARGRADEX > 3), 
      ~home_public, 
      subset(PFIdesign, 
             elementary_secondary == 2 &
               two_parent_or_single == 2),
      svymean, 
      na.rm=TRUE)

svyttest((PARGRADEX > 3) ~ home_public, 
         subset(PFIdesign, 
                elementary_secondary == 2 &
                  two_parent_or_single == 2), 
         na.rm=TRUE)




# 1. Percent expecting their child to obtain a bachelor's degree,
# homeschool v. public school  
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3))
svyttest((SEFUTUREX > 4) ~ home_public,
         PFIdesign, na.rm=TRUE)

# HIGH SES
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES == 3))
svyttest((SEFUTUREX > 4) ~ home_public,
         subset(PFIdesign, SES == 3), 
         na.rm=TRUE)
# grades K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES == 3 & ALLGRADEX < 7))
# grades 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES == 3 & ALLGRADEX > 6))

# LOW/MIDDLE SES 
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES != 3))
svyttest((SEFUTUREX > 4) ~ home_public,
         subset(PFIdesign, SES != 3), 
         na.rm=TRUE)
# grades K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES != 3 & ALLGRADEX < 7))
# grades 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES != 3 & ALLGRADEX > 6))

# ALWAYS v. TRANSFERS
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1))
# high SES
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3)))
# low/middle SES
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3)))
# GRADES K-6 v. GRADES 7-12
# high SES, grades K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3 & ALLGRADEX < 7))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3 & ALLGRADEX < 7)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3 & ALLGRADEX < 7)))
# low/middle SES, grades K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3 & ALLGRADEX < 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3 & ALLGRADEX < 7))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3 & ALLGRADEX < 7)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3 & ALLGRADEX < 7)))
# high SES, grades 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3 & ALLGRADEX > 6))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES == 3 & ALLGRADEX > 6)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES == 3 & ALLGRADEX > 6)))
# low/middle SES, grades 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3 & ALLGRADEX > 6))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & SES != 3 & ALLGRADEX > 6)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & SES != 3 & ALLGRADEX > 6)))

# HSINTNET
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3 & SES != 3))




# HIGH SCHOOL
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 12))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 12)))

# by grade division
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2 | 
                                    ALLGRADEX == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 ALLGRADEX == 4 | 
                                    ALLGRADEX == 5 | 
                                    ALLGRADEX == 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 ALLGRADEX == 7 | 
                                    ALLGRADEX == 8 | 
                                    ALLGRADEX == 9))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 ALLGRADEX == 10 | 
                                 ALLGRADEX == 11 | 
                                 ALLGRADEX == 12))
# public school comparison
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2 | 
                                    ALLGRADEX == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 ALLGRADEX == 4 | 
                                    ALLGRADEX == 5 | 
                                    ALLGRADEX == 6))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 ALLGRADEX == 7 | 
                                    ALLGRADEX == 8 | 
                                    ALLGRADEX == 9))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 ALLGRADEX == 10 | 
                                    ALLGRADEX == 11 | 
                                    ALLGRADEX == 12))

# another grade division
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 3 | 
                                    ALLGRADEX == 4 | 
                                    ALLGRADEX == 5)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 6 | 
                                    ALLGRADEX == 7 | 
                                    ALLGRADEX == 8)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 9 | 
                                    ALLGRADEX == 10 | 
                                    ALLGRADEX == 11 | 
                                    ALLGRADEX == 12)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 0 | 
                                     ALLGRADEX == 1 | 
                                     ALLGRADEX == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 3 | 
                                     ALLGRADEX == 4 | 
                                     ALLGRADEX == 5))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 6 | 
                                     ALLGRADEX == 7 | 
                                     ALLGRADEX == 8))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, 
                                 (ALLGRADEX == 9 | 
                                     ALLGRADEX == 10 | 
                                     ALLGRADEX == 11 | 
                                     ALLGRADEX == 12))))
# public school comparison
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & 
                                 (ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 (ALLGRADEX == 3 | 
                                    ALLGRADEX == 4 | 
                                    ALLGRADEX == 5)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 (ALLGRADEX == 6 | 
                                    ALLGRADEX == 7 | 
                                    ALLGRADEX == 8)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 &  
                                 (ALLGRADEX == 9 | 
                                    ALLGRADEX == 10 | 
                                    ALLGRADEX == 11 | 
                                    ALLGRADEX == 12)))
# private school comparison
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 & 
                                    (ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 3 | 
                                    ALLGRADEX == 4 | 
                                    ALLGRADEX == 5)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 6 | 
                                    ALLGRADEX == 7 | 
                                    ALLGRADEX == 8)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 9 | 
                                    ALLGRADEX == 10 | 
                                    ALLGRADEX == 11 | 
                                    ALLGRADEX == 12)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 & 
                                    (ALLGRADEX == 0 | 
                                        ALLGRADEX == 1 | 
                                        ALLGRADEX == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 3 | 
                                        ALLGRADEX == 4 | 
                                        ALLGRADEX == 5))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 6 | 
                                        ALLGRADEX == 7 | 
                                        ALLGRADEX == 8))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 2 &  
                                    (ALLGRADEX == 9 | 
                                        ALLGRADEX == 10 | 
                                        ALLGRADEX == 11 | 
                                        ALLGRADEX == 12))))

# private school comparison
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 & 
                                    (ALLGRADEX == 0 | 
                                    ALLGRADEX == 1 | 
                                    ALLGRADEX == 2)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 3 | 
                                    ALLGRADEX == 4 | 
                                    ALLGRADEX == 5)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 6 | 
                                    ALLGRADEX == 7 | 
                                    ALLGRADEX == 8)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 9 | 
                                    ALLGRADEX == 10 | 
                                    ALLGRADEX == 11 | 
                                    ALLGRADEX == 12)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 & 
                                    (ALLGRADEX == 0 | 
                                        ALLGRADEX == 1 | 
                                        ALLGRADEX == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 3 | 
                                        ALLGRADEX == 4 | 
                                        ALLGRADEX == 5))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 6 | 
                                        ALLGRADEX == 7 | 
                                        ALLGRADEX == 8))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 4 &  
                                    (ALLGRADEX == 9 | 
                                        ALLGRADEX == 10 | 
                                        ALLGRADEX == 11 | 
                                        ALLGRADEX == 12))))

# by individual grade
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 0))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 4))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 5))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 8))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 9))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 10))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 11))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX == 12))

svymean(~(SEFUTUREX < 4), subset(HOMEdesign, ALLGRADEX > 8))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, ALLGRADEX > 8))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8))

svymean(~(SEFUTUREX < 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8))

svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 3))

svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 3))

svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 3))


svymean(~(SES == 3), subset(HOMEdesign, ALLGRADEX < 3))
svymean(~(SES == 3), subset(HOMEdesign, ALLGRADEX > 6))

# high SES v low/middle SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & SES != 3)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 8 & SES != 3)))

# expectations of longterm homeschoolers?
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 5))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 9))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 10))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL > 11))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL == 2 | TOTAL == 3 | TOTAL == 4 | TOTAL == 5))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL == 6 | TOTAL == 7 | TOTAL == 8 | TOTAL == 9))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, TOTAL == 10 | TOTAL == 11 | TOTAL == 12 | TOTAL == 13))




# ALWAYS homeschooling expectations, by SES and grade level
# grades K-6
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES != 3)))
# grades 7-12
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES != 3)))
# WHAT PERCENT have disabilities
# grades K-6
svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES == 3))
svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX < 7 & ALWAYS == 1 & SES != 3))
# grades 7-12
svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES == 3))
svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX > 6 & ALWAYS == 1 & SES != 3))
# FURTHER BREAKDOWN
# grades K-2
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 3 & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 3 & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 3 & ALWAYS == 1 & SES != 3))
# grades 3-5
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 3 | ALLGRADEX == 4 | ALLGRADEX == 5) & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 3 | ALLGRADEX == 4 | ALLGRADEX == 5) & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 3 | ALLGRADEX == 4 | ALLGRADEX == 5) & ALWAYS == 1 & SES != 3))
# grades 6-8
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 6 | ALLGRADEX == 7 | ALLGRADEX == 8) & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 6 | ALLGRADEX == 7 | ALLGRADEX == 8) & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (ALLGRADEX == 6 | ALLGRADEX == 7 | ALLGRADEX == 8) & ALWAYS == 1 & SES != 3))
# grades 9-12
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 8 & ALWAYS == 1 & SES != 3))

# how many students are in these groups?
svytable(~ SEFUTUREX + TOTAL, HOMEdesign)
table(HOME$SEFUTUREX, HOME$TOTAL)
table(HOME$SES, HOME$TOTAL)
table(HOME$TOTAL)

# IMPORTANT: expectations by SES and grade level
# all grades, homeschool
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES != 3))
# all grades, public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
# grades K-6, homeschool
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX < 7 & SES != 3)))
# grades K-6, public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX < 7 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX < 7 & SES != 3))
# grades 7-12, homeschool
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALLGRADEX > 6 & SES != 3)))
# grades 7-12, public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 6 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & ALLGRADEX > 6 & SES != 3))

svymean(~(SES != 3), subset(HOMEdesign, ALLGRADEX < 7))
svymean(~(SES != 3), subset(HOMEdesign, ALLGRADEX > 6))

svyttest((SEFUTUREX > 4) ~ home_public,
         subset(PFIdesign, SES == 3), 
         na.rm=TRUE)

svyttest((SEFUTUREX > 4) ~ home_public,
         subset(PFIdesign, SES != 3), 
         na.rm=TRUE)



# how many have a child with a disability?
svymean(~(disability == 1), subset(HOMEdesign, ALLGRADEX > 8 & SES == 3))
svymean(~(disability == 1), subset(HOMEdesign, ALLGRADEX > 8 & SES != 3))

svymean(~(ALWAYS == 1), subset(HOMEdesign, ALLGRADEX > 6))



# ----- 

# REASONS FOR HOMESCHOOLING

# by parents' top reason for homeschooling
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 4))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 5)) # cv too high
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 6)) # cv too high
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 9))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 10)) # cv too high
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 11)) # cv too high
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 5 | 
                                    HSMOSTX == 6 | HSMOSTX == 7))

cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 1))) # .13
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 2))) # .08
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 3))) # .16
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 4))) # .23
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 5)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 6)))     
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 7))) # .22
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8))) # .08
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 9))) # .19
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 10)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 11)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 5 | 
                                    HSMOSTX == 6 | HSMOSTX == 7))) # .25
# SES of parents in each group
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 1))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 2))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 3))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 4))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 8))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 9))
svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 5 | 
                                    HSMOSTX == 6 | HSMOSTX == 7))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 1)))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 2)))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 3)))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 4))) # .33
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 8)))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 9)))
cv(svymean(~(SES == 3), subset(HOMEdesign, HSMOSTX == 5 | 
                               HSMOSTX == 6 | HSMOSTX == 7)))

# testing correcting for SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 9 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 9 & SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 9 & SES == 1))

# RELIGION, correcting for SES
# ANSWER: religion does NOT make families less likely to think of college
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSRELGON == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1 | SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSRELGON == 1 & (SES == 1 | SES == 2)))

# NONTRADITIONAL APPROACH TO EDUCATION
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1 | SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8 & (SES == 1 | SES == 2)))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8 & (SES == 1)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 8 & (SES == 2)))

# -----

# ALL DISABILITY, correcting for SES
# homeschooled children disability
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & (SES == 1 | SES == 2))))
# public school disability
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1 & (SES == 1 | SES == 2))))

# t-tests
COMBINEDdesign <- update(COMBINEDdesign,  Hdis_Pdis = ifelse(SCHTYPE == 3 & disability == 1, "Hdis", 
                                                      ifelse(SCHTYPE == 1 & IEP == 1, "Pdis", NA)))
COMBINEDdesign <- update(COMBINEDdesign,  HdisN_PdisN = ifelse(SCHTYPE == 3 & disability != 1, "HdisN", 
                                                        ifelse(SCHTYPE == 1 & IEP != 1, "PdisN", NA)))

svyttest((SEFUTUREX > 4) ~ Hdis_Pdis,
         COMBINEDdesign, 
         na.rm=TRUE)

svyttest((SEFUTUREX > 4) ~ Hdis_Pdis,
         subset(COMBINEDdesign, elementary_secondary!=1), 
         na.rm=TRUE)

svyttest((SEFUTUREX > 4) ~ HdisN_PdisN,
         subset(COMBINEDdesign, SES != 3), 
         na.rm=TRUE)

# DETERMINE what percent of children have an IEP
PFI$IEP <- ifelse(PFI$SCHTYPE == 1 & PFI$HDIEPX == 1, 1, 0)
PFI$public_school <- ifelse(PFI$SCHTYPE == 1, 1, 0)
sum((PFI$IEP*PFI$FPWT), na.rm=TRUE)/sum((PFI$public_school*PFI$FPWT), na.rm=TRUE)
# DETERMINE what percent of children are homeschooeld due to a disability
svymean(~(disability == 1), HOMEdesign)


# all disability, with SES combined
# homeschool
svymean(~(SEFUTUREX > 4), HOMEdesign)
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1)))
# public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1)))
# percent disabled kids low/middle SES
# homeschool
svymean(~(SES != 3), HOMEdesign)
svymean(~(SES != 3), subset(HOMEdesign, disability == 1))
svymean(~(SES != 3), subset(HOMEdesign, disability != 1))
cv(svymean(~(SES != 3), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SES != 3), subset(HOMEdesign, disability != 1)))
# public school
svymean(~(SES != 3), subset(PFIdesign, SCHTYPE==1))
svymean(~(SES != 3), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1))
svymean(~(SES != 3), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1))
cv(svymean(~(SES != 3), subset(PFIdesign, SCHTYPE==1 & HDIEPX == 1)))
cv(svymean(~(SES != 3), subset(PFIdesign, SCHTYPE==1 & HDIEPX != 1)))




svymean(~HDIEPX == 1, subset(PFIdesign, SCHTYPE == 1), na.rm=TRUE)
svymean(~disability == 1, subset(PFIdesign, SCHTYPE == 3))

# ALWAYS V TRANSFER
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1))
# grades K-6
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & elementary_secondary == 1))
# grades 7-12
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & elementary_secondary != 1))
# what percent of disabled homeschooled kids were transfers
svymean(~(ALWAYS == 1), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(ALWAYS == 1), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))

svymean(~(TOTAL), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(TOTAL), subset(HOMEdesign, disability != 1 & elementary_secondary != 1))

svymean(~(TOTAL < 6), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(TOTAL < 6), subset(HOMEdesign, disability != 1 & elementary_secondary != 1))

# Disabled always homeschoolers v. disabled homeschool transfers
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS != 1))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS != 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS != 1 & SES == 3))

svymean(~(disability == 1), subset(HOMEdesign, ALWAYS == 1 & SES != 3))
svymean(~(disability == 1), subset(HOMEdesign, ALWAYS != 1 & SES != 3))
svymean(~(disability == 1), subset(HOMEdesign, ALWAYS == 1 & SES == 3))
svymean(~(disability == 1), subset(HOMEdesign, ALWAYS != 1 & SES == 3))

svymean(~(SES == 1), subset(HOMEdesign, ALWAYS == 1 & SES != 3))
svymean(~(SES == 2), subset(HOMEdesign, ALWAYS == 1 & SES != 3))
svymean(~(SES == 1), subset(HOMEdesign, ALWAYS != 1 & SES != 3))
svymean(~(SES == 2), subset(HOMEdesign, ALWAYS != 1 & SES != 3))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & ALWAYS != 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & SES != 3))

# with a disability
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3))
# without a disability
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))


# Always homeschooling children, disability, by SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability != 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability != 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability != 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability != 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS == 1 & disability == 1 & (SES == 1 | SES == 2))))
# the puzzle: always homeschooling children w/o a disability are WEIRDLY
# unlikely to have parents expect them to earn a college degree

table(HOME$ALWAYS, HOME$disability)
table(HOME$ALWAYS, HOME$SES)

part <- subset(HOME, ALWAYS==1)
table(part$disability, part$SES)

part <- subset(HOME, ALWAYS!=1)
table(part$disability, part$SES)

weirdo <- subset(HOME, ALWAYS==1 & SES != 3)
table(weirdo$disability, weirdo$SEFUTUREX)

29/63
6/20

weirdo$FPWT 
weirdo$SEFUTUREX

svytable(~ALWAYS + disability, HOMEdesign)

# Homeschool transfers, disability
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability != 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability != 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability != 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability != 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, ALWAYS != 1 & disability == 1 & (SES == 1 | SES == 2))))

# DISABILITY MOST, correcting for SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) & 
                                    SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1 | SES == 2))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) & 
                                    (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) & 
                                    SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7) & 
                                    (SES == 1 | SES == 2))))

# WHAT ABOUT AGE??
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & disability == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & disability == 1))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & disability == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & disability == 1)))
# public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary == 1 & 
                                    IEP == 1 & SCHTYPE == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary != 1 & 
                                    IEP == 1 & SCHTYPE == 1))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary == 1 & 
                                       IEP == 1 & SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary != 1 & 
                                       IEP == 1 & SCHTYPE == 1)))
# add in SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & 
                                    disability == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & 
                                    disability == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    disability == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    disability == 1 & SES != 3))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    disability == 1 & SES != 3)))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary != 1 & 
                                    IEP == 1 & SCHTYPE == 1 & SES != 3))


# child does not have a disability
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & disability != 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & disability != 1))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary == 1 & disability != 1)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & disability != 1)))
# public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary == 1 & 
                                    IEP != 1 & SCHTYPE == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary != 1 & 
                                    IEP != 1 & SCHTYPE == 1))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary == 1 & 
                                       IEP != 1 & SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(PFIdesign, elementary_secondary != 1 & 
                                       IEP != 1 & SCHTYPE == 1)))


# HOW MANY students in each group?
svymean(~(disability == 1), subset(HOMEdesign, # 10.3%
                                 SES == 3 & ALWAYS == 1))
svymean(~(disability == 1), subset(HOMEdesign, # 21.1%
                                   SES != 3 & ALWAYS == 1))
svymean(~(disability == 1), subset(HOMEdesign, # 26.9%
                                   SES == 3 & ALWAYS != 1))
svymean(~(disability == 1), subset(HOMEdesign, # 41.6%
                                   SES != 3 & ALWAYS != 1))

svymean(~(HDDEAFIMX == 1), subset(HOMEdesign,
                                      elementary_secondary == 1 & disability == 1))
svymean(~(HDDEAFIMX == 1), subset(HOMEdesign, 
                                      elementary_secondary != 1 & disability == 1))

# 5 v 6 v 7
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSDISABLX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSILLX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSPCLNDX == 1))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 5))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSMOSTX == 7))


svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    HSDISABLX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    HSILLX == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    HSSPCLNDX == 1))

svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    HSMOSTX == 5 | HSMOSTX == 6 | HSMOSTX == 7))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, elementary_secondary != 1 & 
                                    disability == 1))

# how the percent changes, according to specific answer (5, 6, or 7)
svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary == 1))
svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary != 1))
svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary == 1))
svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary != 1))
svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary == 1))
svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary != 1))
# overall
svymean(~ (disability == 1), subset(HOMEdesign, elementary_secondary == 1))
svymean(~ (disability == 1), subset(HOMEdesign, elementary_secondary != 1))
# what percent OF DISABLED STUDENTS were homeschooled for each reason
svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                      disability == 1))
svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                      disability == 1))
svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                   disability == 1))
svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                   disability == 1))
svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                      disability == 1))
svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                      disability == 1))
cv(svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                      disability == 1)))
cv(svymean(~ (HSDISABLX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                      disability == 1)))
cv(svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                   disability == 1)))
cv(svymean(~ (HSILLX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                   disability == 1)))
cv(svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary == 1 & 
                                      disability == 1)))
cv(svymean(~ (HSSPCLNDX == 1), subset(HOMEdesign, elementary_secondary != 1 & 
                                      disability == 1)))


# what percent of disabled children were poor or near poor
svymean(~ (SES != 3), subset(HOMEdesign, disability == 1 & 
                                elementary_secondary == 1))
svymean(~ (SES != 3), subset(HOMEdesign, disability == 1 & 
                                elementary_secondary != 1))
# low v. middle
svymean(~ (SES == 1), subset(HOMEdesign, disability == 1 & 
                                elementary_secondary != 1))
svymean(~ (SES == 2), subset(HOMEdesign, disability == 1 & 
                                elementary_secondary != 1))
# public school
svymean(~ (SES != 3), subset(PFIdesign, IEP == 1 & SCHTYPE == 1 &
                                elementary_secondary == 1))
svymean(~ (SES != 3), subset(PFIdesign, IEP == 1 & SCHTYPE == 1 &
                                elementary_secondary != 1))
# low v. middle
svymean(~ (SES == 1), subset(PFIdesign, IEP == 1 & SCHTYPE == 1 &
                                elementary_secondary != 1))
svymean(~ (SES == 2), subset(PFIdesign, IEP == 1 & SCHTYPE == 1 &
                                elementary_secondary != 1))


# what happens if you take out those homeschooling due to disability?
# ANSWER: parental expectations are still lower
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES == 3 & disability != 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, SES != 3 & disability != 1))

# breakdown by specific expectation, disabilities or no
# disabilities, public school
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1)))
# disabilities, public school, grades K-6
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary == 1)))
# disabilities, public school, grades 7-12
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & elementary_secondary != 1)))
# disabilities, homeschool
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1))
cv(svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1)))
cv(svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1)))
# disabilities, homeschool, grades K-6
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1 & elementary_secondary == 1)))
# disabilities, homeschool, grades 7-12
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1 & elementary_secondary != 1))
cv(svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability == 1 & elementary_secondary != 1)))

# What percent of disabled kids are each SES, by school type?
svymean(~(SES == 3), subset(HOMEdesign, disability == 1))
svymean(~(SES == 2), subset(HOMEdesign, disability == 1))
svymean(~(SES == 1), subset(HOMEdesign, disability == 1))

svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))

# what percent of NON disabled kids are each SES, by school type?
svymean(~(SES == 3), subset(HOMEdesign, disability != 1))
svymean(~(SES == 2), subset(HOMEdesign, disability != 1))
svymean(~(SES == 1), subset(HOMEdesign, disability != 1))

svymean(~(SES == 3), subset(PFIdesign, SCHTYPE == 1 & IEP != 1))
svymean(~(SES == 2), subset(PFIdesign, SCHTYPE == 1 & IEP != 1))
svymean(~(SES == 1), subset(PFIdesign, SCHTYPE == 1 & IEP != 1))

# OVERALL, disability or no, homeschool v. public school
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1))

svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1))

# High & low/middle SES, with & without disability, homeschool & public school
# high SES, with disability
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 3))
# high SES, no disability
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
# low/middle SES, with disability
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3))
# low/middle SES, no disability
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 3 & disability != 1 & SES != 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))

# dividing low and middle out, with disability, public school and homecshool
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 1))
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 2))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 2))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 1)))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 2)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 1)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 2)))

# religion -- what role does it play?
# low/middle SES, with disability
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3))
# low/middle SES, no disability
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))

# by always v. transfer
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES != 3 & ALWAYS == 1))
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES != 3 & ALWAYS != 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES != 3 & ALWAYS == 1)))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES != 3 & ALWAYS != 1)))
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES == 3 & ALWAYS == 1))
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES == 3 & ALWAYS != 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES == 3 & ALWAYS == 1)))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability != 1 & SES == 3 & ALWAYS != 1)))

# t-tests
COMBINEDdesign <- update(COMBINEDdesign,  Hdis_Pdis = ifelse(SCHTYPE == 3 & disability == 1, "Hdis", 
                                                             ifelse(SCHTYPE == 1 & IEP == 1, "Pdis", NA)))
COMBINEDdesign <- update(COMBINEDdesign,  HdisN_PdisN = ifelse(SCHTYPE == 3 & disability != 1, "HdisN", 
                                                               ifelse(SCHTYPE == 1 & IEP != 1, "PdisN", NA)))

svyttest((SEFUTUREX > 4) ~ HdisN_PdisN,
         subset(COMBINEDdesign, SES != 3), 
         na.rm=TRUE)

svyttest((SEFUTUREX > 4) ~ HdisN_PdisN,
         subset(COMBINEDdesign, SES == 3), 
         na.rm=TRUE)


# adding in grade level!
# high SES, with disability, grades K-6
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 3 & elementary_secondary == 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 3 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 3 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 3 & elementary_secondary == 1)))
# high SES, with disability, grades 7-12
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 3 & elementary_secondary != 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 3 & elementary_secondary != 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES == 3 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES == 3 & elementary_secondary != 1)))
# high SES, with disability, grades K-6
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3 & elementary_secondary == 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3 & elementary_secondary == 1)))
# high SES, with disability, grades 7-12
svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3 & elementary_secondary != 1))
svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3 & elementary_secondary != 1))
cv(svymean(~(SEFUTUREX >4), subset(HOMEdesign, disability == 1 & SES != 3 & elementary_secondary != 1)))
cv(svymean(~(SEFUTUREX >4), subset(PFIdesign, SCHTYPE == 1 & IEP == 1 & SES != 3 & elementary_secondary != 1)))



svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability != 1))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability != 1))

svymean(~(SEFUTUREX < 4), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability != 1 & SES == 3))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability != 1 & SES == 3))

svymean(~(SEFUTUREX < 4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))

svymean(~(SEFUTUREX < 4), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, disability != 1 & SES != 3))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, disability != 1 & SES != 3))

svymean(~(SEFUTUREX < 4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))


# END disability section

# -----

# HSINTNET
# Use of internet courses
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 2 | HSSTYL == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 2 | HSSTYL == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4)))
# Use of internet courses by SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSINTNET == 2 | HSINTNET == 3) & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSINTNET == 2 | HSINTNET == 3) & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSINTNET == 2 | HSINTNET == 3) & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSINTNET == 2 | HSINTNET == 3) & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSINTNET == 4 & (SES == 1 | SES == 2))))

# Homeschool style
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 2 | HSSTYL == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 2 | HSSTYL == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4)))
# Homeschool style by SES
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1 & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSSTYL == 2 | HSSTYL == 3) & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSSTYL == 2 | HSSTYL == 3) & (SES == 1 | SES == 2)))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4 & SES == 3))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4 & (SES == 1 | SES == 2)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 1 & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSSTYL == 2 | HSSTYL == 3) & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, (HSSTYL == 2 | HSSTYL == 3) & (SES == 1 | SES == 2))))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4 & SES == 3)))
cv(svymean(~(SEFUTUREX > 4), subset(HOMEdesign, HSSTYL == 4 & (SES == 1 | SES == 2))))


# FIRST YEAR homeschoolers
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, FIRST == 1 & ALLGRADEX > 6))
svymean(~(SEFUTUREX > 4), subset(HOMEdesign, FIRST != 1 & ALLGRADEX > 6))
# WHAT

# What ARE parents' educational expectations, then?
# break it down by individual level
# public school
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1)))
# homeschool
svymean(~(SEFUTUREX == 1), HOMEdesign)
svymean(~(SEFUTUREX == 2), HOMEdesign)
svymean(~(SEFUTUREX == 3), HOMEdesign)
svymean(~(SEFUTUREX == 4), HOMEdesign)
svymean(~(SEFUTUREX == 5), HOMEdesign)
svymean(~(SEFUTUREX == 6), HOMEdesign)
cv(svymean(~(SEFUTUREX == 1), HOMEdesign))
cv(svymean(~(SEFUTUREX == 2), HOMEdesign))
cv(svymean(~(SEFUTUREX == 3), HOMEdesign))
cv(svymean(~(SEFUTUREX == 4), HOMEdesign))
cv(svymean(~(SEFUTUREX == 5), HOMEdesign))
cv(svymean(~(SEFUTUREX == 6), HOMEdesign))
# high SES, public school
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & SES == 3))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & SES == 3)))
# high SES, homeschool
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, SES == 3))
cv(svymean(~(SEFUTUREX == 1), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX == 2), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX == 3), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX == 4), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX == 5), subset(HOMEdesign, SES == 3)))
cv(svymean(~(SEFUTUREX == 6), subset(HOMEdesign, SES == 3)))
# low/middle SES, public school
svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & SES != 3))
cv(svymean(~(SEFUTUREX == 1), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX == 2), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX == 3), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX == 4), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX == 5), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
cv(svymean(~(SEFUTUREX == 6), subset(PFIdesign, SCHTYPE == 1 & SES != 3)))
# low/middle SES, homeschool
svymean(~(SEFUTUREX == 1), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX == 2), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX == 3), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX == 4), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX == 5), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX == 6), subset(HOMEdesign, SES != 3))
cv(svymean(~(SEFUTUREX == 1), subset(HOMEdesign, SES != 3)))
cv(svymean(~(SEFUTUREX == 2), subset(HOMEdesign, SES != 3)))
cv(svymean(~(SEFUTUREX == 3), subset(HOMEdesign, SES != 3)))
cv(svymean(~(SEFUTUREX == 4), subset(HOMEdesign, SES != 3)))
cv(svymean(~(SEFUTUREX == 5), subset(HOMEdesign, SES != 3)))
cv(svymean(~(SEFUTUREX == 6), subset(HOMEdesign, SES != 3)))

# now let's run some t-tests

svyttest((SEFUTUREX > 4) ~ home_public,
         PFIdesign, na.rm=TRUE)

svyttest((SEFUTUREX < 3) ~ home_public,
         PFIdesign, na.rm=TRUE)

svyttest((SEFUTUREX == 6) ~ home_public,
         PFIdesign, na.rm=TRUE)

svyttest((SEFUTUREX == 3) ~ home_public,
         PFIdesign, na.rm=TRUE)



# DO REASONS FOR HOMESCHOOLIGN ACTUALLY AFFECT PARENT EXPECTATIONS?
# START: create new data frame
TopReason <- c(1:490)
TopReason <- as.data.frame(TopReason)
TopReason$env <- ifelse(HOME$HSMOSTX == 1, 1, 0)
TopReason$aca <- ifelse(HOME$HSMOSTX == 2, 1, 0)
TopReason$rel <- ifelse(HOME$HSMOSTX == 3, 1, 0)
TopReason$mor <- ifelse(HOME$HSMOSTX == 4, 1, 0)
TopReason$non <- ifelse(HOME$HSMOSTX == 8, 1, 0)
TopReason$fam <- ifelse(HOME$HSMOSTX == 9, 1, 0)
TopReason$dis <- ifelse(HOME$HSMOSTX == 5 | 
                           HOME$HSMOSTX == 6 | 
                           HOME$HSMOSTX == 7, 1, 0)
TopReason$col <- ifelse(HOME$SEFUTUREX > 4, 1, 0)
TopReason
# END: new data frame is created!

# START: create new data frame ONE SES ONLY
TopReason <- c(1:490)
TopReason <- as.data.frame(TopReason)
TopReason$env <- ifelse(HOME$HSMOSTX == 1 & HOME$SES == 3, 1, 0)
TopReason$aca <- ifelse(HOME$HSMOSTX == 2 & HOME$SES == 3, 1, 0)
TopReason$rel <- ifelse(HOME$HSMOSTX == 3 & HOME$SES == 3, 1, 0)
TopReason$mor <- ifelse(HOME$HSMOSTX == 4 & HOME$SES == 3, 1, 0)
TopReason$non <- ifelse(HOME$HSMOSTX == 8 & HOME$SES == 3, 1, 0)
TopReason$fam <- ifelse(HOME$HSMOSTX == 9 & HOME$SES == 3, 1, 0)
TopReason$dis <- ifelse((HOME$HSMOSTX == 5 | 
                           HOME$HSMOSTX == 6 | 
                           HOME$HSMOSTX == 7) & HOME$SES == 3, 1, 0)
TopReason$col <- ifelse(HOME$SEFUTUREX > 4 & HOME$SES == 3, 1, 0)
TopReason
# END: new data frame is created!

# START: create new data frame ONE SES ONLY
TopReason <- c(1:490)
TopReason <- as.data.frame(TopReason)
TopReason$env <- ifelse(HOME$HSMOSTX == 1 & HOME$SES != 3, 1, 0)
TopReason$aca <- ifelse(HOME$HSMOSTX == 2 & HOME$SES != 3, 1, 0)
TopReason$rel <- ifelse(HOME$HSMOSTX == 3 & HOME$SES != 3, 1, 0)
TopReason$mor <- ifelse(HOME$HSMOSTX == 4 & HOME$SES != 3, 1, 0)
TopReason$non <- ifelse(HOME$HSMOSTX == 8 & HOME$SES != 3, 1, 0)
TopReason$fam <- ifelse(HOME$HSMOSTX == 9 & HOME$SES != 3, 1, 0)
TopReason$dis <- ifelse((HOME$HSMOSTX == 5 | 
                            HOME$HSMOSTX == 6 | 
                            HOME$HSMOSTX == 7) & HOME$SES != 3, 1, 0)
TopReason$col <- ifelse(HOME$SEFUTUREX > 4 & HOME$SES != 3, 1, 0)
TopReason
# END: new data frame is created!


logistic <- glm(col ~
                   env + aca + rel + mor + non + fam + dis,
                data=TopReason, family="binomial")
summary(logistic)
confint(logistic)




# old code 
# 1. Percent expecting their child to obtain a bachelor's degree,
# homeschool v. public school  
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, SCHTYPE == 3))
svyttest((SEFUTUREX > 4) ~ home_public,
         PFIdesign, na.rm=TRUE)

# 2. Percent of elementary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school 
# -- GRADES K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   elementary_secondary == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 1), na.rm=TRUE)

# 3. Percent of secondary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school  
# -- GRADES 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   elementary_secondary == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   elementary_secondary == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, elementary_secondary == 2), na.rm=TRUE)

# RACE OF CHILD

# 4. Percent of white parents parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 1), na.rm=TRUE)

# 5. Percent of non-white school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 2), na.rm=TRUE)

# ADD IN SCHOOL LEVEL

# 6. Percent of white elementary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
# -- GRADES K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 1 & 
                                   elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 1 & 
                                   elementary_secondary == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 1 & 
                  elementary_secondary == 1), na.rm=TRUE)

# 7. Percent of white secondary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
# -- GRADES 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 1 & 
                                   elementary_secondary == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 1 & 
                                   elementary_secondary == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 1 & 
                  elementary_secondary == 2), na.rm=TRUE)

# 8. Percent of non-white elementary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
# -- GRADES K-6
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 2 & 
                                   elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 2 & 
                                   elementary_secondary == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 2 & 
                  elementary_secondary == 1), na.rm=TRUE)

# 9. Percent of non-white secondary school parents expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
# -- GRADES 7-12
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   white_nonwhite == 2 & 
                                   elementary_secondary == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   white_nonwhite == 2 & 
                                   elementary_secondary == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, white_nonwhite == 2 & 
                  elementary_secondary == 2), na.rm=TRUE)

# PARENTS WHO HAVE BA DEGREES

# 4. Percent of parents with a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1), na.rm=TRUE)

# 5. Percent of parents without a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 2), na.rm=TRUE)

# ADD IN SCHOOL LEVEL

# 6. Percent of elementary school parents with a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 1 & 
                                   elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 1 & 
                                   elementary_secondary == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1 & 
                  elementary_secondary == 1), na.rm=TRUE)

# 7. Percent of secondary school parents with a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 1 & 
                                   elementary_secondary == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 1 & 
                                   elementary_secondary == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 1 & 
                  elementary_secondary == 2), na.rm=TRUE)

# 8. Percent of elementary school parents without a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 2 & 
                                   elementary_secondary == 1))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 2 & 
                                   elementary_secondary == 1))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 2 & 
                  elementary_secondary == 1), na.rm=TRUE)

# 9. Percent of secondary school parents without a BA expecting their child 
# to obtain a bachelor's degree, homeschool v. public school
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 1 & 
                                   ba_no_ba == 2 & 
                                   elementary_secondary == 2))
svymean(~(SEFUTUREX > 4), subset(PFIdesign, home_public == 2 & 
                                   ba_no_ba == 2 & 
                                   elementary_secondary == 2))
svyttest((SEFUTUREX > 4) ~ home_public, 
         subset(PFIdesign, ba_no_ba == 2 & 
                  elementary_secondary == 2), na.rm=TRUE)

# BY SPECIFIC RACE
svymean(~SEFUTUREX > 4, subset(HOMEdesign, RACEETH == 1))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, RACEETH == 2))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, RACEETH == 3))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, RACEETH == 4))

svymean(~SEFUTUREX > 4, subset(PFIdesign, RACEETH == 1 & SCHTYPE == 1))
svymean(~SEFUTUREX > 4, subset(PFIdesign, RACEETH == 2 & SCHTYPE == 1))
svymean(~SEFUTUREX > 4, subset(PFIdesign, RACEETH == 3 & SCHTYPE == 1))
svymean(~SEFUTUREX > 4, subset(PFIdesign, RACEETH == 4 & SCHTYPE == 1))


# Homeschool transfers v always, NON-DISABLED
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
# transfers, by SES
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES == 3))
cv(svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3)))
cv(svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES == 3)))
# public school
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3))
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3))
cv(svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3)))
cv(svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3)))
# always homeschooled
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES == 3))
cv(svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3)))
cv(svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES == 3)))

# t-tests
COMBINEDdesign <- update(COMBINEDdesign, NoDis_Always = 
                           ifelse(SCHTYPE == 3 & disability != 1 & ALWAYS == 1, "home", 
                              ifelse(SCHTYPE == 1 & IEP != 1, "public", NA)))
COMBINEDdesign <- update(COMBINEDdesign, NoDis_Trans = 
                           ifelse(SCHTYPE == 3 & disability != 1 & ALWAYS != 1, "home", 
                                  ifelse(SCHTYPE == 1 & IEP != 1, "public", NA)))

svyttest((SEFUTUREX > 4) ~ NoDis_Always,
         subset(COMBINEDdesign, SES == 3), 
         na.rm=TRUE)
svyttest((SEFUTUREX > 4) ~ NoDis_Always,
         subset(COMBINEDdesign, SES != 3), 
         na.rm=TRUE)
svyttest((SEFUTUREX > 4) ~ NoDis_Trans,
         subset(COMBINEDdesign, SES == 3), 
         na.rm=TRUE)
svyttest((SEFUTUREX > 4) ~ NoDis_Trans,
         subset(COMBINEDdesign, SES != 3), 
         na.rm=TRUE)

# nondisabled transfers v. nondisabled always, reasons for hsing
svymean(~HSSAFETYX == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSSAFETYX == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSDISSATX == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSDISSATX == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSRELGON == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSRELGON == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSMORAL == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSMORAL == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSALTX == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSALTX == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSFMLY == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSFMLY == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))
svymean(~HSOTHERX == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1))
svymean(~HSOTHERX == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1))

# specific SES level for low/middle
# homeschool
svymean(~SES == 1, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3))
svymean(~SES == 2, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3))
svymean(~SES == 1, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3))
svymean(~SES == 2, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3))
# public school
svymean(~SES == 1, subset(PFIdesign, IEP != 1 & SES != 3))
svymean(~SES == 2, subset(PFIdesign, IEP != 1 & SES != 3))

# effect of grade level, non-disabled always hsing and transfers, by SES
# transfers, by SES
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES == 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES != 3 & elementary_secondary == 2))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS != 1 & SES == 3 & elementary_secondary == 2))
# public school
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES != 3 & elementary_secondary == 2))
svymean(~SEFUTUREX > 4, subset(PFIdesign, SCHTYPE == 1 & IEP != 1 & SES == 3 & elementary_secondary == 2))
# always homeschooled
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES == 3 & elementary_secondary == 1))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES != 3 & elementary_secondary == 2))
svymean(~SEFUTUREX > 4, subset(HOMEdesign, disability != 1 & ALWAYS == 1 & SES == 3 & elementary_secondary == 2))



# PROBABLY DELETE THIS SECTION 

# parents v. kids

table(HOME$PARGRADEX, HOME$FUTUREX)

HOME$FUTUREX - HOME$PARGRADEX
sum(HOME$FUTUREX - HOME$PARGRADEX)/sum(HOME$countn)

PUBLIC <- subset(PFI, SCHTYPE == 1)
PUBLIC$FUTUREX - PUBLIC$PARGRADEX
sum(PUBLIC$FUTUREX - PUBLIC$PARGRADEX)/sum(PUBLIC$countn)

# low/middle SES homeschooling families
LMHOME <- subset(HOME, SES != 3)
LMHOME$FUTUREX - LMHOME$PARGRADEX
sum(LMHOME$FUTUREX - LMHOME$PARGRADEX)/sum(LMHOME$countn)

HHOME <- subset(HOME, SES == 3)
HHOME$FUTUREX - HHOME$PARGRADEX
sum(HHOME$FUTUREX - HHOME$PARGRADEX)/sum(HHOME$countn)

HHOME <- subset(HOME, ALWAYS == 1 & SES !=3)
HHOME$FUTUREX - HHOME$PARGRADEX
sum(HHOME$FUTUREX - HHOME$PARGRADEX)/sum(HHOME$countn)

svymean(~FUTUREX>=1, subset(HOMEdesign, PARGRADEX == 1))
svymean(~FUTUREX>=2, subset(HOMEdesign, PARGRADEX == 2))
svymean(~FUTUREX>=3, subset(HOMEdesign, PARGRADEX == 3))
svymean(~FUTUREX>=4, subset(HOMEdesign, PARGRADEX == 4))
svymean(~FUTUREX>=5, subset(HOMEdesign, PARGRADEX == 5))

svymean(~FUTUREX>=1, subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 1))
svymean(~FUTUREX>=2, subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 2))
svymean(~FUTUREX>=3, subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 3))
svymean(~FUTUREX>=4, subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 4))
svymean(~FUTUREX>=5, subset(PFIdesign, SCHTYPE == 1 & PARGRADEX == 5))

svymean(~FUTUREX>=1, subset(HOMEdesign, ALWAYS == 1 & PARGRADEX == 1))
svymean(~FUTUREX>=2, subset(HOMEdesign, ALWAYS == 1 & PARGRADEX == 2))
svymean(~FUTUREX>=3, subset(HOMEdesign, ALWAYS == 1 & PARGRADEX == 3))
svymean(~FUTUREX>=4, subset(HOMEdesign, ALWAYS == 1 & PARGRADEX == 4))
svymean(~FUTUREX>=5, subset(HOMEdesign, ALWAYS == 1 & PARGRADEX == 5))

svymean(~FUTUREX>=1, subset(HOMEdesign, ALWAYS != 1 & PARGRADEX == 1))
svymean(~FUTUREX>=2, subset(HOMEdesign, ALWAYS != 1 & PARGRADEX == 2))
svymean(~FUTUREX>=3, subset(HOMEdesign, ALWAYS != 1 & PARGRADEX == 3))
svymean(~FUTUREX>=4, subset(HOMEdesign, ALWAYS != 1 & PARGRADEX == 4))
svymean(~FUTUREX>=5, subset(HOMEdesign, ALWAYS != 1 & PARGRADEX == 5))



summary(glm((SEFUTUREX>4) ~ DISABILITY*SES*home_public, family=binomial, data=PFI))

summary(glm((SEFUTUREX>4) ~ SES*home_public, family=binomial, data=PFI))
summary(glm((SEFUTUREX>4) ~ DISABILITY*home_public, family=binomial, data=PFI))



summary(glm((SEFUTUREX>4) ~ (SES==3)*disability, family=binomial, data=HOME))

summary(glm((SEFUTUREX>4) ~ DISABILITY, family=binomial, subset(PFI, home_public == 1)))
summary(glm((SEFUTUREX>4) ~ DISABILITY, family=binomial, subset(PFI, home_public != 1)))




summary(svyglm((SEFUTUREX>4) ~ disability*ALWAYS, family=binomial, design=HOMEdesign))



summary(svyglm((SEFUTUREX>4) ~ SES*elementary_secondary*(ALWAYS==1), family=binomial, design=HOMEdesign))

summary(svyglm((SEFUTUREX>4) ~ home_public*DISABILITY*SES*elementary_secondary, family=binomial, design=PFIdesign))


summary(svyglm((SEFUTUREX>4) ~ SES*elementary_secondary, family=binomial, design=PFIdesign))

summary(svyglm((SEFUTUREX>4) ~ home_public*elementary_secondary, family=binomial, design=PFIdesign))





# CREATE DATA SET for regression

EXPECT <- subset(PFI, SCHTYPE == 1 | SCHTYPE == 3)
which( colnames(EXPECT)=="HOMEKX" ) # checking colnumbers
which( colnames(EXPECT)=="HOME12" ) # should be 59 and 70
EXPECT$TOTAL <- rowSums(EXPECT[ , c(58:70)], na.rm=TRUE)
EXPECT$ALWAYS <- ifelse((EXPECT$TOTAL == (EXPECT$ALLGRADEX + 1)), 1, 0)
sum(HOME$ALWAYS) # testing
sum(EXPECT$ALWAYS) # these should be identical
EXPECT$BA <- ifelse(EXPECT$SEFUTUREX>4, 1, 0)
EXPECT$HIGHSES <- ifelse(EXPECT$SES == 3, 1, 0)
# turn relevant columns into integers
EXPECT$BA <- as.integer(EXPECT$BA)
EXPECT$ALWAYS <- as.integer(EXPECT$ALWAYS)
EXPECT$DISABILITY <- as.integer(EXPECT$DISABILITY)
EXPECT$HIGHSES <- as.integer(EXPECT$HIGHSES)
EXPECT$home_public <- as.integer(EXPECT$home_public)
EXPECT$SCHTYPE <- as.integer(EXPECT$SCHTYPE)

# CREATE design object from new data set
EXPECTdesign <- svrepdesign(
  data = EXPECT, 
  repweights = subset(EXPECT, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(EXPECTdesign)

is.integer(EXPECT$HIGHSES)
is.integer(EXPECT$ALWAYS)
is.integer(EXPECT$DISABILITY)
is.integer(EXPECT$BA)
is.integer(EXPECT$SCHTYPE)

# just disability
summary(svyglm((BA) ~ DISABILITY, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 1)))
summary(svyglm((BA) ~ DISABILITY, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 3)))
# just high SES
summary(svyglm((BA) ~ HIGHSES, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 1)))
summary(svyglm((BA) ~ HIGHSES, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 3)))
# just home_public
summary(svyglm((BA) ~ home_public, family=quasibinomial, EXPECTdesign))
# each factor separately
summary(svyglm((BA) ~ home_public+DISABILITY+HIGHSES, family=quasibinomial, EXPECTdesign))
# interactions
summary(svyglm((BA) ~ home_public*HIGHSES, family=quasibinomial, EXPECTdesign))
summary(svyglm((BA) ~ home_public*DISABILITY, family=quasibinomial, EXPECTdesign))
summary(svyglm((BA==1) ~ home_public*DISABILITY*HIGHSES, family=quasibinomial, EXPECTdesign))

summary(svyglm((BA==1) ~ home_public*DISABILITY, family=quasibinomial, 
                         subset(EXPECTdesign, HIGHSES != 1)))



# homeschool only
summary(svyglm((BA) ~ ALWAYS*DISABILITY*HIGHSES, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 3)))
summary(svyglm((BA) ~ ALWAYS*DISABILITY, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 3)))
summary(svyglm((BA) ~ DISABILITY*HIGHSES, family=quasibinomial, 
               subset(EXPECTdesign, SCHTYPE == 3)))


# independent factors
GLM1 <- svyglm((BA) ~ (home_public==1)+(DISABILITY==1)+(HIGHSES==1), family=quasibinomial, EXPECTdesign)
round(exp(coef(GLM1)), digits = 2)




# basic comparisons

# ses only
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3),
         na.rm=TRUE)

# disabilities only
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 3 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 1 & elementary_secondary == 2))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, DISABILITY == 1 & elementary_secondary == 2),
         na.rm=TRUE)

svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY != 1 & SCHTYPE == 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY != 1 & SCHTYPE == 1))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, DISABILITY != 1),
         na.rm=TRUE)


svymean(~disability == 1, subset(PFIdesign, elementary_secondary == 1))

svymean(~SEFUTUREX>4, subset(HOMEdesign, elementary_secondary == 2 & disability == 1))
svymean(~SEFUTUREX>4, subset(PFIdesign, elementary_secondary == 2 & DISABILITY == 1 & SCHTYPE == 1))

svymean(~SEFUTUREX>4, subset(HOMEdesign, elementary_secondary == 2 & disability != 1))
svymean(~SEFUTUREX>4, subset(PFIdesign, elementary_secondary == 2 & DISABILITY != 1 & SCHTYPE == 1))


svymean(~SEFUTUREX>4, subset(HOMEdesign, SES == 3 & disability != 1 & ALWAYS == 1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES == 3 & disability != 1 & ALWAYS == 1 &
                             elementary_secondary == 2))
svymean(~SEFUTUREX>4, subset(PFIdesign, SES == 3 & DISABILITY != 1 & #public school
                               elementary_secondary == 2))


svymean(~SEFUTUREX>4, subset(HOMEdesign, SES == 3 & ALWAYS == 1))
svymean(~SEFUTUREX>4, subset(PFIdesign, SES == 3))

svymean(~(SES == 3 & ALWAYS == 1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(SES != 3), subset(HOMEdesign, elementary_secondary==2))
svymean(~(ALWAYS != 1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(SES != 3 & ALWAYS == 1), subset(HOMEdesign, elementary_secondary==2))
svymean(~(SES == 3 & ALWAYS != 1), subset(HOMEdesign, elementary_secondary==2))

# low/middle SES transfers
svymean(~(SES != 3 & ALWAYS != 1), subset(HOMEdesign, elementary_secondary==2))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                            SES != 3 & ALWAYS != 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, elementary_secondary==2 & 
                                 SES != 3))

# all low/middle SES, grades 7-12
svymean(~(SES != 3), subset(HOMEdesign, elementary_secondary==2))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, elementary_secondary==2 & 
                                 SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, elementary_secondary==2 & SES != 3),
         na.rm=TRUE)

# low/middle transfers, disability or no
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3 & ALWAYS != 1 & disability == 1))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3 & ALWAYS != 1 & disability != 1))

svymean(~(disability==1), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3 & ALWAYS != 1))


cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3 & ALWAYS != 1 & disability == 1)))

# low/middle ses 7-12 disability, homeschool and public school
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3 & disability == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==2 & 
                                 SES != 3 & DISABILITY == 1))
# high ses 7-12 disability, homeschool and public school
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES == 3 & disability == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==2 & 
                                 SES == 3 & DISABILITY == 1))

# low/middle ses K-6 disability, homeschool and public school
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==1 & 
                                 SES != 3 & disability == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==1 & 
                                 SES != 3 & DISABILITY == 1))
svyttest((SEFUTUREX>4) ~ (home_public), 
          subset(PFIdesign, elementary_secondary==1 & SES != 3 & DISABILITY == 1),
          na.rm=TRUE)
# high ses K-6 disability, homeschool and public school
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==1 & 
                                 SES == 3 & disability == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==1 & 
                                 SES == 3 & DISABILITY == 1))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, elementary_secondary==1 & SES == 3 & DISABILITY == 1),
         na.rm=TRUE)

# something
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES == 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==2 & 
                                 SES == 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, elementary_secondary==2 & SES == 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & 
                                 SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==2 & 
                                 SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, elementary_secondary==2 & SES != 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(HOMEdesign, elementary_secondary==2 & disability != 1 &
                                 SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 & elementary_secondary==2 & disability != 1 &
                                 SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, elementary_secondary==2 & DISABILITY != 1 & SES != 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                                 SES != 3 & elementary_secondary==1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 &
                                 SES != 3 & elementary_secondary==1))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & elementary_secondary==1),
         na.rm=TRUE)

svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               SES != 3 & ALWAYS==1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 &
                                 SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & elementary_secondary==1),
         na.rm=TRUE)

svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               HSINTNET==1 & SES !=3))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               HSINTNET==1 & SES ==3))


svymean(~(HSINTNET==1 & SES !=3), HOMEdesign)


svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               HSINTNET==1 & SES != 3))


part <- subset(HOME, SES !=3)
table(part$HSINTNET, part$SEFUTUREX)

svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 &
                                 SES != 3))

svymean(~(SES==2), subset(HOMEdesign, 
                               SES != 3))
svymean(~(SES==2), subset(PFIdesign, SCHTYPE == 1 &
                                 SES != 3))
svyttest((SES==2) ~ (home_public), 
         subset(PFIdesign, SES != 3),
         na.rm=TRUE)

svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 &
                                 SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               SES == 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SCHTYPE == 1 &
                                 SES == 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3),
         na.rm=TRUE)



svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               ALWAYS != 1))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               ALWAYS == 1))
svyttest((SEFUTUREX>4) ~ (ALWAYS == 1), 
         HOMEdesign,
         na.rm=TRUE)

svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               ALWAYS != 1 & SES == 3))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, 
                               ALWAYS == 1 & SES == 3))
svyttest((SEFUTUREX>4) ~ (ALWAYS == 1), 
         subset(HOMEdesign, SES != 3),
         na.rm=TRUE)


svymean(~(SES != 3 & disability == 1), subset(HOMEdesign, elementary_secondary==2))
svymean(~(SES != 3 & disability != 1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(SES == 3 & disability == 1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(SES != 3), subset(PFIdesign, elementary_secondary==2 & DISABILITY == 1))

svymean(~(ALWAYS!=1), subset(HOMEdesign, elementary_secondary==2 & disability == 1 &
                             SES != 3))
svymean(~(ALWAYS!=1), subset(HOMEdesign, elementary_secondary==1 & disability == 1 &
                               SES != 3))

svymean(~(ALWAYS!=1), subset(HOMEdesign, elementary_secondary==2 & disability == 1))
svymean(~(ALWAYS!=1), subset(HOMEdesign, elementary_secondary==1 & disability == 1))

svymean(~(HSDISABLX==1), subset(HOMEdesign, elementary_secondary==1))
svymean(~(HSDISABLX==1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(HSDISABLX==1), subset(HOMEdesign, elementary_secondary==1 & SES != 3))
svymean(~(HSDISABLX==1), subset(HOMEdesign, elementary_secondary==2 & SES != 3))





round((svytable(~(TOTAL), subset(HOMEdesign, elementary_secondary==2 & disability == 1 &
                               SES != 3))) / 
  sum(svytable(~(TOTAL), subset(HOMEdesign, elementary_secondary==2 & disability == 1 &
                                    SES != 3))), digits = 3)

part <- subset(HOME, elementary_secondary==2 & SES!=3 & disability == 1)
part$ALLGRADEX - part$TOTAL + 1

round((svytable(~FIRSTyr, subset(HOMEdesign, elementary_secondary==2 & SES!=3 & disability == 1)) /
  sum(svytable(~FIRSTyr, subset(HOMEdesign, elementary_secondary==2 & SES!=3 & disability == 1))))*100)


svymean(~(disability == 1), subset(HOMEdesign, elementary_secondary==2 & SES != 3))


# has disability, high SES and low/middle SES
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 3 & SES == 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 1 & SES == 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, DISABILITY == 1 & SES == 3),
         na.rm=TRUE)
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 3 & SES != 3))
svymean(~(SEFUTUREX>4), subset(PFIdesign, DISABILITY == 1 & SCHTYPE == 1 & SES != 3))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, DISABILITY == 1 & SES != 3),
         na.rm=TRUE)

# background on students by age
svymean(~(disability==1), subset(HOMEdesign, elementary_secondary==1))
svymean(~(disability==1), subset(HOMEdesign, elementary_secondary==2))

svymean(~(SES!=3), subset(HOMEdesign, elementary_secondary==1))
svymean(~(SES!=3), subset(HOMEdesign, elementary_secondary==2))

svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX < 3))
svymean(~(disability==1), subset(HOMEdesign, ALLGRADEX > 8))

svymean(~(SES==3), subset(HOMEdesign, ALLGRADEX < 3))
svymean(~(SES==3), subset(HOMEdesign, ALLGRADEX > 8))

svymean(~(ALWAYS == 1), subset(HOMEdesign, ALLGRADEX < 3))
svymean(~(ALWAYS == 1), subset(HOMEdesign, ALLGRADEX > 8))

# quick run of expectations
# grades K-6, disability
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 1)))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3 & DISABILITY == 1 & elementary_secondary == 1),
         na.rm=TRUE)
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & DISABILITY == 1 & elementary_secondary == 1),
         na.rm=TRUE)

# grades K-6, no disability
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability != 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability != 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 1))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 1))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability != 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability != 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 1)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 1)))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3 & DISABILITY != 1 & elementary_secondary == 1),
         na.rm=TRUE)
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & DISABILITY != 1 & elementary_secondary == 1),
         na.rm=TRUE)

# grades 7-12, disability
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability == 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability == 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 2))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability == 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability == 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY == 1 & elementary_secondary == 2)))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3 & DISABILITY == 1 & elementary_secondary == 2),
         na.rm=TRUE)
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & DISABILITY == 1 & elementary_secondary == 2),
         na.rm=TRUE)

# grades 7-12, no disability
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability != 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability != 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 2))
svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 2))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES == 3 & 
                                 disability != 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(HOMEdesign, SES != 3 & 
                                 disability != 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES == 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 2)))
cv(svymean(~(SEFUTUREX>4), subset(PFIdesign, SES != 3 & SCHTYPE == 1 &
                                 DISABILITY != 1 & elementary_secondary == 2)))
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES == 3 & DISABILITY != 1 & elementary_secondary == 2),
         na.rm=TRUE)
svyttest((SEFUTUREX>4) ~ (home_public), 
         subset(PFIdesign, SES != 3 & DISABILITY != 1 & elementary_secondary == 2),
         na.rm=TRUE)




# END SCRIPT