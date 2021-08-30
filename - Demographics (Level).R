# DEMOGRAPHICS by level, homeschooled students only
# compare elementary (grades 1-6) with secondary (grades 7-12)

# Note: this script does not suffer from any subsetting errors

# Create subsets to enable comparison of homeschooled elementary and secondary

HOME <- subset(PFI, SCHTYPE == 3)

EL_HS <- subset(HOME, elementary_secondary == 1)
SEC_HS <- subset(HOME, elementary_secondary == 2)

# ...

# Child's race, homeschool elementary v. secondary

wpct(EL_HS$white_nonwhite, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$white_nonwhite, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((white_nonwhite == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((white_nonwhite == 2) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ... 

# Parent has a bachelor's degree, homeschool elementary v. secondary

wpct(EL_HS$ba_no_ba, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$ba_no_ba, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# Parent has a bachelor's degree, white homeschooled students

EL_HS_W <- subset(EL_HS, white_nonwhite == 1) # create a white subset
SEC_HS_W <- subset(SEC_HS, white_nonwhite == 1) # create a white subset

wpct(EL_HS_W$ba_no_ba, weight=EL_HS_W$FPWT, na.rm=TRUE) 
wpct(SEC_HS_W$ba_no_ba, weight=SEC_HS_W$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 1),
         na.rm=TRUE)

# Parent has a bachelor's degree, minority homeschooled students

EL_HS_M <- subset(EL_HS, white_nonwhite == 2) # create a minority subset
SEC_HS_M <- subset(SEC_HS, white_nonwhite == 2) # create a minority subset

wpct(EL_HS_M$ba_no_ba, weight=EL_HS_M$FPWT, na.rm=TRUE) 
wpct(SEC_HS_M$ba_no_ba, weight=SEC_HS_M$FPWT, na.rm=TRUE)

svyttest((ba_no_ba == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3 & 
                  white_nonwhite == 2),
         na.rm=TRUE)

# ... 

# Household structure, homeschool elementary v. secondary

wpct(EL_HS$two_parent_or_single, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$two_parent_or_single, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((two_parent_or_single == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ... 

# Households with a SAHP, homeschool elementary v. secondary

wpct(EL_HS$sahp, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$sahp, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((sahp == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# ... 

# Household Income
# NO SIGNIFICANT DIFFERENCE

wpct(EL_HS$income, weight=EL_HS$FPWT, na.rm=TRUE)
wpct(SEC_HS$income, weight=SEC_HS$FPWT, na.rm=TRUE)

svyttest((income == 1) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 2) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 3) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

svyttest((income == 4) ~ elementary_secondary, 
         subset(PFIdesign, SCHTYPE == 3),
         na.rm=TRUE)

# END COMPARISON BY SCHOOL LEVEL