# Years homeschooled

HOME <- subset(PFI, SCHTYPE == 3)

# create a data subset with only the columns for years
YEARS <- HOME[,c(59:70)]

# remove weird formatting
YEARS <- ifelse(YEARS == 1, 1, 0)

# make into a data frame
YEARS <- as.data.frame(YEARS)

# replace NAs with 0
YEARS[is.na(YEARS)] <- 0

YEARS$HOME1 <- as.numeric(YEARS$HOME1)
YEARS$HOME2 <- as.numeric(YEARS$HOME2)

YEARS$TOTAL <- YEARS$HOME1 + 
                YEARS$HOME2 + 
                YEARS$HOME3 + 
                YEARS$HOME4 + 
                YEARS$HOME5 + 
                YEARS$HOME6 + 
                YEARS$HOME7 + 
                YEARS$HOME8 + 
                YEARS$HOME9 + 
                YEARS$HOME10 + 
                YEARS$HOME11 + 
                YEARS$HOME12

table(YEARS$TOTAL)

# Add totals column to Home subset
HOME$years <- YEARS$TOTAL

table(HOME$ALLGRADEX, HOME$years)

wpct(HOME$years, weight=HOME$FPWT, na.rm=TRUE)

high_school <- subset(HOME, ALLGRADEX > 8)
wpct(high_school$HOME6, weight=high_school$FPWT, na.rm=TRUE)

wpct(high_school$HOME6, weight=high_school$FPWT, na.rm=TRUE)
wpct(high_school$HOME7, weight=high_school$FPWT, na.rm=TRUE)
wpct(high_school$HOME8, weight=high_school$FPWT, na.rm=TRUE)



high_school <- subset(HOME, ALLGRADEX > 8)
wpct(high_school$years, weight=high_school$FPWT, na.rm=TRUE)





grade_1 <- subset(HOME, ALLGRADEX == 1)
wpct(grade_1$years, weight=grade_1$FPWT, na.rm=TRUE)

grade_2 <- subset(HOME, ALLGRADEX == 2)
wpct(grade_2$years, weight=grade_2$FPWT, na.rm=TRUE)

grade_3 <- subset(HOME, ALLGRADEX == 3)
wpct(grade_3$years, weight=grade_3$FPWT, na.rm=TRUE)

grade_4 <- subset(HOME, ALLGRADEX == 4)
wpct(grade_4$years, weight=grade_4$FPWT, na.rm=TRUE)

grade_5 <- subset(HOME, ALLGRADEX == 5)
wpct(grade_5$years, weight=grade_5$FPWT, na.rm=TRUE)

grade_6 <- subset(HOME, ALLGRADEX == 6)
wpct(grade_6$years, weight=grade_6$FPWT, na.rm=TRUE)

grade_7 <- subset(HOME, ALLGRADEX == 7)
wpct(grade_7$years, weight=grade_7$FPWT, na.rm=TRUE)

grade_8 <- subset(HOME, ALLGRADEX == 8)
wpct(grade_8$years, weight=grade_8$FPWT, na.rm=TRUE)

grade_9 <- subset(HOME, ALLGRADEX == 9)
wpct(grade_9$years, weight=grade_9$FPWT, na.rm=TRUE)

grade_10 <- subset(HOME, ALLGRADEX == 10)
wpct(grade_10$years, weight=grade_10$FPWT, na.rm=TRUE)

grade_11 <- subset(HOME, ALLGRADEX == 11)
wpct(grade_11$years, weight=grade_11$FPWT, na.rm=TRUE)

grade_12 <- subset(HOME, ALLGRADEX == 12)
wpct(grade_12$years, weight=grade_12$FPWT, na.rm=TRUE)


