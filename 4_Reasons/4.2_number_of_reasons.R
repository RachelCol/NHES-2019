# NUMBER OF REASONS selected for homeschooling

# note: This script is designed to run after 0_data_subsets script.

# note: This script uses the numberR variable created in 0_data_subsets.

# -----

# average number of reasons selected
sum(HOME[, 71:80] == 1)/sum(HOME$countn == 1)

# excluding "other"
sum(HOME[, 71:79] == 1)/sum(HOME$countn == 1)

# AVERAGE NUMBER OF REASONS, by subset
# by reason for homeschooling
sum(HOME[, 71:80] == 1 & HOME$HSRELGON == 1)/sum(HOME$countn == 1 & HOME$HSRELGON == 1)
sum(HOME[, 71:80] == 1 & HOME$HSRELGON != 1)/sum(HOME$countn == 1 & HOME$HSRELGON != 1)
sum(HOME[, 71:80] == 1 & HOME$HSALTX == 1)/sum(HOME$countn == 1 & HOME$HSALTX == 1)
sum(HOME[, 71:80] == 1 & HOME$HSALTX != 1)/sum(HOME$countn == 1 & HOME$HSALTX != 1)
sum(HOME[, 71:80] == 1 & HOME$disability == 1)/sum(HOME$countn == 1 & HOME$disability == 1)
sum(HOME[, 71:80] == 1 & HOME$disability != 1)/sum(HOME$countn == 1 & HOME$disability != 1)
# first year transfer v. some years transfer v. always homeschooling v. all transfers
sum(HOME[, 71:80] == 1 & HOME$FIRST == 1 & HOME$ALLGRADEX > 0)/sum(HOME$countn == 1 & HOME$FIRST == 1 & HOME$ALLGRADEX > 0)
sum(HOME[, 71:80] == 1 & HOME$FIRST == 0 & HOME$ALWAYS == 0)/sum(HOME$countn == 1 & HOME$FIRST == 0 & HOME$ALWAYS == 0)
sum(HOME[, 71:80] == 1 & HOME$ALWAYS == 1)/sum(HOME$countn == 1 & HOME$ALWAYS == 1)
sum(HOME[, 71:80] == 1 & HOME$ALWAYS != 1)/sum(HOME$countn == 1 & HOME$ALWAYS != 1)
# college v. no, poverty, elementary v. secondary
sum(HOME[, 71:80] == 1 & HOME$ba_no_ba == 1)/sum(HOME$countn == 1 & HOME$ba_no_ba == 1)
sum(HOME[, 71:80] == 1 & HOME$ba_no_ba != 1)/sum(HOME$countn == 1 & HOME$ba_no_ba != 1)
sum(HOME[, 71:80] == 1 & HOME$poverty == 1)/sum(HOME$countn == 1 & HOME$poverty == 1)
sum(HOME[, 71:80] == 1 & HOME$poverty != 3)/sum(HOME$countn == 1 & HOME$poverty != 3)
sum(HOME[, 71:80] == 1 & HOME$poverty == 3)/sum(HOME$countn == 1 & HOME$poverty == 3)
sum(HOME[, 71:80] == 1 & HOME$elementary_secondary == 1)/sum(HOME$countn == 1 & HOME$elementary_secondary == 1)
sum(HOME[, 71:80] == 1 & HOME$elementary_secondary != 1)/sum(HOME$countn == 1 & HOME$elementary_secondary != 1)

# WHY do religious homeschoolers select more reasons?
svymean(~numberR, subset(HOMEdesign, HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, HSRELGON != 1), na.rm=TRUE)

# By college degree, religion
svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba == 1 & HSRELGON == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, ba_no_ba != 1 & HSRELGON == 2), na.rm=TRUE)

# By elementary v. secondary, religion
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary != 1 & HSRELGON == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary == 1 & HSRELGON == 2), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, elementary_secondary != 1 & HSRELGON == 2), na.rm=TRUE)

# Two parents v. single parent
svymean(~numberR, subset(HOMEdesign, two_parent_or_single == 1), na.rm=TRUE)
svymean(~numberR, subset(HOMEdesign, two_parent_or_single != 1), na.rm=TRUE)

# -----

# What reasons do parents who only choose one reason (other than other) give?
numberR1 <- subset(HOME, numberR==1)
wpct(numberR1$HSSAFETYX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSDISSATX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSRELGON, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSMORAL, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$disability, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSALTX, weight=numberR1$FPWT, na.rm=TRUE)
wpct(numberR1$HSFMLY, weight=numberR1$FPWT, na.rm=TRUE)
# what percent of those who give one reason (other than other) also select other?
wpct(numberR1$HSOTHERX, weight=numberR1$FPWT, na.rm=TRUE)

# What reasons do parents who only choose two reasons (other than other) give?
numberR2 <- subset(HOME, numberR==2)
wpct(numberR2$HSSAFETYX, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$HSDISSATX, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$HSRELGON, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$HSMORAL, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$disability, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$HSALTX, weight=numberR2$FPWT, na.rm=TRUE)
wpct(numberR2$HSFMLY, weight=numberR2$FPWT, na.rm=TRUE)
# what percent of those who give one reason (other than other) also select other?
wpct(numberR2$HSOTHERX, weight=numberR2$FPWT, na.rm=TRUE)

# -----

# BAR GRAPH CREATION, number of reasons selected, religious v. non-religious

# CREATE A CHART of the number of reasons, religious homeschoolers
number <- subset(HOME, HSRELGON==1)
counts <- (round(wpct(number$numberR, weight=number$FPWT, na.rm=TRUE), digits = 3)*100)

pdf(file="4_Reasons/number_of_reasons_religious.pdf")
barplot(counts, main="Number of reasons given for homeschooling \n (parents with religious reasons for homeschooling)", ylab="percent of respondents")
dev.off()

# CREATE A CHART of the number of reasons, NONreligious homeschoolers
number <- subset(HOME, HSRELGON!=1)
counts <- (round(wpct(number$numberR, weight=number$FPWT, na.rm=TRUE), digits = 3)*100)

pdf(file="4_Reasons/number_of_reasons_non_religious.pdf")
barplot(counts, main="Number of reasons given for homeschooling \n (parents without religious reasons for homeschooling)", ylab="percent of respondents")
dev.off()

# END CHART CREATION

# -----


model <- lm((numberR) ~ 
              (elementary_secondary==2) + 
              (ba_no_ba!=1) +
              (HSRELGON == 0), 
            data=HOME)
summary(model)


# -----

# CREATE REGRESSION OBJECT

NUM <- HOME
# turn Relevant columns into integers
NUM$ALWAYS <- as.integer(NUM$ALWAYS)
NUM$DISABILITY <- as.integer(NUM$DISABILITY)
NUM$SES <- as.integer(NUM$SES)
NUM$elementary_secondary <- as.integer(NUM$elementary_secondary)
NUM$FIRST <- as.integer(NUM$FIRST)
NUM$PARGRADEX <- as.integer(NUM$PARGRADEX)
NUM$HSRELGON <- as.integer(NUM$HSRELGON)
NUM$poverty <- as.integer(NUM$poverty)
NUM$numberR <- as.integer(NUM$numberR)

# CREATE design object from new data set
NUMdesign <- svrepdesign(
  data = NUM, 
  repweights = subset(NUM, select = FPWT1:FPWT80), 
  weights= ~FPWT, type="JK1", mse=TRUE, combined.weights=TRUE, 
  scale=79/80)
summary(NUMdesign)

# run regressions

summary(svyglm((numberR>5) ~ (PARGRADEX>3) + (HSRELGON==1) + (poverty==3) + 
                 (ALWAYS==1) + (disability!=1), family=quasibinomial, NUMdesign)) 

summary(svyglm((numberR>5) ~ (PARGRADEX>3) + (HSRELGON==1) + 
                 (ALWAYS==1) + (disability==1), family=quasibinomial, NUMdesign)) 

summary(svyglm((numberR>5) ~ (HSRELGON==1) + (ALWAYS==1) + (disability==1), 
                 family=quasibinomial, NUMdesign)) 

summary(svyglm((numberR>5) ~ (disability==1) + (HSRELGON==1), 
               family=quasibinomial, NUMdesign)) # THIS ONE!

# create function
invlogit <- function(x) {1/(1+exp(-x))}

# Religious reasons, v. non-religious reasons
# religion: 1 = true
# disability: 1 = false
invlogit (-3.62 + 2.68*1 + 3.44*1) - invlogit (-3.62 + 2.68*0 + 3.44*0)

# double checking
svymean(~numberR>5, subset(HOMEdesign, HSRELGON == 1 & disability != 1))
svymean(~numberR>5, subset(HOMEdesign, HSRELGON != 1 & disability != 1))

svymean(~numberR>5, subset(HOMEdesign, HSRELGON == 1 & disability == 1))
svymean(~numberR>5, subset(HOMEdesign, HSRELGON == 1 & disability != 1))

svymean(~numberR>5, subset(HOMEdesign, disability == 1))
svymean(~numberR>5, subset(HOMEdesign, disability != 1))

svymean(~numberR>5, subset(HOMEdesign, HSRELGON == 1))
svymean(~numberR>5, subset(HOMEdesign, HSRELGON != 1))

svymean(~numberR>5, HOMEdesign)

# END script