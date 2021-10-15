# REASONS for homeschooling
# with comparison of SES & ALWAYS

# Number of students in each subset
# ALL GRADES
svytotal(~ALWAYS==1, subset(HOMEdesign, SES==3))
svytotal(~ALWAYS==1, subset(HOMEdesign, SES!=3))
# ELEMENTARY
svytotal(~ALWAYS==1, subset(HOMEdesign, SES==3 & elementary_secondary==1))
svytotal(~ALWAYS==1, subset(HOMEdesign, SES!=3 & elementary_secondary==1))
# SECONDARY
svytotal(~ALWAYS==1, subset(HOMEdesign, SES==3 & elementary_secondary==2))
svytotal(~ALWAYS==1, subset(HOMEdesign, SES!=3 & elementary_secondary==2))

part <- subset(HOME, elementary_secondary==2)
table(part$SES, part$ALWAYS)

sum(table(part$SES, part$ALWAYS))

part <- subset(HOME, elementary_secondary==2 & ALWAYS != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

HOMEdesign <- update(HOMEdesign,  pub_vir = ifelse(HSINTPUB == 1, "pub", ifelse((HSINTVRT == 1), "vir", NA)))
svyttest((SEFUTUREX>4) ~ (ALWAYS==1), 
         subset(HOMEdesign, elementary_secondary==2 & SES < 3),
         na.rm=TRUE)

part <- subset(HOME, elementary_secondary==2 & ALWAYS == 1 & SES != 3)
table(part$HSMOSTX, part$ALWAYS)

part <- subset(HOME, HSRELGON != 1)
table(part$ba_no_ba, part$ALWAYS)

part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES != 3)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)

svymean(~HSRELGON==1, subset(HOMEdesign, RACEETH==1 & ba_no_ba==1))
svymean(~HSRELGON==1, subset(HOMEdesign, RACEETH!=1 & ba_no_ba==1))
svymean(~HSRELGON==1, subset(HOMEdesign, RACEETH==1 & ba_no_ba!=1))
svymean(~HSRELGON==1, subset(HOMEdesign, RACEETH!=1 & ba_no_ba!=1))

part <- subset(HOME, ba_no_ba != 1)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, ba_no_ba == 1)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES != 3)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, SES == 3)
round(wpct(part$HSRELGON, weight=part$FPWT, na.rm=TRUE), digits = 3)

svymean(~ba_no_ba == 1, PFIdesign)
svymean(~ba_no_ba == 1, HOMEdesign)

svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON==1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, HSRELGON!=1))

svymean(~ba_no_ba == 1, subset(HOMEdesign, ALWAYS==1))
svymean(~ba_no_ba == 1, subset(HOMEdesign, ALWAYS!=1))

svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON==1))
svymean(~ALWAYS == 1, subset(HOMEdesign, HSRELGON!=1))

part <- subset(HOME, HSRELGON == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, HSRELGON != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, HSRELGON == 1 & ALWAYS == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1 & ALWAYS == 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON == 1 & ALWAYS != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON != 1 & ALWAYS != 1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)



part <- subset(HOME, ba_no_ba != 1 & poverty == 1 & elementary_secondary==2)
round(wpct(part$HSSAFETYX, weight=part$FPWT, na.rm=TRUE), digits = 3)

sum(HOME[, 71:79] == 1 & HOME$ba_no_ba != 1 & HOME$poverty == 1 & HOME$elementary_secondary==2)/sum(HOME$countn == 1 & HOME$ba_no_ba != 1 & HOME$poverty == 1 & HOME$elementary_secondary==2)

part <- subset(HOME, HSRELGON==1 & ALWAYS==1)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)


round(wpct(HOME$SES, weight=HOME$FPWT, na.rm=TRUE), digits = 3)
round(wpct(PFI$SES, weight=PFI$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, two_parent_or_single==2)
round(wpct(part$SES, weight=part$FPWT, na.rm=TRUE), digits = 3)


part <- subset(HOME, HSRELGON==1 & ALWAYS==1)
round(wpct(part$white_nonwhite, weight=part$FPWT, na.rm=TRUE), digits = 3)
part <- subset(HOME, HSRELGON!=1 & ALWAYS!=1)
round(wpct(part$white_nonwhite, weight=part$FPWT, na.rm=TRUE), digits = 3)


part <- subset(HOME, poverty==1 & ba_no_ba!=1 & elementary_secondary!=1)

sum(part$countn * part$FPWT) / sum(HOME$countn * HOME$FPWT)

part <- subset(PFI, poverty==1 & ba_no_ba!=1 & elementary_secondary!=1)
round(wpct(part$HSMOSTX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(PFI, poverty!=1 & ba_no_ba==1 & elementary_secondary!=1)
round(wpct(part$HSMOSTX, weight=part$FPWT, na.rm=TRUE), digits = 3)


# TESTING ON EACH SUBSET
part <- subset(HOME, SES==3 & ALWAYS==1 & elementary_secondary!=1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES!=3 & ALWAYS==1 & elementary_secondary!=1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES==3 & ALWAYS!=1 & elementary_secondary!=1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES!=3 & ALWAYS!=1 & elementary_secondary!=1)
round(wpct(part$HSWHOX, weight=part$FPWT, na.rm=TRUE), digits = 3)

part <- subset(HOME, SES!=3 & ALWAYS==1 & elementary_secondary!=1)
table(part$FPWT)

# ALL GRADES
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSAFETYX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSDISSATX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSRELGON==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSRELGON==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSMORAL==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSMORAL==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~disability==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSDISABLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSILLX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSILLX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSSPCLNDX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSALTX==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSALTX==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))

# ALL GRADES
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~HSFMLY==1, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~HSFMLY==1, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))


# ALL GRADES
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS==1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS==1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS!=1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS!=1))
# ELEMENTARY
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==1))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==1))
# SECONDARY
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS==1 & elementary_secondary==2))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==2))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES==3 & ALWAYS!=1 & elementary_secondary==2))
svymean(~SEFUTUREX>4, subset(HOMEdesign, SES!=3 & ALWAYS!=1 & elementary_secondary==2))





svymean(~HSSTYL==4, subset(HOMEdesign, SES!=3 & ALWAYS==1 & elementary_secondary==1))

svymean(~SES == 1, subset(HOMEdesign, ALWAYS==1 & elementary_secondary==1))
svymean(~SES == 2, subset(HOMEdesign, ALWAYS==1 & elementary_secondary==1))
svymean(~SES == 3, subset(HOMEdesign, ALWAYS==1 & elementary_secondary==1))

svymean(~SES == 1, subset(HOMEdesign, ALWAYS!=1 & elementary_secondary==1))
svymean(~SES == 2, subset(HOMEdesign, ALWAYS!=1 & elementary_secondary==1))
svymean(~SES == 3, subset(HOMEdesign, ALWAYS!=1 & elementary_secondary==1))

