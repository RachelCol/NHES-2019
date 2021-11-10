# PLOTS for RETENTION

# note: This script is designed to run after 0_data_subsets script.

# -----

# NUMBER OF CHILDREN HOMESCHOOLED

# Number of children homeschooled in each grade
round(svytable(~ALLGRADEX, HOMEdesign))
# Total number of children homeschooled
sum(round(svytable(~ALLGRADEX, HOMEdesign)))

# BAR CHART CREATION BY SEGMENT
# GRADE RANGES: K-6, 7-12
round(svytable(~elementary_secondary, HOMEdesign))
(round(wpct(HOME$elementary_secondary, weight=HOME$FPWT, na.rm=TRUE), digits = 3)*100)
# create a chart 
two_ranges <- (round(svytable(~elementary_secondary, HOMEdesign))/1000)
pdf(file="charts/num_hsed_1.pdf")
barplot(two_ranges, main="Number of children homeschooled, by grade range", 
        ylab = "number in thousands",
        names.arg = c("K-6th", "7th-12th"))
dev.off()

# GRADE RANGES: K-2, 3-5, 6-8, 9-12
round(svytable(~grade_range, HOMEdesign))
(round(wpct(HOME$grade_range, weight=HOME$FPWT, na.rm=TRUE), digits = 3)*100)
# create a chart 
four_ranges <- (round(svytable(~grade_range, HOMEdesign))/1000)
pdf(file="charts/num_hsed_2.pdf")
barplot(four_ranges, main="Number of children homeschooled, by grade range", 
        ylab = "number in thousands",
        names.arg = c("K-2nd", "3rd-5th", "6th-8th", 
                      "9th-12th"))
dev.off()

# GRADE RANGES: 1-2, 3-4, 5-6, 7-8, 9-10, 11-12
round(svytable(~grade_range2, HOMEdesign))
(round(wpct(HOME$grade_range2, weight=HOME$FPWT, na.rm=TRUE), digits = 3)*100)
# create a chart
six_ranges <- (round(svytable(~grade_range2, subset(HOMEdesign, ALLGRADEX > 0)))/1000)
pdf(file="charts/num_hsed_3.pdf")
barplot(six_ranges, main="Number of children homeschooled, by grade range", 
        ylab = "number in thousands",
        names.arg = c("1st-2nd", "3rd-4th", "5th-6th", 
                      "7th-8th", "9th-10th", "11th-12th"))
dev.off()

# -----

# NUMBER OF CHILDREN IN THEIR FIRST YEAR OF HOMESCHOOLING

# Number of children homeschooled in each grade
round(svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1)))
# Total number of children in their first year of homeschooling
sum(round(svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1))))
# Percent in their first year of homeschooling, by grade
round(((svytable(~ALLGRADEX, subset(HOMEdesign, FIRST == 1))) / 
         svytable(~ALLGRADEX, HOMEdesign)*100), digits = 1)

# BAR CHART CREATION BY SEGMENT

# K-6, 7-12: Percent in their first year of homeschooling
two_rangesFY <- (round(svytable(~elementary_secondary, subset(HOMEdesign, FIRST == 1)) /
                         svytable(~elementary_secondary, HOMEdesign), digits = 3)*100)
# create a chart 
pdf(file="charts/first_year_percent_1.pdf")
barplot(two_rangesFY, main="Percent in their first year of homeschooling", 
        ylab = "percent",
        names.arg = c("K-6th", "7th-12th"))
dev.off()

# K-2, 3-5, 6-8, 9-12: Percent in their first year of homeschooling
four_rangesFY <- (round(svytable(~grade_range, subset(HOMEdesign, FIRST == 1)) /
                          svytable(~grade_range, HOMEdesign), digits = 3)*100)
# create a chart 
pdf(file="charts/first_year_percent_2.pdf")
barplot(four_rangesFY, main="Percent in their first year of homeschooling", 
        ylab = "percent",
        names.arg = c("K-2nd", "3rd-5th", "6th-8th", 
                      "9th-12th"))
dev.off()

# 1-3, 4-6, 7-9, 10-12: Percent in their first year of homeschooling
four_rangesFY <- (round(svytable(~grade_range3, subset(HOMEdesign, FIRST == 1)) /
                          svytable(~grade_range3, HOMEdesign), digits = 3)*100)
# create a chart 
pdf(file="charts/first_year_percent_3.pdf")
barplot(four_rangesFY, main="Percent in their first year of homeschooling", 
        ylab = "percent",
        names.arg = c("1st-3rd", "4th-6th", "7th-9th", 
                      "10th-12th"))
dev.off()

# -----

# WEIGHTED NUMBER OF CHILDREN HOMESCHOOLED IN EACH GRADE 
# create table, create bar plot with confidence intervals

# CREATE TABLE showing the number of students in each grade, in thousands
levels <- round((svytable(~ALLGRADEX, HOMEdesign))/1000)
levels <- as.data.frame(levels)
levels

# What percent of homeschooled students are in each grade?
svymean(~ALLGRADEX == 0, HOMEdesign)
svymean(~ALLGRADEX == 1, HOMEdesign)
svymean(~ALLGRADEX == 2, HOMEdesign)
svymean(~ALLGRADEX == 3, HOMEdesign)
svymean(~ALLGRADEX == 4, HOMEdesign)
svymean(~ALLGRADEX == 5, HOMEdesign)
svymean(~ALLGRADEX == 6, HOMEdesign)
svymean(~ALLGRADEX == 7, HOMEdesign)
svymean(~ALLGRADEX == 8, HOMEdesign)
svymean(~ALLGRADEX == 9, HOMEdesign)
svymean(~ALLGRADEX == 10, HOMEdesign)
svymean(~ALLGRADEX == 11, HOMEdesign)
svymean(~ALLGRADEX == 12, HOMEdesign)

# These standard errors come from the calculations directly above.
se <- c(0.0169, 0.0144, 0.0146, 0.013, 0.0173, 0.0151, 0.0211, 0.0213, 0.0111, 0.0141, 0.0136, 0.009, 0.0153)
# standard error * total number of respondents * 1.96 = standard deviation
sd <- (se*sum(HOME$FPWT)*1.96)/1000

grades <- cbind(levels, sd)

# Create bar graph
ggplot(data=grades, aes(x=ALLGRADEX, y=Freq, group=1)) +
  geom_bar(stat="identity", width=.85, fill="steelblue")+
  labs(title="Number of children homeschooled by grade, \n with 95% confidence intervals", 
       x="grades (0 = K)", y = "number in thousands") +
  theme_minimal() + 
  geom_errorbar(aes(x=ALLGRADEX, ymin=Freq-sd, ymax=Freq+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

# END WEIGHTED NUMBER OF HOMESCHOOLED CHILDREN IN EACH GRADE PLOT

# -----

# Troubleshooting grade 11 oddities:
# What percent of 12th graders were always homeschooled?
YearsTable[13, 13]/sum(YearsTable[, 13])
# What percent of 12th graders are first-year homeschoolers?
YearsTable[1, 13]/sum(YearsTable[, 13])
# CREATE a table of years homeschooled and grades, counts only
CountsTable <- round(table(HOME$TOTAL, HOME$ALLGRADEX))
CountsTable
# note: for mean weights by grade, see "Weights" script
# end troubleshooting of grade 11 oddities

# -----

# CREATE TABLE AND BAR PLOT WITH NUMBER OF RESPONDENTS IN EACH GRADE

# Number of respondents homeschooled in each grade
resp <- table(HOME$ALLGRADEX)
resp <- as.data.frame(resp)

# Create bar graph
ggplot(data=resp, aes(x=Var1, y=Freq, group=1)) +
  geom_bar(stat="identity", width=.85, fill="steelblue")+
  labs(title="Homeschooled respondents by grade, NHES:2019", 
       x="grades (K = 0)", y = "number of respondents") +
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

# END PLOT SHOWING NUMBER OF STUDENTS IN EACH GRADE

# -----

# FIRST-YEAR HOMESCHOOLERS BY GRADE
# create table, create bar plot 

FirstTable <- round(svytable(~ FIRST + ALLGRADEX, HOMEdesign))
rownames(FirstTable) <- c("Long", "Fyr")
FirstTable <- t(FirstTable)
FirstTable <- as.data.frame.matrix(FirstTable)
FirstTable$total <- FirstTable$Fyr + FirstTable$Long
FirstTable$percent <- (round(FirstTable$Fyr/(FirstTable$total), digits = 3)*100)
FirstTable$grade <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
FirstTable <- FirstTable[-1, ]
FirstTable

FirstTableChart <- ggplot(data = FirstTable, aes(x = grade, y = percent, group = 1)) +
  geom_bar(stat="identity", width=.85, fill="lightgray") +
  geom_line(linetype = "dashed", colour="darkgray") + geom_point() + 
  geom_text(aes(label=percent, fontface="bold"), hjust=.5, vjust=-.7) +
  xlab('grade') +
  ylab('percent') +
  scale_y_continuous(limits = c(0,100)) + 
  scale_x_discrete(name ="grade", limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  ggtitle("First-time homeschooled students in each grade, \n as a percent of all homeschooled students") 
print(FirstTableChart)

# END SCRIPT