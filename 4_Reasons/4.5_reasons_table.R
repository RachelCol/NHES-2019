# REASONS TABLE
# CREATE a table showing what percent of those who selected each reason for
# homeschooling also also selected each OTHER reason for homeschooling.

# note: This script is designed to run after 0_data_subsets script.

category <- HOME$HSSAFETYX
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Safety <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$HSDISSATX
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Academic <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$HSRELGON
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Religion <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$HSMORAL
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Moral <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$disability
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Disability <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$HSALTX
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
AltEd <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

category <- HOME$HSFMLY
saf <- as.data.frame(svymean(~HSSAFETYX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
aca <- as.data.frame(svymean(~HSDISSATX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
rel <- as.data.frame(svymean(~HSRELGON == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
mor <- as.data.frame(svymean(~HSMORAL == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
dis <- as.data.frame(svymean(~disability == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
alt <- as.data.frame(svymean(~HSALTX == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
fam <- as.data.frame(svymean(~HSFMLY == 1, subset(HOMEdesign, category == 1), na.rm=TRUE))
Family <- round(c(saf[2,1], aca[2,1], rel[2,1], mor[2,1], dis[2,1], alt[2,1], fam[2,1]), digits = 3)

# create table using items created above
ReasonsChart <- rbind(Safety, Academic, Religion, Moral, Disability, AltEd, Family)
ReasonsPercent <- ReasonsChart*100
colnames(ReasonsPercent) <- c("Safety", "Academic", "Religion", "Moral", "Disability", "AltEd", "Family")
ReasonsPercent

# CREATE CHART
# create color scheme for heat map
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
# create chart
reasons_chart <- reactable(ReasonsPercent, defaultColDef = colDef(
  style = function(value) {
    if (!is.numeric(value)) return()
    normalized <- (value - min(ReasonsPercent)) / (max(ReasonsPercent) - min(ReasonsPercent))
    color <- BuYlRd(normalized)
    list(background = color)},
  format = colFormat(digits = 1),
  minWidth = 50),
  columns = list(.rownames = colDef(name = "", sortable = TRUE, align = "left")),
  bordered = TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
   searchInputStyle = list(width = "100%")))
 
save_reactable(reasons_chart, "charts/reasons_chart.png")

# write.csv(ReasonsPercent,"/Users/Rachel/R-Projects/NHES-2019/ReasonsPercent.csv", row.names = TRUE)

# END TABLE on what percent also selected other options