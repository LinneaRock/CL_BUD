library(tsoutliers)
# Create a boxplot of the dataset where outliers are shown as distinct points 
#add $out after the end parenthesis to get a dataset with all outliers according to this boxplot
boxplot(loggerYN$Full.Range)
boxplot(loggerYI$Full.Range)
boxplot(loggerYS$Full.Range)
boxplot(loggerSW$Full.Range)
boxplot(logger6MC$Full.Range)
boxplot(loggerDC$Full.Range)
boxplot(loggerPBMS$Full.Range)
boxplot(loggerPBSF$Full.Range)

#Also visual checks on outliers using cond_compare(loggerXX) functions in rivers.R
#excluding outliers from the raw actaul conductivity data (what the logger measures), rather than specific conductivity, which is calculated later


quantile(loggerYI$Full.Range, c(.01, .99))

#Finding the highest and lowest 
subYN <- loggerYN %>%
  filter(Full.Range > 1.25 * quantile(Full.Range, .99))

subYI <- loggerYI %>%
  filter(Full.Range < .5 * quantile(Full.Range, .01) |
           Full.Range > 1.25 * quantile(Full.Range, .99))
library(plyr)
loggery <- loggerYI[!loggerYI$Full.Range %in% subYI$Full.Range, ]
detach("package:plyr", unload = TRUE)


cond_compare(fieldcondYI, loggery)
cond(loggery)

cond(loggerYI)


YIoutliertest <- tso(loggerYI$Full.Range)
