
library(tidyverse)


# Create a boxplot of the dataset where outliers are shown as distinct points 
#add $out after the end parenthesis to get a dataset with all outliers according to this boxplot
boxplot(loggerYN$sp.cond)
boxplot(loggerYI$sp.cond)
boxplot(loggerYS$sp.cond)
boxplot(loggerSW$sp.cond)
boxplot(logger6MC$sp.cond)
boxplot(loggerDC$sp.cond)
boxplot(loggerPBMS$sp.cond)
boxplot(loggerPBSF$sp.cond)

#Also visual checks on outliers using cond(loggerXX) functions in rivers.R

outliers <- function(data) {
  data <- data %>%
    filter(ifelse(
      sp.cond[i] > (sp.cond[i +1] * 4) && sp.cond[i] > (sp.cond[i-1])
    ))
}

mean(loggerYI$sp.cond)
