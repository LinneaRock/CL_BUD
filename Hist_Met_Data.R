library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
source("Functions/MET.R")

data <- read.csv("Data/Historical_External/Mad_Met_His.csv") %>%
  mutate(date = as.Date(as.yearmon(DATE))) %>%
  mutate(Y = factor(year(date)),
         M = month(date))



#Monthly Mean Temp
linepl(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))
bar(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))


  

