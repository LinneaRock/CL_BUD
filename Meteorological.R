library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(cowplot)
source("Functions/MET.R")

data <- read.csv("Data/Historical_External/Mad_Met_His.csv") %>%
  mutate(date = as.Date(as.yearmon(DATE))) %>%
  mutate(Y = factor(year(date)),
         M = month(date))



#Monthly Mean Temp
linepl(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))
bar(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))


precip_temp_data <- read.csv("Data/Historical_External/precip_temp.csv") %>%
  mutate(PRCP = PRCP * 25.4) %>%  #inches to mm
  mutate(date = as.POSIXct(as.character(DATE)))
  
precip_data <- read.csv("Data/met_data.csv") %>% 
  mutate(date = as.POSIXct(as.character(DATE)))

precip <- bar(precip_temp_data, precip_temp_data$date, precip_temp_data$PRCP, bquote("Daily Precipitation (mm)"))

