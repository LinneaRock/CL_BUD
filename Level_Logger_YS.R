library(tidyverse)
library(lubridate)
source("Functions/waterlevel_calc.R")

#Read in the saved .rds files that contain the HOBO logger data and the AOS barometric pressure data with corresponding datetimes.
##name separately so that the measured reference height can be properly applied to the model

level_Mar3 <- read_rds("Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds")

level_Mar16 <- read_rds("Data/HOBO_Loggers/YS/Mar16_Jun17/level_data.rds") 

level_Jun17 <- read_rds("Data/HOBO_Loggers/YS/Jun17_Aug26/level_data.rds")

#The model written in waterlevel_calc.R will calculate the water level 
Mar3_Mar16 <- CalculateLevel(level_Mar3, 0.085)
Mar16_Jun17 <- CalculateLevel(level_Mar16, 0.10)
Jun17_Aug26 <- CalculateLevel(level_Jun17, 0.4572)

#Bind the datasets together for easy graphing
level.data <- rbind(Mar3_Mar16, Mar16_Jun17, Jun17_Aug26)

#CHECK
ggplot(level.data) +
  geom_line(aes(Date, Water_Depth)) 

ggplot(level.data) +
  geom_line(aes(Date, -(Bar.Pressure - Pressure)))




