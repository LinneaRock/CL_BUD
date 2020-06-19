library(tidyverse)
library(lubridate)
source("Functions/waterlevel_calc.R")

level_Mar3 <- read_rds("Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds")

level_Mar16 <- read_rds("Data/HOBO_Loggers/YS/Mar16_Jun17/level_data.rds") %>%
  mutate(Date = Date - hours(1)) #daylight savings grr..

Mar3_Mar16 <- CalculateLevel(level_Mar3, 0.085)
Mar16_Jun17 <- CalculateLevel(level_Mar16, 0.10)

level.data <- rbind(Mar3_Mar16, Mar16_Jun17)

#CHECK
ggplot(level.data) +
  geom_line(aes(Date, Water_Depth)) 

ggplot(level.data) +
  geom_line(aes(Date, -(Bar.Pressure - Pressure)))




