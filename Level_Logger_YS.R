library(tidyverse)
library(lubridate)


levellogger <- read.csv("Data/HOBO_Loggers/YS/Feb3_Mar16/WL20484276.csv") %>%
  mutate(char = as.character(Date),
         Date = as_datetime(char)) %>%
  select(Date, Pressure, Temp)

AOS <- read.csv("C:/Users/Linne/Downloads/pressure_AOS.csv") %>% #This file is really large so I am not saving it to GitHub (it gave me a warning)
  mutate(Bar.Pressure = Pressure * .1) %>% #convert hPa to kPa
  mutate(char = as.character(Date)) %>%
  mutate(char = gsub("T", " ", char),
         char = gsub("Z", "", char)) %>%
  mutate(date = as_datetime(char)) %>%
  select(date, Bar.Pressure) %>%
  rename(Date = date)
  
level.data <- left_join(levellogger, AOS, by = "Date")
write_rds(level.data, "Data/HOBO_Loggers/YS/Feb3_Mar16/level_data.rds") #When you want to run this script, load this file

#Water Density [kg m^-3]; x = temperature in °C
watden <- function(x){
  pt1 <- x + 288.9414
  pt2 <- 508929 * (x + 68.129630)
  pt3 <- (x - 3.9863) ^2
  1000 * (1 - (pt1/pt2) * pt3)
}