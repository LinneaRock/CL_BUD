library(tidyverse)
library(lubridate)


levellogger <- read.csv("Data/HOBO_Loggers/YS/Feb3_Mar16/WL20484276.csv")
AOS <- read.csv("Data/HOBO_Loggers/YS/Feb3_Mar16/pressure_AOS.csv") #%>%
  mutate(Pressure = Pressure * .1) #%>% #convert hPa to kPa

  


         