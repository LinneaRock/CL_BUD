#Run at least weekly
library(RJSONIO)
library(tidyverse)
library(lubridate)

setwd("C:/Users/linne/Box Sync/CL_BUD")

temp_coversion <- function(temperature) {
  (temperature - 32) * (5/9)
}

format_scraped <- function(dataname, parameter) {
  do.call(rbind.data.frame, dataname) %>%
    rename(date = 1, parameter = 2) %>%
    mutate(parameter = as.numeric(parameter)) %>%
    mutate(date = as.POSIXct(date/1000, origin = as.POSIXct('1970-01-01', tz = "Etc/GMT-7"))) 
}



checkplot <- function(df, parameter) {
  ggplot(df) + geom_path(aes(x = date, y = parameter))
}

#Read in latest data from the website
df = fromJSON('http://infosyahara.org/plot/7days.wingra.json')
str(df)

#Read in old data
stage <- read.csv("Data/WingraCreek_Data/stage_WIC.csv") %>%
  select(date, stage) %>%
  mutate(date = ymd_hms(date))%>% 
  mutate(date = date - hours(6))
stage$date = force_tz(stage$date, tzone = "Etc/GMT-7")

discharge <- read.csv("Data/WingraCreek_Data/discharge_WIC.csv") %>%
  select(date, discharge) %>%
  mutate(date = ymd_hms(date))%>% 
  mutate(date = date - hours(6))
discharge$date = force_tz(discharge$date, tzone = "Etc/GMT-7")

temp <- read.csv("Data/WingraCreek_Data/temp_WIC.csv") %>%
  select(date, temp) %>%
  mutate(date = ymd_hms(date))%>% 
  mutate(date = date - hours(6))
temp$date = force_tz(temp$date, tzone = "Etc/GMT-7")


#find last dates in old datasets
s.day <- max(stage$date)
d.day <- max(discharge$date)
t.day <- max(temp$date)


#Format the new data based on last datetime of old data
WIC_temp.df <- format_scraped(df$temp, temp) %>%
  rename(temp = parameter) %>%
  mutate(temp = temp_coversion(temp)) %>%
  filter(date > t.day)
#WIC_temp.df$date = force_tz(WIC_temp.df, tzone = "Etc/GMT-7") 
checkplot(WIC_temp.df, WIC_temp.df$temp)


WIC_discharge.df <- format_scraped(df$discharge, discharge) %>%
  rename(discharge = parameter) %>%
  mutate(discharge = discharge * 0.028316847) %>% #convert [ft^3 s^-1] to [m^3 s^-1]
  filter(date > d.day)
checkplot(WIC_discharge.df, WIC_discharge.df$discharge)


WIC_stage.df <- format_scraped(df$stage, stage) %>%
  rename(stage = parameter) %>%
  mutate(stage = stage / 3.281) %>% #convert ft to m
  filter(date > s.day)
checkplot(WIC_stage.df, WIC_stage.df$stage)




#combine old and new data.. alwaWIC check here before saving
WIC_stage <- rbind(stage, WIC_stage.df) %>% 
  distinct() 
checkplot(WIC_stage, WIC_stage$stage)

WIC_discharge<- rbind(discharge, WIC_discharge.df) %>% 
  distinct() 
checkplot(WIC_discharge, WIC_discharge$discharge)

WIC_temp <- rbind(temp, WIC_temp.df) %>% 
  distinct() 
checkplot(WIC_temp, WIC_temp$temp)

#final failsafe while saving, if there is a mistake, try again and the old dataset still exists in the folder
write.csv(WIC_temp, "Data/WingraCreek_Data/temp_WICCHECK.csv")

write.csv(WIC_discharge, "Data/WingraCreek_Data/discharge_WICCHECK.csv")

write.csv(WIC_stage, "Data/WingraCreek_Data/stage_WICCHECK.csv")




#annoying code to deal with daylight savings change
# WIC_temp_CDTGMT <- WIC_temp.df %>%
#   filter(row_number() <= 1584) %>%
#   mutate(date = date + hours(5)) 
# 
# WIC_temp_CSTGMT <- WIC_temp.df %>%
#   filter(row_number() >= 1585) %>%
#   mutate(date = date + hours(6))
# 
# WIC_temp <- rbind(temp, WIC_temp_CDTGMT, WIC_temp_CSTGMT) %>% 
#   distinct() 
# 
# WIC_discharge_CDTGMT <- WIC_discharge.df %>%
#   filter(row_number() <= 1584) %>%
#   mutate(date = date + hours(5)) 
# 
# WIC_discharge_CSTGMT <- WIC_discharge.df %>%
#   filter(row_number() >= 1585) %>%
#   mutate(date = date + hours(6))
# 
# WIC_discharge <- rbind(discharge, WIC_discharge_CDTGMT, WIC_discharge_CSTGMT) %>% 
#   distinct() 
# 
# WIC_stage_CDTGMT <- WIC_stage.df %>%
#   filter(row_number() <= 1584) %>%
#   mutate(date = date + hours(5)) 
# 
# WIC_stage_CSTGMT <- WIC_stage.df %>%
#   filter(row_number() >= 1585) %>%
#   mutate(date = date + hours(6))
# 
# WIC_stage <- rbind(stage, WIC_stage_CDTGMT, WIC_stage_CSTGMT) %>% 
#   distinct() 
