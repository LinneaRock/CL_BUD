#Run at least weekly
library(RJSONIO)
library(tidyverse)
library(lubridate)

temp_coversion <- function(temperature) {
  (temperature - 32) * (5/9)
}

format_scraped <- function(dataname, parameter) {
  do.call(rbind.data.frame, dataname) %>%
    rename(date = 1, parameter = 2) %>%
    mutate(parameter = as.numeric(parameter)) %>%
    mutate(date = as.POSIXct(date/1000, origin = as.POSIXct('1970-01-01', tz = 'US/Central'))) %>%
    mutate(date = date + hours(6))  #converting to GMT from CST
  
}


#use only during daylight savings time.. eye roll
format_scraped_DST <- function(dataname, parameter) {
  do.call(rbind.data.frame, dataname) %>%
    rename(date = 1, parameter = 2) %>%
    mutate(parameter = as.numeric(parameter)) %>%
    mutate(date = as.POSIXct(date/1000, origin = as.POSIXct('1970-01-01', tz = 'US/Central'))) %>%
    mutate(date = date + hours(5))  #converting to GMT from CDT
  
}

checkplot <- function(df, parameter) {
  ggplot(df) + geom_path(aes(x = date, y = parameter))
}

#Read in latest data from the website
df = fromJSON('http://infosyahara.org/plot/7days.mononaoutlet.json')
str(df)

#Read in old data
velocity <- read.csv("Data/Monona_Outlet_Data/velocity_YS.csv") %>%
  select(date, velocity) %>%
  mutate(date = ymd_hms(date))

stage <- read.csv("Data/Monona_Outlet_Data/stage_YS.csv") %>%
  select(date, stage) %>%
  mutate(date = ymd_hms(date))

discharge <- read.csv("Data/Monona_Outlet_Data/d_YS.csv") %>%
  select(date, discharge) %>%
  mutate(date = ymd_hms(date))

temp <- read.csv("Data/Monona_Outlet_Data/temp_YS.csv") %>%
  select(date, temp) %>%
  mutate(date = ymd_hms(date))


#find last dates in old datasets
v.day <- max(temp$date)
s.day <- max(stage$date)
d.day <- max(discharge$date)
t.day <- max(temp$date)


#Format the new data based on last datetime of old data
YS_temp.df <- format_scraped_DST(df$temp, temp) %>%
  rename(temp = parameter) %>%
  mutate(temp = temp_coversion(temp)) %>%
  filter(date > t.day)
checkplot(YS_temp.df, YS_temp.df$temp)


YS_discharge.df <- format_scraped_DST(df$discharge, discharge) %>%
  rename(discharge = parameter) %>%
  mutate(discharge = discharge * 0.028316847) %>% #convert [ft^3 s^-1] to [m^3 s^-1]
  filter(date > d.day)
checkplot(YS_discharge.df, YS_discharge.df$discharge)


YS_velocity.df <- format_scraped_DST(df$velocity, velocity) %>%
  rename(velocity = parameter) %>%
  mutate(velocity = velocity / 3.281) %>% #convert [ft s^-1] to [m s^-1]
  filter(date > v.day)
checkplot(YS_velocity.df, YS_velocity.df$velocity)


YS_stage.df <- format_scraped_DST(df$stage, stage) %>%
  rename(stage = parameter) %>%
  mutate(stage = stage / 3.281) %>% #convert ft to m
  filter(date > s.day)
checkplot(YS_stage.df, YS_stage.df$stage)




#combine old and new data.. always check here before saving
YS_velocity <- rbind(velocity, YS_velocity.df) %>% 
  distinct() 
checkplot(YS_velocity, YS_velocity$velocity)

YS_stage <- rbind(stage, YS_stage.df) %>% 
  distinct() 
checkplot(YS_stage, YS_stage$stage)

YS_discharge<- rbind(discharge, YS_discharge.df) %>% 
  distinct() 
checkplot(YS_discharge, YS_discharge$discharge)

YS_temp <- rbind(temp, YS_temp.df) %>% 
  distinct() 
checkplot(YS_temp, YS_temp$temp)

#final failsafe while saving, if there is a mistake, try again and the old dataset still exists in the folder
write.csv(YS_temp, "Data/Monona_Outlet_Data/temp_YSCHECK.csv")

write.csv(YS_discharge, "Data/Monona_Outlet_Data/d_YSCHECK.csv")

write.csv(YS_velocity, "Data/Monona_Outlet_Data/velocity_YSCHECK.csv")

write.csv(YS_stage, "Data/Monona_Outlet_Data/stage_YSCHECK.csv")
