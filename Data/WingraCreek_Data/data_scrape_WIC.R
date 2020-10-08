#Run at least weekly
library(RJSONIO)

temp_coversion <- function(temperature) {
  (temperature - 32) * (5/9)
}

format_scraped_DST <- function(dataname, parameter) {
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
df = fromJSON('http://infosyahara.org/plot/7days.wingra.json')
str(df)

#Read in old data
stage <- read.csv("Data/WingraCreek_Data/stage_WIC.csv") %>%
  select(date, stage) %>%
  mutate(date = ymd_hms(date))

discharge <- read.csv("Data/WingraCreek_Data/d_WIC.csv") %>%
  select(date, discharge) %>%
  mutate(date = ymd_hms(date))

temp <- read.csv("Data/WingraCreek_Data/temp_WIC.csv") %>%
  select(date, temp) %>%
  mutate(date = ymd_hms(date))


#find last dates in old datasets
s.day <- max(stage$date)
d.day <- max(discharge$date)
t.day <- max(temp$date)


#Format the new data based on last datetime of old data
WIC_temp.df <- format_scraped_DST(df$temp, temp) %>%
  rename(temp = parameter) %>%
  mutate(temp = temp_coversion(temp)) %>%
  filter(date > t.day)
checkplot(WIC_temp.df, WIC_temp.df$temp)


WIC_discharge.df <- format_scraped_DST(df$discharge, discharge) %>%
  rename(discharge = parameter) %>%
  mutate(discharge = discharge * 0.028316847) %>% #convert [ft^3 s^-1] to [m^3 s^-1]
  filter(date > d.day)
checkplot(WIC_discharge.df, WIC_discharge.df$discharge)


WIC_stage.df <- format_scraped_DST(df$stage, stage) %>%
  rename(stage = parameter) %>%
  mutate(stage = stage / 3.281) %>% #convert ft to m
  filter(date > s.day)
checkplot(WIC_stage.df, WIC_stage.df$stage)




#combine old and new data.. alwaWIC check here before saving
WIC_stage <- rbind(stage, WIC_stage.df) %>% 
  distinct() 

WIC_discharge<- rbind(discharge, WIC_discharge.df) %>% 
  distinct() 

WIC_temp <- rbind(temp, WIC_temp.df) %>% 
  distinct() 


#final failsafe while saving, if there is a mistake, try again and the old dataset still exists in the folder
write.csv(WIC_temp, "Data/WingraCreek_Data/temp_WICCHECK.csv")

write.csv(WIC_discharge, "Data/WingraCreek_Data/d_WICCHECK.csv")

write.csv(WIC_stage, "Data/WingraCreek_Data/stage_WICCHECK.csv")


