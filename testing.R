library(tidyverse)
library(data.table)
library(ggpubr)
library(broom)

source("Functions/join_datasets_chloride.R")
source("Functions/join_datasets_cond.R")
source("Functions/linreg.R")
source("functions/L_theme.R")

chloride_ts_mass <- function(chloride_data, logger_data, discharge_data) {
  
  a <- join_datasets_chloride(chloride_data, logger_data) %>%
    select(date, datetime_collected, chloride_mgL)
  
  
  b <- join_datasets_cond(logger_data, discharge_data) %>%
    select(i.date, runningmeandis) %>%
    rename(date = i.date)
  
  combined <- logger_data %>%
    left_join(a, by = "date") %>%
    left_join(b, by = "date") %>%
    #separate(date, c("date", "time"), sep = " ") %>% #add a daily timestamp
    mutate(date = ymd_hms(date)) %>%
    mutate(year = year(date)) %>%
    mutate(year_mon = paste(year(date), month(date), sep = "-")) %>%
    mutate(mon = months.POSIXt(date)) %>% #add months
    mutate(season = NA) %>% # add seasons
    mutate(season = ifelse(mon == "January" |
                             mon == "February" |
                             mon == "March", "Winter", season),
           season = ifelse(mon == "April" |
                             mon == "May" |
                             mon == "June", "Spring", season),
           season = ifelse(mon == "July" |
                             mon == "August" |
                             mon == "September", "Summer", season),
           season = ifelse(mon == "October" |
                             mon == "November" |
                             mon == "December", "Fall", season))
  
  #function to obtain coefficient information of linear regression for chloride and conductivity
  info <- function(chloride_data, logger_data) {
    
    qsc <- join_datasets_chloride(chloride_data, logger_data)
    
    info <- lm(chloride_mgL ~ runningmean, qsc)
    
    #print coefficient information
    return(summary(info))
    
  }
  
  slope <- coef(info(chloride_data, logger_data))[2,1] #get slope value
  intercept <- coef(info(chloride_data, logger_data))[1,1] #intercept value 
  
  
  
  ts_load <- combined %>%
    mutate(chloride_predict = (slope * runningmean) + intercept) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
    mutate(chloride_use = ifelse(is.na(chloride_mgL), chloride_predict, chloride_mgL)) %>% #use the actual data when we have it and estimated values in all other instances
    mutate(cl_rate = chloride_use * runningmeandis) %>% #load rate in [g s^-1] - 1000L and 1000mg unit coversions cancel out
    mutate(cl_load = cl_rate * 1800) #grams chloride every 30 mins #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * 1800 s]
    
  return(ts_load)
  
}



daily_load <- function(mass_data) {
mass_data <- mass_data %>%
group_by(date) %>%
  summarise(daily_mass = sum(cl_load)/1000) #daily load of chloride [kg]
}

monthly_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year_mon) %>%
    summarise(monthly_mass = sum(cl_load)/1000000) #monthly load of chloride [Mg]
    
}

seasonal_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year, season) %>%
    summarise(seasonal_mass = sum(cl_load)/1000000) #seasonal load of chloride [Mg]
  
}

annual_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year) %>%
    summarise(annual_mass = sum(cl_load)/1000000) #annual load of chloride [Mg]
}


#problem with daily load

load <- chloride_ts_mass(labPBMS, PBMS_cond_data, PBMS_discharge)
PBMS_example1 <- ggplot(load, aes(date, chloride_use)) + geom_line() + L_theme() + labs(y = "Chloride Concentration"~(mg~L^-1), x = "", title = "PBMS Concentration Timeseries Simple Regression")



#daily_lr <- daily_load(load)
PBMS_example2 <- ggplot(load, aes(date, cl_load/1000)) + geom_line() + L_theme() + labs(y = "Chloride Daily Mass"~(kg), x = "", title = "PBMS Mass Loading Timeseries Simple Regression")
dcmonthly_lr <- monthly_load(dcload)
dcseason_lr <- seasonal_load(dcload)
dcannual_lr <- annual_load(dcload)


 




test_mass <- function(chloride_data, logger_data, discharge_data) {
  
  a <- join_datasets_chloride(chloride_data, logger_data) %>%
    select(date, datetime_collected, chloride_mgL)
  
  
  b <- join_datasets_cond(logger_data, discharge_data) %>%
    select(i.date, runningmeandis) %>%
    rename(date = i.date)
  
  combined <- logger_data %>%
    left_join(a, by = "date") %>%
    left_join(b, by = "date") %>%
    #separate(date, c("date", "time"), sep = " ") %>% #add a daily timestamp
    mutate(date = ymd_hms(date)) %>%
    mutate(year = year(date)) %>%
    mutate(year_mon = paste(year(date), month(date), sep = "-")) %>%
    mutate(mon = months.POSIXt(date)) %>% #add months
    mutate(season = NA) %>% # add seasons
    mutate(season = ifelse(mon == "January" |
                             mon == "February" |
                             mon == "March", "Winter", season),
           season = ifelse(mon == "April" |
                             mon == "May" |
                             mon == "June", "Spring", season),
           season = ifelse(mon == "July" |
                             mon == "August" |
                             mon == "September", "Summer", season),
           season = ifelse(mon == "October" |
                             mon == "November" |
                             mon == "December", "Fall", season))
  
#using functions from """"testing, obtain upper and lower limit slopes
  
  upper_slope <- slope_up(chloride_data, logger_data) 
  upper_intercept <- intercept_up(chloride_data, logger_data)
  lower_slope <- slope_low(chloride_data, logger_data) 
  lower_intercept <- intercept_low(chloride_data, logger_data)
  breakpoint_cond <- breakpoint(chloride_data, logger_data)
  
  test_load <- combined %>%
    #mutate(chloride_predict = (slope * runningmean) + intercept) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
    mutate(chloride_predict = ifelse(runningmean >= breakpoint_cond, ((upper_slope * runningmean) + upper_intercept), ((lower_slope * runningmean) + lower_intercept))) %>%
    #mutate(chloride_predict = ifelse(runningmean >= breakpoint_cond, upper_slope, lower_slope)) %>%
    mutate(chloride_use = ifelse(is.na(chloride_mgL), chloride_predict, chloride_mgL)) %>% #use the actual data when we have it and estimated values in all other instances
    mutate(cl_rate = chloride_use * runningmeandis) %>% #load rate in [g s^-1] - 1000L and 1000mg unit coversions cancel out
    mutate(cl_load = cl_rate * 1800) #grams chloride every 30 mins #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * 1800 s]
  
  return(test_load)
  
}



daily_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(date) %>%
    summarise(daily_mass = sum(cl_load)/1000) #daily load of chloride [kg]
}

monthly_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year_mon) %>%
    summarise(monthly_mass = sum(cl_load)/1000000) #monthly load of chloride [Mg]
  
}

seasonal_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year, season) %>%
    summarise(seasonal_mass = sum(cl_load)/1000000) #seasonal load of chloride [Mg]
  
}

annual_load <- function(mass_data) {
  mass_data <- mass_data %>%
    group_by(year) %>%
    summarise(annual_mass = sum(cl_load)/1000000) #annual load of chloride [Mg]
}


dctest <- test_mass(labPBMS, PBMS_cond_data, PBMS_discharge)
PBMS_example3 <- ggplot(dctest, aes(date, chloride_use)) + geom_line() + L_theme() + labs(y = "Chloride Concentration"~(mg~L^-1), x = "", title = "PBMS Concentration Timeseries Segmented Regression")
dcdaily <- daily_load(dctest)
PBMS_example4 <- ggplot(dcdaily, aes(date, daily_mass)) + geom_line() + L_theme() + labs(y = "Chloride Daily Mass"~(kg), x = "", title = "PBMS Mass Loading Timeseries Segmented Regression")
dcmonthly <- monthly_load(dctest)
dcseason <- seasonal_load(dctest)
dcannual <- annual_load(dctest)  
  
  
dcload <- chloride_ts_mass(labDC, DC_cond_data, DC_discharge)
DC_example1 <- ggplot(dcload, aes(date, chloride_use)) + geom_line() + L_theme() + labs(y = "Chloride Concentration"~(mg~L^-1), x = "", title = "DC Concentration Timeseries Simple Regression")
dcdaily_lr <- daily_load(dcload)
DC_example2 <- ggplot(dcdaily_lr, aes(date, daily_mass)) + geom_line() + L_theme() + labs(y = "Chloride Daily Mass"~(kg), x = "", title = "DC Mass Loading Timeseries Simple Regression")

dctest <- test_mass(labDC, DC_cond_data, DC_discharge)
DC_example3 <- ggplot(dctest, aes(date, chloride_use)) + geom_line() + L_theme() + labs(y = "Chloride Concentration"~(mg~L^-1), x = "", title = "DC Concentration Timeseries Segmented Regression")
dcdaily <- daily_load(dctest)
DC_example4 <- ggplot(dcdaily, aes(date, daily_mass)) + geom_line() + L_theme() + labs(y = "Chloride Daily Mass"~(kg), x = "", title = "DC Mass Loading Timeseries Segmented Regression")
  