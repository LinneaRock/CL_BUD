library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(broom)

source("Functions/join_datasets_chloride.R")
source("Functions/join_datasets_cond.R")
source("Functions/linreg.R")
source("functions/L_theme.R")

#function to calculate a timeseries at 30 min timesteps of chloride concentration, loading rate, mass load, and cumulative loading. 
#4 visualization functions follow
chloride_ts_mass <- function(chloride_data, logger_data, discharge_data) {
  
  cumSkipNA <- function(x, FUNC)
  {
    d <- deparse(substitute(FUNC))
    funs <- c("max", "min", "prod", "sum")
    stopifnot(is.vector(x), is.numeric(x), d %in% funs)
     FUNC <- match.fun(paste0("cum", d))
    x[!is.na(x)] <- FUNC(x[!is.na(x)])
    x
  }
  
  
  
  a <- join_datasets_chloride(chloride_data, logger_data) %>%
    select(date, datetime_collected, chloride_mgL)
  
  
  b <- join_datasets_cond(logger_data, discharge_data) %>%
    select(date.x, runningmeandis) %>%
    rename(date = date.x)
  
  combined <- logger_data %>%
    left_join(a, by = "date") %>%
    left_join(b, by = "date") %>%
    #separate(date, c("date", "time"), sep = " ") %>% #add a daily timestamp
    mutate(date = ymd_hms(date)) %>%
    mutate(year = year(date)) %>%
    mutate(year_mon = paste(year(date), month(date), sep = "-")) %>%
    mutate(mon = months.POSIXt(date)) %>% #add months
    mutate(season = NA) %>% # add seasons
    mutate(season = ifelse(mon == "October" |
                             mon == "November" |
                             mon == "December" |
                             mon == "January" |
                             mon == "February" |
                             mon == "March" |
                             mon == "April", "October - April", season),
           season = ifelse(is.na(season), "May - September", season)) %>%
    mutate(season_id = NA) %>%
    mutate(season_id = ifelse(
      year_mon == "2019-12" | year_mon == "2020-1" | year_mon == "2020-2" | year_mon == "2020-3",
      "2019-2020 Salting",
      season_id
    )) %>%
    mutate(season_id = ifelse(
      year_mon == "2020-4" | year_mon == "2020-5" | year_mon == "2020-6" | year_mon == "2020-7" | year_mon == "2020-8" | year_mon == "2020-9" | year_mon == "2020-10",
      "2020 Non-Salting",
      season_id)) %>%
    mutate(season_id = ifelse(
      year_mon == "2020-11" | year_mon == "2020-12" | year_mon == "2021-1" | year_mon == "2021-2" | year_mon == "2021-3" | year_mon == "2021-4",
      "2020-2021 Salting",
      season_id))
  
  #function to obtain coefficient information of linear regression for chloride and conductivity
  info <- function(chloride_data, logger_data) {
    
    qsc <- join_datasets_chloride(chloride_data, logger_data)
    
    info <- lm(chloride_mgL ~ runningmean, qsc)
    
    #print coefficient information
    return(summary(info))
    
  }
  
  slope <- coef(info(chloride_data, logger_data))[2,1] #get slope value
  intercept <- coef(info(chloride_data, logger_data))[1,1] #intercept value 
  minobs <- min(chloride_data$chloride_mgL)
  
  
  ts_load <- combined %>%
    mutate(timestep = date - lag(date)) %>% #timestep in minutes
    mutate(chloride_predict = (slope * runningmean) + intercept) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
    mutate(chloride_use_mgL = ifelse(is.na(chloride_mgL), chloride_predict, chloride_mgL)) %>% #use the actual data when we have it and estimated values in all other instances
    mutate(chloride_use_mgL = ifelse(chloride_use_mgL <= 0, minobs, chloride_use_mgL)) %>% #if concentration falls to or below zero, use the minimum observed value
    #mutate(chloride_use_mgL = ifelse(chloride_predict <= 0, minobs, chloride_predict)) %>% #if concentration falls to or below zero, use the minimum observed value
    mutate(cl_rate_gs = chloride_use_mgL * runningmeandis) %>% #load rate in [g s^-1] - 1000L/m^3 and 1000mg/g unit coversions cancel out
    mutate(cl_load_g = cl_rate_gs * (timestep)) %>% #grams chloride every timestep #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * s]
    mutate(cl_load_g = ifelse(timestep > 5000, NA, cl_load_g)) %>% #loggers were removed for a week and I don't want to calculate load during that missing data period
    mutate(cumulative_cl_g = cumSkipNA(cl_load_g, sum)) %>% #grams chloride cumulative loading
    filter(timestep > 0)
    
  return(ts_load)
  
}

concentration_ts <- function(ts_data, customTitle) {
  ggplot(ts_data, aes(date, chloride_use_mgL)) + 
    geom_line() + L_theme() + 
    labs(y = "Chloride Concentration"~(mg~L^-1), 
         x = "", 
         title = customTitle)
}

rate_ts <- function(ts_data, customTitle) {
  ggplot(ts_data, aes(date, cl_rate_gs)) + 
    geom_line() + L_theme() + 
    labs(y = "Chloride Loading Rate"~(g~s^-1), 
         x = "", 
         title = customTitle)
}

load_ts <- function(ts_data, customTitle) {
  ggplot(ts_data, aes(date, cl_load_g/1000)) + 
    geom_line() + L_theme() + 
    labs(y = "Chloride Mass"~(kg), 
         x = "", 
         title = customTitle)
}

cumulative_ts <- function(ts_data, customTitle) {
  ggplot(ts_data) +
    geom_line(aes(date, cumulative_cl_g/1000000)) +
    L_theme() +
    labs(
      y = "Cumulative Chloride Loading"~(Mg),
      x = "",
      title = customTitle)
}

#function to calculate daily mass load and average daily concentration
#2 visualization functions follow
chloride_daily_mass <- function(ts_data) {
  daily <- ts_data %>%
    separate(date, c("date", "time"), sep = " ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(daily_mass_kg = sum(cl_load_g)/1000,
              daily_concentration_mgL = mean(chloride_use_mgL)) #daily load of chloride [kg]
  
  return(daily)
}

daily_load <- function(daily_data, customTitle) {
  ggplot(daily_data, aes(date, daily_mass_kg)) + 
    geom_line() + L_theme() + 
    labs(y = "Daily Chloride Mass"~(kg), 
         x = "", 
         title = customTitle)
}

daily_ave_conc <- function(daily_data, customTitle) {
  ggplot(daily_data) +
    geom_line(aes(date, daily_concentration_mgL)) +
    L_theme() +
    labs(title = customTitle,
         y = "Average Daily Chloride Concentration"~(mg~L^-1),
         x = "")
}

#function to calculate monthly mass load
#visualization function follows
chloride_monthly_load <- function(ts_data) {
  monthly <- ts_data %>%
    group_by(year_mon) %>%
    summarise(monthly_mass_Mg = sum(na.omit(cl_load_g))/1000000) #monthly load of chloride [Mg]
  
  return(monthly)
}

monthly_load <- function(monthly_data, customTitle) {
  ggplot(monthly_data, aes(year_mon, monthly_mass_Mg)) + 
    geom_bar(stat = "identity", fill = '#1C366B') + L_theme() + 
    labs(y = "Monthly Chloride Mass "~(Mg), 
         x = "", 
         title = customTitle)
}

# function to calculate seasonal mass load
# visualization function follows
chloride_seasonal_load <- function(ts_data) {
  seasonal <- ts_data %>%
    group_by(season_id) %>%
    summarise(seasonal_mass_Mg = sum(na.omit(cl_load_g))/1000000) #seasonal load of chloride [Mg]
}

seasonal_load <- function(seasonal_data, customTitle) {
  ggplot(seasonal_data, aes(season_id, seasonal_mass_Mg)) +
    geom_bar(stat = 'identity', fill = '#1C366B') + L_theme() +
    labs(y = "Seasonal Chloride Mass"~(Mg),
         x = "",
         title = customTitle)
}

#function to calculate annual mass load
chloride_annual_load <- function(ts_data) {
  annually <- ts_data %>%
    group_by(year) %>%
    summarise(annual_mass_Mg = sum(cl_load_g)/1000000) #annual load of chloride [Mg]
  
  return(annually)
}







#if we ever decide to use segmented linear regression
#test_mass <- function(chloride_data, logger_data, discharge_data) {
#   
#   a <- join_datasets_chloride(chloride_data, logger_data) %>%
#     select(date, datetime_collected, chloride_mgL)
#   
#   
#   b <- join_datasets_cond(logger_data, discharge_data) %>%
#     select(i.date, runningmeandis) %>%
#     rename(date = i.date)
#   
#   combined <- logger_data %>%
#     left_join(a, by = "date") %>%
#     left_join(b, by = "date") %>%
#     #separate(date, c("date", "time"), sep = " ") %>% #add a daily timestamp
#     mutate(date = ymd_hms(date)) %>%
#     mutate(year = year(date)) %>%
#     mutate(year_mon = paste(year(date), month(date), sep = "-")) %>%
#     mutate(mon = months.POSIXt(date)) %>% #add months
#     mutate(season = NA) %>% # add seasons
#     mutate(season = ifelse(mon == "January" |
#                              mon == "February" |
#                              mon == "March", "Winter", season),
#            season = ifelse(mon == "April" |
#                              mon == "May" |
#                              mon == "June", "Spring", season),
#            season = ifelse(mon == "July" |
#                              mon == "August" |
#                              mon == "September", "Summer", season),
#            season = ifelse(mon == "October" |
#                              mon == "November" |
#                              mon == "December", "Fall", season))
#   
# #using functions from """"testing, obtain upper and lower limit slopes
#   
#   upper_slope <- slope_up(chloride_data, logger_data) 
#   upper_intercept <- intercept_up(chloride_data, logger_data)
#   lower_slope <- slope_low(chloride_data, logger_data) 
#   lower_intercept <- intercept_low(chloride_data, logger_data)
#   breakpoint_cond <- breakpoint(chloride_data, logger_data)
#   
#   test_load <- combined %>%
#     #mutate(chloride_predict = (slope * runningmean) + intercept) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
#     mutate(chloride_predict = ifelse(runningmean >= breakpoint_cond, ((upper_slope * runningmean) + upper_intercept), ((lower_slope * runningmean) + lower_intercept))) %>%
#     #mutate(chloride_predict = ifelse(runningmean >= breakpoint_cond, upper_slope, lower_slope)) %>%
#     mutate(chloride_use = ifelse(is.na(chloride_mgL), chloride_predict, chloride_mgL)) %>% #use the actual data when we have it and estimated values in all other instances
#     mutate(cl_rate = chloride_use * runningmeandis) %>% #load rate in [g s^-1] - 1000L and 1000mg unit coversions cancel out
#     mutate(cl_load = cl_rate * 1800) #grams chloride every 30 mins #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * 1800 s]
#   
#   return(test_load)
#   
# }


  
