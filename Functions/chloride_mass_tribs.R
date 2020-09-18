library(tidyverse)
library(data.table)
library(ggpubr)
library(broom)

source("Functions/join_datasets_chloride.R")
source("Functions/join_datasets_cond.R")
source("Functions/linreg.R")
source("functions/L_theme.R")


chloride_mass_load_rate <- function(chloride_data, logger_data, discharge_data) {

a <- join_datasets_chloride(chloride_data, logger_data) %>%
  select(date, datetime_collected, chloride_mgL)


b <- join_datasets_cond(logger_data, discharge_data) %>%
  select(i.date, discharge) %>%
  rename(date = i.date) %>%
  mutate(discharge = discharge * 1000) #convert [m^3 s^-1 to L s^-1]

combined <- logger_data %>%
  left_join(a, by = "date") %>%
  left_join(b, by = "date")

#function to obtain coefficient information of linear regression for chloride and conductivity
info <- function(chloride_data, logger_data) {
  
  qsc <- join_datasets_chloride(chloride_data, logger_data)
  
  info <- lm(chloride_mgL ~ sp.cond, qsc)
  
  #print coefficient information
  return(summary(info))
  
}

slope <- coef(info(chloride_data, logger_data))[2,1] #get slope value
intercept <- coef(info(chloride_data, logger_data))[1,1] #intercept value 


cl_load <- combined %>%
  mutate(chloride_predict = (slope * sp.cond) + intercept) %>% #interpolate chloride [mg L^-1] for each specific conductivity measure
  mutate(cl_rate = chloride_predict * discharge * 0.001) #load rate in [g s^-1]

return(cl_load)

}

plot_load <- function(data, title) {
  
  ggplot(data %>% filter(date < "2020-05-01 00:00:00")) +
    geom_line(aes(date, cl_rate)) +
    L_theme() +
    labs(title = title,
         y = "Chloride Loading"~(g~s^-1),
         x = "")
}

plot_load_daily <- function(data, title) {
  
  ggplot(data %>% filter(date < "2020-05-01 00:00:00")) +
    geom_line(aes(date, cl_mass)) +
    L_theme() +
    labs(title = title,
         y = "Chloride Loading"~(Mg),
         x = "")
}         