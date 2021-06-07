library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(cowplot)
source("Functions/MET.R")

data <- read.csv("Data/Historical_External/Mad_Met_His.csv") %>%
  mutate(date = as.Date(as.yearmon(DATE))) %>%
  mutate(Y = factor(year(date)),
         M = month(date))



#Monthly Mean Temp
linepl(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))
bar(data$date, data$TAVG, bquote("Average Monthly Temp "*~degree*C))



precip_temp_data <- read.csv("Data/Historical_External/precip_temp.csv") %>%
  mutate(PRCP = PRCP * 25.4) %>%  #inches to mm
  mutate(date = as.POSIXct(as.character(DATE)))
  

# precip data  1939 - 2021
precip_data <- read.csv("Data/met_data.csv") %>%  #data in mm already
  mutate(date = as.POSIXct(as.character(DATE)))


winter2021prcp = sum((precip_data %>% filter(date >= "2020-11-01" & date < "2021-04-01"))$PRCP) #165.8
winter1920prcp = sum((precip_data %>% filter(date >= "2019-11-01" & date < "2020-04-01"))$PRCP) #261.8
notwinter2020prcp = sum((precip_data %>% filter(date >= "2020-04-01" & date < "2020-11-01"))$PRCP) #757.1


ggplot(precip_data, aes(date, PRCP)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(as.POSIXct("2019-11-01"), as.POSIXct("2021-04-01"))) +
  labs(x = "", y = "Daily Precipitation (mm)") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2019-11-01")), linetype = "dotted", color = "#1DACE8") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-04-01")), linetype = "dotted", color = "#1DACE8") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-11-01")), linetype = "dotted", color = "#1DACE8") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-04-01")), linetype = "dotted", color = "#1DACE8") +
  labs(caption = "Figure 20. Precipitation from 2019-11-01 through 2021-04-01. Blue dotted lines indicate break 
points between salting and non-salting seasons.")

ggsave("Plots/precip.png", height = 4.25, width = 6.25, units = "in")



precip <- bar(precip_temp_data, precip_temp_data$date, precip_temp_data$PRCP, bquote("Daily Precipitation (mm)"))






#annual precip 
annual_precip <- precip_data %>%
  mutate(year = year(date)) %>%
  filter(year < 2021 & year > 1939) %>%
  group_by(year) %>%
  summarise(annualprcp_cm = sum(PRCP)/10) %>%
  mutate(average = mean(annualprcp_cm))
 

ggplot(annual_precip) +
  geom_bar(aes(year, annualprcp_cm), stat = "identity", color = "black", fill = "#1C366B") +
  geom_line(aes(year, average), stat = "identity", color = "#F24D29") +
  L_theme() +
  labs(y = "Precipitation (cm)",
       x = "", 
       caption = "Figure 9. Annual precipitation (cm) recorded at the Dane County Airport from 1940-2020. 
Orange line is the average annual precipitation (cm) from 1940-2020.")

ggsave("Plots/Historical_Data_Viz/precip.png", height = 4.25, width = 6.25, units = "in")

