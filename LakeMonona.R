library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpubr)
library(zoo)
library(viridisLite)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/outlier_detect_remove.R")
source("Functions/cond_compare.R")
source("Functions/clseries.R")


#conductivity data from loggers -- remove points that were collected outside of the water
loggerMO_Epi20_1 <- loggerMO_Epi20 %>% 
  filter(date <= as.POSIXct("2020-05-12 12:45:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Hypo20_1 <- loggerMO_Hypo20 %>% 
  filter(date <= as.POSIXct("2020-05-12 12:45:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Epi21_1 <- loggerMO_Epi21 %>%
  filter(date <= as.POSIXct("2021-04-10 06:30:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Hypo21_1 <- loggerMO_Hypo21 %>% 
  filter(date <= as.POSIXct("2021-04-10 06:30:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water

#remove any outliers
MO_Epi_cond_data_20 <- outlier_detect_remove(loggerMO_Epi20_1, "MO_epi_20") %>% mutate(year = year(date))
MO_Hypo_cond_data_20 <- outlier_detect_remove(loggerMO_Hypo20_1, "MO_hypo_20") %>% mutate(year = year(date))
MO_Epi_cond_data_21 <- outlier_detect_remove(loggerMO_Epi21_1, "MO_epi_21") %>% mutate(year = year(date))
MO_Hypo_cond_data_21 <- outlier_detect_remove(loggerMO_Hypo21_1, "MO_hypo_21") %>% mutate(year = year(date))

#conductivity timeseries for 2019-2020
lakecond(MO_Epi_cond_data_20, MO_Hypo_cond_data_20, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona during the winter and spring 2019-2020")
splot("conductance_time_series/", "MO_19-20")

#conductivity timeseries for 2020-2021
lakecond(MO_Epi_cond_data_21, MO_Hypo_cond_data_21, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona during the winter and spring 2020-2021")
splot("conductance_time_series/", "MO_20-21")

MO_epi <- MO_Epi_cond_data_20 %>%
  rbind(MO_Epi_cond_data_21)

ggplot(MO_epi) +
  geom_line(aes(date, sp.cond), color = "snow3") +
  geom_line(aes(date, runningmean)) +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       x = "") + L_theme()



p1 <- cond(MO_Epi_cond_data_20) + ylim(500, 700)
  
p2 <- cond(MO_Epi_cond_data_21) + ylim(500, 700)

p1 | p2

