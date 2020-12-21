library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpubr)
library(zoo)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/find_outlier.R")

#plotting Mendota time series for 2019-2020

lakecond(loggerME_Epi, loggerME_Hypo, "Mendota 24m", "Mendota 1.5m") +
  capt_scseries("Lake Mendota", "deep hole of Lake Mendota")
splot("conductance_time_series/", "ME")


lakecond(loggerMO_Epi, loggerMO_Hypo, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona")
splot("conductance_time_series/", "MO")

cond_compare(fieldcondME %>% filter(depth == 24.0), loggerME_Hypo)
cond_compare(fieldcondME %>% filter(depth == 1.5), loggerME_Epi)
cond_compare(fieldcondMO %>% filter(depth == 20), loggerMO_Hypo)
cond_compare(fieldcondMO %>% filter(depth == 1.5), loggerMO_Epi)

#Calculating residence time
ME_vol <- 506880000 #volume in [m^3]
MO_vol <- 111520000 #volume in [m^3]


format_discharge <- function(d) {
    d %>%
    filter(year(date) == 2019) %>%
    group_by(yday(date)) %>%
    summarise(meandaily = mean(discharge * 3600 *24)) %>% 
    ungroup() %>%
    summarise(annual = sum(meandaily)) 
  
}


ME_discharge_out <- d.YI %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge *3600 * 24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily)) 

res.time <- ME_vol / ME_discharge_out

PBMS_in <- d.PBMS %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SMC_in <- d.6MC %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

YN_in <- d.YN %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SH_in <- d.sc.SH %>%
  filter(year(date) == 2019) %>%
  na.omit %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

ME_discharge_in <- SH_in + PBMS_in + SMC_in + YN_in

ME_discharge_in/ME_discharge_out

(ME_vol + ME_discharge_in) - ME_discharge_out






