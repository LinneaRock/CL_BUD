library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpubr)
library(zoo)
source("Functions/cond.R")
source("Functions/splot.R")

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

#checking max and min values for the Mendota data
checkepi <- loggerME_Epi %>%
  mutate(day = as.Date(date, "%m/%d/%y", tz = "GMT")) %>%
  group_by(day) %>%
  summarise(MAX = max(sp.cond),
            MIN = min(sp.cond))

checkhypo <- loggerME_Hypo %>%
  mutate(day = as.Date(date, "%m/%d/%y", tz = "GMT")) %>%
  group_by(day) %>%
  summarise(MAX = max(sp.cond),
            MIN = min(sp.cond))

ggplot() +
  geom_line(checkepi, mapping = aes(day, MIN)) +
  geom_line(checkhypo, mapping = aes(day, MIN))


#Trying a moving daily average to see if it helps with the diurnal fluctuations in the part of the data taht has a lot of fluctuation

RMepiME <- loggerME_Epi %>%
  mutate(rollingmean = rollmean(sp.cond, 47, fill = NA))

ggplot(RMepiME) +
  geom_line(aes(date, sp.cond, color = "original")) +
  geom_line(aes(date, rollingmean, color = "zoo"))

RMhypoME <- loggerME_Hypo %>%
  mutate(rollingmean = rollmean(sp.cond, 47, fill = NA))

ggplot(RMhypoME) +
  geom_line(aes(date, sp.cond, color = "original")) +
  geom_line(aes(date, rollingmean, color = "zoo"))


RMepiME <- RMepiME %>%
  select(date, rollingmean) %>%
  rename(sp.cond = rollingmean)

RMhypoME <- RMhypoME %>%
  select(date, rollingmean) %>%
  rename(sp.cond = rollingmean)

lakecond(RMepiME, RMhypoME, "Mendota 24m", "Mendota 1.5m")

#looking at lake sp. conductivity with inputs' sp. conductivity patterns


ggplot() +
  geom_line(loggerME_Hypo %>%
              filter(date >= "2020-03-01 00:00:00" & date <= "2020-03-29 00:00:00"),
           mapping = aes(date, sp.cond, color = "ME hypo")) +
  #geom_line(loggerME_Epi %>%
   #           filter(date >= "2020-03-01 00:00:00" & date <= "2020-03-29 00:00:00"),
    #        mapping = aes(date, sp.cond, color = "ME epi")) +
  geom_line(logger6MC %>%
              filter(date >= "2020-03-01 00:00:00" & date <= "2020-03-29 00:00:00"),
            mapping = aes(date, sp.cond, color = "6MC")) +
  geom_line(loggerDC %>%
              filter(date >= "2020-03-01 00:00:00" & date <= "2020-03-29 00:00:00"),
            mapping = aes(date, sp.cond, color = "DC")) +
  geom_line(loggerPBMS %>%
              filter(date >= "2020-03-01 00:00:00" & date <= "2020-03-29 00:00:00"),
            mapping = aes(date, sp.cond, color = "PBMS"))
