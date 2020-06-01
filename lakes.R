library(tidyverse)
library(lubridate)
library(ggpubr)
library(zoo)
source("Functions/cond.R")
source("Functions/splot.R")

#plotting Mendota time series for 2019-2020

lakecond(loggerME_Epi, loggerME_Hypo, "Mendota 24m", "Mendota 1.5m")
splot("conductance_time_series/", "ME")


lakecond(loggerMO_Epi, loggerMO_Hypo, "Monona 20m", "Monona 1.5m")
splot("conductance_time_series/", "MO")


#checking max and min values for the Mendota data
checkepi <- loggerME_Epi %>%
  mutate(day = as.Date(date, "%m/%d/%y", tz = "America/Chicago")) %>%
  group_by(day) %>%
  summarise(MAX = max(sp.cond),
            MIN = min(sp.cond))

checkhypo <- loggerME_Hypo %>%
  mutate(day = as.Date(date, "%m/%d/%y", tz = "America/Chicago")) %>%
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
