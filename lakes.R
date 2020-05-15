library(tidyverse)
library(lubridate)
library(ggpubr)
source("Functions/cond.R")
source("Functions/splot.R")

#plotting Mendota time series for 2019-2020

lakecond(loggerME_Epi, loggerME_Hypo, "Mendota 24m", "Mendota 1.5m")
splot("conductance_time_series/", "ME")


lakecond(loggerMO_Epi, loggerMO_Hypo, "Monona 20m", "Monona 1.5m")
splot("conductance_time_series/", "MO")

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
