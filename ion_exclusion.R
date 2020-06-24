library(tidyverse)
library(lubridate)

#load and format data 
ions <- read_csv("Data/LTER_ions.csv") %>%
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(depth = ifelse(depth == 0, 0.25, depth))

hypso <- read_csv("Data/lake_hypsometry.csv")

icedepth <- read_csv("Data/winter.csv")

icedates <- read_csv("Data/icedates.csv") %>%
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(ice_on = mdy(ice_on),
         ice_off = mdy(ice_off),
         year4 = year4 + 1)


#joining all the data together
winterions <- ions %>%
  left_join(hypso, by = c("lakeid", "depth")) %>%
  left_join(icedates, by = c("lakeid", "year4")) %>%
  left_join(icedepth, by = c("lakeid", "year4"))

#formatting data
winterions <- winterions %>%
  select(lakeid, season, year4, sampledate.x, depth, cl, so4, ca, mg, na, k, fe, mn, hp_factor, ice_on, ice_off, ice_duration, totice) %>%
  drop_na(cl) %>% 
  filter(cl > 0) %>%
  mutate(vol_cubicmeters = ifelse(lakeid == "ME", hp_factor * 506880000, hp_factor * 111520000)) %>% #hypsometrically weighting lake volumes [m^-3]
  mutate(vol_liters = vol_cubicmeters * 1000) %>% #converting cubic meters to liters
  mutate(cl_mass = cl * vol_liters * 0.000000001,
         so4_mass = so4 * vol_liters * 0.000000001,
         ca_mass = ca * vol_liters * 0.000000001,
         mg_mass = mg * vol_liters * 0.000000001,
         na_mass = na * vol_liters * 0.000000001,
         k_mass = k * vol_liters * 0.000000001,
         fe_mass = fe * vol_liters * 0.000000001,
         mn_mass = mn * vol_liters * 0.000000001) %>% #converting concentration [mg L^-1] to mass [Mg]
  rowwise() %>%
  mutate(winter = ifelse(between(sampledate.x, ice_on, ice_off), 1, 0)) #to easily distinguish ice on vs. ice off data



