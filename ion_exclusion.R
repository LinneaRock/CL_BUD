library(tidyverse)
library(lubridate)

#load and format the datasets
ions <- read_csv("Data/LTER_ions.csv") %>% #all ion data from LTER
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(depth = replace(depth, depth == 0, 0.25)) %>% 
  mutate(depth = replace(depth, depth == 21.9, 22)) #rounding depths to nearest depth with hp_factor to ensure dall data is included
  
hypso <- read_csv("Data/lake_hypsometry.csv") #hypsometry factor to multiply by total lake volume

icedepth <- read_csv("Data/winter.csv") %>% #depth of ice
  rename(icedate = sampledate)  #icedate refers to the date the ice depth was measured
  
icedates <- read_csv("Data/icedates.csv") %>% #ice on and off dates
  filter(lakeid == "MO" | lakeid == "ME") %>%
  mutate(ice_on = mdy(ice_on),
         ice_off = mdy(ice_off),
         year4 = year4 + 1)

#joining all datasets together
combined_data <- ions %>%
  left_join(hypso, by = c("lakeid", "depth")) %>%
  left_join(icedates, by = c("lakeid", "year4")) %>%
  left_join(icedepth, by = c("lakeid", "year4"))


mass <- combined_data %>%
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
         mn_mass = mn * vol_liters * 0.000000001) %>% #converting concentration [mg L^-1] to mass in tonnes [Mg]
  rowwise() %>%
  mutate(winter = ifelse(between(sampledate, ice_on, ice_off), "Y", "N")) %>% #to easily distinguish ice on vs. ice off data
  mutate_if(is.numeric, ~replace_na(., 0)) %>% #converting NAs to 0 for the next step
  mutate(All_ions = sum(cl_mass + so4_mass + ca_mass + mg_mass + na_mass + k_mass + fe_mass + mn_mass)) %>%
  rename(iondate = sampledate,
         iondepth = depth)






ggplot(mass %>% filter(lakeid == "ME")) +
  geom_point(aes(iondate, cl_mass, color = iondepth, shape = winter)) +
  scale_color_viridis_c(direction = -1) 

ggplot(mass %>% filter(lakeid == "ME"))+
  geom_point(aes(iondate, cl, shape = winter, color = iondepth)) +
  scale_color_viridis_c(direction = -1) 
