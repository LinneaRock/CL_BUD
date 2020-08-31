library(tidyverse)
library(sf)

#read in applicable layers from geodatabase 
Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement")
SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")

#Combine SaltRoutes with Pavement to get number of lanes in each salting segment
#They need to be converted to dataframes in order to use left_join
#keep applicable columns so I don't have to deal with 97 variables...
road_info <- SaltRoutes %>% as.data.frame() %>%
  left_join(Pavement %>% as.data.frame(), by = "mslink") %>%
  select(mslink, funct_class.x, calc_length_ft, surface_width, SaltRt_Name, RouteNumber, lanes) %>%
  mutate(routeloc = ifelse(str_detect(SaltRt_Name, "E "), "E", "W")) %>%
  mutate(routeno = parse_number(SaltRt_Name))



