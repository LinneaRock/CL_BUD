
library(tidyverse)
library(sf)
library(tidygeocoder)
library(readxl)

pools <- read_xlsx("Data/Historical_External/pool.xlsx")

pool_locations <- pools %>%
  geocode(address, lat = latitude, long = longitude) %>%
  na.omit() %>%
  select(Site, latitude, longitude) 



location_na <- pool_locations %>%
  filter(is.na(latitude)) %>%
  select(Site, latitude, longitude) %>%
  distinct() %>%
  mutate(latitude = ifelse(Site == "5607 Summer Shine Dr", 43.121650, latitude),
         longitude = ifelse(Site == "5607 Summer Shine Dr",  -89.283655, longitude),
         latitude = ifelse(Site == "1080 N Pleasant View Rd", 43.080517, latitude),
         longitude = ifelse(Site == "1080 N Pleasant View Rd", -89.537356, longitude),
         latitude = ifelse(Site == "1602 W Beltline Hwy", 43.035780, latitude),
         longitude = ifelse(Site == "1602 W Beltline Hwy", -89.409167, longitude),
         latitude = ifelse(Site == "1624 Fordem Ave", 43.093629, latitude),
         longitude = ifelse(Site == "1624 Fordem Ave", -89.366627, longitude),
         latitude = ifelse(Site == "3177 E Washington Ave", 43.108462, latitude),
         longitude = ifelse(Site == "3177 E Washington Ave", -89.335858, longitude),
         latitude = ifelse(Site == "401 N Thompson Dr2", 43.103363, latitude),
         longitude = ifelse(Site == "401 N Thompson Dr2", -89.297451, longitude),
         latitude = ifelse(Site == "4602 Eastpark Blvd", 43.155020, latitude),
         longitude = ifelse(Site == "4602 Eastpark Blvd", -89.299994, longitude),
         latitude = ifelse(Site == "5020 Pendleton Dr", 43.154203, latitude),
         longitude = ifelse(Site == "5020 Pendleton Dr", -89.284118, longitude),
         latitude = ifelse(Site == "5711 Slate Dr", 43.153464, latitude),
         longitude = ifelse(Site == "5711 Slate Dr", -89.280674, longitude),
         latitude = ifelse(Site == "639 Pleasant View Rd", 43.073630, latitude),
         longitude = ifelse(Site == "639 Pleasant View Rd", -89.536106, longitude),
         latitude = ifelse(Site == "8310 Globe Dr", 43.071656, latitude),
         longitude = ifelse(Site == "8310 Globe Dr", -89.526051, longitude),
         latitude = ifelse(Site == "9328 Silverstone Ln", 43.030638, latitude),
         longitude = ifelse(Site == "9328 Silverstone Ln", -89.547916, longitude),
         latitude = ifelse(Site == "9603 Paragon St", 43.064658, latitude),
         longitude = ifelse(Site == "9603 Paragon St", -89.553901, longitude))
 
         