library(tidyverse)
library(sf)
library(readxl)

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



#Read in Road salt application data from city
#function to convert tons and gallons to metric tonnes and liters
format_salt <- function(original) {
  df <- read_xlsx(original, sheet = "SHIFT TOTALS") %>%
    mutate(DATE = as.Date(as.character(DATE))) %>%
    filter(DATE < "2020-03-04") %>% #excel sheet had a lot of extra rows with no information
    mutate(Total_Salt_Mg = `TOTAL SALT TONS` * 0.907185,
           SALT_Mg = SALT_ton * 0.907185,
           SAND_Mg = SAND_ton * 0.907185,
           BRINE_l = BRINE_gal * 3.785411784) %>%
    dplyr::select(Total_Salt_Mg, DATE, SHIFT, TRUCK, ROUTE, SALT_load, SAND_load, BRINE_l, SALT_Mg, SAND_Mg)
  
}


E2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx")
W2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx")




E19_perdate <- E2019 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

sum(E19_perdate$total_ton)




W19_perdate <- W2019 %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

sum(W19_perdate$total_ton) + sum(E19_perdate$total_ton)

Winter19 <- left_join(E19_perdate, W19_perdate, by = "DATE")
Winter19[is.na(Winter19)] <- 0
Winter19 <- Winter19 %>%
  rename(total_salt_EAST = total_ton.x,
         total_salt_WEST = total_ton.y) %>%
  mutate(total_salt = (total_salt_EAST + total_salt_WEST))


