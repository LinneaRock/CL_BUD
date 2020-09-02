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
  dplyr::select(mslink, funct_class.x, calc_length_ft, surface_width, SaltRt_Name, RouteNumber, lanes) %>%
  mutate(lanes = ifelse(mslink == 2780, 2, lanes),
         lanes = ifelse(mslink == 2707, 2, lanes),
         lanes = ifelse(mslink == 23284, 2, lanes),
         lanes = ifelse(mslink == 25731, 4, lanes)) %>% #manually adding lane numbers to rows that had missing values. Checked using google maps and road segments before and after in the database 
  mutate(routeloc = ifelse(str_detect(SaltRt_Name, "E "), "E", "W")) %>% #indicates East or West route
  mutate(routeno = parse_number(SaltRt_Name)) %>% #route number 
  mutate(calc_length_m = calc_length_ft / 3.280839895) %>% #convert length of road to [meters]
  mutate(surface_width_m = surface_width / 3.280839895) %>% #convert road widths to [meters] 
  mutate(surface_area_m2 = calc_length_m * surface_width_m) %>% #road area in [m^2]
  mutate(centerline_miles = calc_length_ft / 5280) %>% #calculate roadway in mileage
  mutate(lane_miles = centerline_miles * lanes) #calculate lane miles in each roadway


#Read in Road salt application data from city
#function format road salt application data
#Convert tons and gallons to metric tonnes and liters, dates
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

#2019-20 East Madison salt application per route per event
E2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx")
#2019-20 West Madison salt application per route per event
W2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx")



E_roads <- road_info %>%
  filter(routeloc == "E") %>%
 # mutate(lanes = ifelse(mslink == 2780, 2, lanes),
#         lanes = ifelse(mslink == 2707, 2, lanes),
#         lanes = ifelse(mslink == 23284, 2, lanes),
#         lanes = ifelse(mslink == 25731, 4, lanes)) %>% #manually adding lane numbers to rows that had missing values. Checked using google maps and road segments before and after in the database 
  group_by(routeno) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m),
    sum_area = sum(surface_area_m2)
  ) 




check <- SaltRoutes %>%
  filter(mslink == 	2780 |
           mslink == 	23284 |
           mslink == 	2707 |
           mslink == 	25731 |
           segment_name == "WINNEBAGO ST")


CHECK2 <- Pavement %>%
  filter(mslink == 	2780 |
           mslink == 	23284 |
           mslink == 	2707 |
           mslink == 	25731 |
           segment_name == "E JOHNSON ST")






























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


