library(tidyverse)
library(sf)
library(readxl)

#read in applicable layers from geodatabase 
Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement")
SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")

#Combine SaltRoutes with Pavement to get number of lanes in each salting segment
#They need to be converted to dataframes in order to use left_join
#keep applicable columns so I don't have to deal with 97 variables...
#Additionally a lot of manual work needed to ensure this dataset matches the PDF sent by City Streets
road_info <- SaltRoutes %>% as.data.frame() %>%
  left_join(Pavement %>% as.data.frame(), by = "mslink") %>%
  dplyr::select(mslink, segment_name.x, funct_class.x, calc_length_ft, surface_width, SaltRt_Name, RouteNumber, lanes) %>%
  mutate(lanes = ifelse(mslink == 2780, 2, lanes),
         lanes = ifelse(mslink == 2707, 2, lanes),
         lanes = ifelse(mslink == 23284, 2, lanes),
         lanes = ifelse(mslink == 25731, 4, lanes),
         lanes = ifelse(mslink == 5777, 2, lanes),
         lanes = ifelse(mslink == 26011, 2, lanes),
         lanes = ifelse(mslink == 14012, 2, lanes),
         lanes = ifelse(mslink == 3963, 4, lanes)) %>% #manually adding lane numbers to rows that had missing values. Checked using google maps and road segments before and after in the database 
  mutate(RouteNumber = ifelse(segment_name.x == "S BEDFORD ST" |
                                mslink == 4718 |
                                mslink == 4719 |
                                mslink == 4708 |
                                mslink == 4993 |
                                mslink == 4969 |
                                mslink == 4381 |
                                mslink == 4728 |
                                segment_name.x == "E WILSON ST"
                                , 1, RouteNumber)) %>% #manually adding segments to E route 1 (some overlap with W route 1)
  mutate(RouteNumber = ifelse(segment_name.x == "MILLPOND RD" |
                                segment_name.x == "LONG DR" |
                                segment_name.x == "SAVANNAH RD" |
                                segment_name.x == "EVAN ACRES RD" |
                                mslink == 4167 |
                                mslink == 4168 |
                                mslink == 4169 |
                                mslink == 4172 |
                                mslink == 4232 |
                                mslink == 4036
                              , 15, RouteNumber)) %>% #manually adding segments to E route 15
  mutate(RouteNumber = ifelse(segment_name.x == "DUNWOODY DR" |
                                segment_name.x == "BADGER LN" |
                                segment_name.x == "FELL RD" |
                                segment_name.x == "GOLDEN GATE WAY" |
                                segment_name.x == "MOORLAND RD" |
                                segment_name.x == "LAKE FARM RD" |
                                segment_name.x == "NOB HILL RD" |
                                segment_name.x == "E BADGER RD" |
                                segment_name.x == "WAUNONA WAY" |
                                segment_name.x == "HARRIMAN LN" |
                                segment_name.x == "ETHELWYN RD" |
                                segment_name.x == "GREENLEAF DR" |
                                segment_name.x == "ESTHER BEACH RD" |
                                segment_name.x == "FRAZIER AVE" |
                                segment_name.x == "FAYETTE AVE" |
                                segment_name.x == "HOBOKEN RD" |
                                segment_name.x == "LAKE POINT DR" |
                                segment_name.x == "BRIDGE RD" |
                                mslink == 14051 |
                                mslink == 4166 |
                                segment_name.x == "DUTCH MILL RD" |
                                segment_name.x == "E BROADWAY" |
                                segment_name.x == "E BROADWAY (EB)" |
                                segment_name.x == "COLLINS CT" |
                                mslink == 17102 |
                                mslink == 21272 |
                                mslink == 21273 |
                                mslink == 21274 |
                                segment_name.x == "MARSH RD" |
                                segment_name.x == "VOGES RD" |
                                segment_name.x == "OWL CREEK DR" |
                                segment_name.x == "GREAT GRAY DR" |
                                segment_name.x == "VALOR WAY" |
                                segment_name.x == "BRANDENBURG WAY" |
                                segment_name.x == "FREESE LN",
                              16, RouteNumber)) %>% #manually adding segments for E route 16
  mutate(SaltRt_Name = ifelse(mslink == 4427 |
                                mslink == 4446 |
                                mslink == 4715 |
                                mslink == 6178 | #route 17?
                                mslink == 6177  #route 17?
                              , "W Salt Route 1", SaltRt_Name)) %>% #manually adding segments for W route 1
  mutate(SaltRt_Name = ifelse(mslink == 6257, "W Salt Route 2", SaltRt_Name)) %>% #manually adding segments for W route 2 from route 18??
  mutate(SaltRt_Name = ifelse(mslink == 1185 | #route 20?
                                mslink == 1184 | #route 20?
                                mslink == 25750   #route 21?
                              , "W Salt Route 5", SaltRt_Name)) %>% #manually adding segments for w route 5
  mutate(SaltRt_Name = ifelse(mslink == 23139 |
                                mslink == 5915 | #route 22
                                mslink == 23141 | #route 22
                                mslink == 23139 | #route 22
                                mslink == 5915 #route 22
                              , "W Salt Route 7", SaltRt_Name)) %>% #manually adding segments for w route 7
  mutate(SaltRt_Name = ifelse(mslink == 	5993	, "W Salt Route 8", SaltRt_Name)) %>% #manually adding segmetns for w route 8 from route 23??
  mutate(SaltRt_Name = ifelse(mslink == 6943 |
                                mslink ==	6941 |
                                mslink == 6942 |
                                mslink == 6823 | #route 24?
                                mslink == 6609 | #route 24?
                                mslink == 6705 | #route 24?
                                mslink == 10726 | #route 24?
                                mslink == 24418 | #route 24?
                                mslink == 24417 #route 24?
                              , "W Salt Route 10", SaltRt_Name)) %>% #manually adding segements for w route 10
  mutate(SaltRt_Name = ifelse(mslink == 226	|
                                mslink == 227, "W Salt Route 11", SaltRt_Name)) %>% #manually adding segments for w route 11 from route 27??
  mutate(SaltRt_Name = ifelse(segment_name.x == "S HIGH POINT RD" |
                                mslink == 10629
                              , "W Salt Route 12", SaltRt_Name)) %>% # manually adding segements for  w route 12
  mutate(SaltRt_Name = ifelse(mslink == 1002 |
                                mslink == 1003 |
                                mslink == 5700 |
                                mslink == 5701 |
                                mslink == 1004 |
                                mslink == 1011 |
                                mslink == 1012
                              , "W Salt Route 14", SaltRt_Name)) %>% #manually adding segments for w route 14
  mutate(SaltRt_Name = ifelse(mslink == 10209 |
                                mslink == 627 |
                                mslink == 630
                              , "W Salt Route 16", SaltRt_Name)) %>% #manually adding segments for w route 16 from route 30??
  mutate(routeloc = ifelse(str_detect(SaltRt_Name, "E "), "E", "W")) %>% #indicates East or West route
  mutate(routeloc = ifelse(SaltRt_Name == "<Null>", "E", routeloc)) %>% #Manual check of these null routes shows these are part of E route 4
  #mutate(routeno = parse_number(SaltRt_Name)) %>% #route number 
  #mutate(routeno = ifelse(SaltRt_Name == "<Null>", 4, routeno)) %>% #Manual check of these null routes shows these are part of E route 4
  mutate(calc_length_m = calc_length_ft / 3.280839895) %>% #convert length of road to [meters]
  mutate(surface_width_m = surface_width / 3.280839895) %>% #convert road widths to [meters] 
  mutate(surface_area_m2 = calc_length_m * surface_width_m) %>% #road area in [m^2]
  mutate(centerline_miles = calc_length_ft / 5280) %>% #calculate roadway in mileage
  mutate(lane_miles = centerline_miles * lanes) #calculate lane miles in each roadway




#East roads dataset for mapping only
library(plyr)
eastkey <- road_info %>%
  filter(RouteNumber != "NA")

E_Map_Geo <- SaltRoutes[SaltRoutes$mslink %in% eastkey$mslink, ] %>%
  mutate(RouteNumber = ifelse(segment_name == "S BEDFORD ST" |
                                mslink == 4718 |
                                mslink == 4719 |
                                mslink == 4708 |
                                mslink == 4993 |
                                mslink == 4969 |
                                mslink == 4381 |
                                mslink == 4728 |
                                segment_name == "E WILSON ST"
                              , 1, RouteNumber)) %>% #manually adding segments to E route 1 (some overlap with W route 1)
  mutate(RouteNumber = ifelse(segment_name == "MILLPOND RD" |
                                segment_name == "LONG DR" |
                                segment_name == "SAVANNAH RD" |
                                segment_name == "EVAN ACRES RD" |
                                mslink == 4167 |
                                mslink == 4168 |
                                mslink == 4169 |
                                mslink == 4172 |
                                mslink == 4232 |
                                mslink == 4036
                              , 15, RouteNumber)) %>% #manually adding segments to E route 15
  mutate(RouteNumber = ifelse(segment_name == "DUNWOODY DR" |
                                segment_name == "BADGER LN" |
                                segment_name == "FELL RD" |
                                segment_name == "GOLDEN GATE WAY" |
                                segment_name == "MOORLAND RD" |
                                segment_name == "LAKE FARM RD" |
                                segment_name == "NOB HILL RD" |
                                segment_name == "E BADGER RD" |
                                segment_name == "WAUNONA WAY" |
                                segment_name == "HARRIMAN LN" |
                                segment_name == "ETHELWYN RD" |
                                segment_name == "GREENLEAF DR" |
                                segment_name == "ESTHER BEACH RD" |
                                segment_name == "FRAZIER AVE" |
                                segment_name == "FAYETTE AVE" |
                                segment_name == "HOBOKEN RD" |
                                segment_name == "LAKE POINT DR" |
                                segment_name == "BRIDGE RD" |
                                mslink == 14051 |
                                mslink == 4166 |
                                segment_name == "DUTCH MILL RD" |
                                segment_name == "E BROADWAY" |
                                segment_name == "E BROADWAY (EB)" |
                                segment_name == "COLLINS CT" |
                                mslink == 17102 |
                                mslink == 21272 |
                                mslink == 21273 |
                                mslink == 21274 |
                                segment_name == "MARSH RD" |
                                segment_name == "VOGES RD" |
                                segment_name == "OWL CREEK DR" |
                                segment_name == "GREAT GRAY DR" |
                                segment_name == "VALOR WAY" |
                                segment_name == "BRANDENBURG WAY" |
                                segment_name == "FREESE LN",
                              16, RouteNumber)) %>% #manually adding segments for E route 16
  mutate(RouteNumber = as.character(RouteNumber))

#West roads dataset for mapping only
westkey <- road_info %>%
  filter(routeloc == "W")

W_Map_Geo <- SaltRoutes[SaltRoutes$mslink %in% westkey$mslink, ] %>%
  mutate(SaltRt_Name = ifelse(mslink == 4427 |
                                mslink == 4446 |
                                mslink == 4715 | 
                                mslink == 6178 | #route 17?
                                mslink == 6177  #route 17?
                              , "W Salt Route 1", SaltRt_Name)) %>% #manually adding segments for W route 1
  mutate(SaltRt_Name = ifelse(mslink == 6257, "W Salt Route 2", SaltRt_Name)) %>% #manually adding segments for W route 2 from route 18??
  mutate(SaltRt_Name = ifelse(mslink == 1185 | #route 20?
                                mslink == 1184 | #route 20?
                                mslink == 25750   #route 21?             
                              , "W Salt Route 5", SaltRt_Name)) %>% #manually adding segments for w route 5
  mutate(SaltRt_Name = ifelse(mslink == 23139 |
                                mslink == 5915 | #route 22
                                mslink == 23141 | #route 22
                                mslink == 23139 | #route 22
                                mslink == 5915 #route 22
                              , "W Salt Route 7", SaltRt_Name)) %>% #manually adding segments for w route 7
  mutate(SaltRt_Name = ifelse(mslink == 	5993	, "W Salt Route 8", SaltRt_Name)) %>% #manually adding segmetns for w route 8 from route 23??
  mutate(SaltRt_Name = ifelse(mslink == 6943 | 
                                mslink ==	6941 |
                                mslink == 6942 |
                                mslink == 6823 | #route 24?
                                mslink == 6609 | #route 24?
                                mslink == 6705 | #route 24?
                                mslink == 10726 | #route 24?
                                mslink == 24418 | #route 24?
                                mslink == 24417 #route 24?
                              , "W Salt Route 10", SaltRt_Name)) %>% #manually adding segements for w route 10
  mutate(SaltRt_Name = ifelse(mslink == 226	|
                                mslink == 227, "W Salt Route 11", SaltRt_Name)) %>% #manually adding segments for w route 11 from route 27??
  mutate(SaltRt_Name = ifelse(segment_name == "S HIGH POINT RD" |
                                mslink == 10629
                              , "W Salt Route 12", SaltRt_Name)) %>% # manually adding segements for  w route 12
  mutate(SaltRt_Name = ifelse(mslink == 1002 |
                                mslink == 1003 |
                                mslink == 5700 |
                                mslink == 5701 |
                                mslink == 1004 |
                                mslink == 1011 |
                                mslink == 1012 
                              , "W Salt Route 14", SaltRt_Name)) %>% #manually adding segments for w route 14
  mutate(SaltRt_Name = ifelse(mslink == 10209 |
                                mslink == 627 |
                                mslink == 630
                              , "W Salt Route 16", SaltRt_Name)) #manually adding segments for w route 16 from route 30??
  detach(package:plyr)







E_roads <- road_info %>%
  filter(RouteNumber != "NA") %>%
 # mutate(lanes = ifelse(mslink == 2780, 2, lanes),
#         lanes = ifelse(mslink == 2707, 2, lanes),
#         lanes = ifelse(mslink == 23284, 2, lanes),
#         lanes = ifelse(mslink == 25731, 4, lanes)) %>% #manually adding lane numbers to rows that had missing values. Checked using google maps and road segments before and after in the database 
  group_by(RouteNumber) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m),
    sum_area = sum(surface_area_m2)
  ) %>%
  mutate(RouteNumber = as.character(RouteNumber))




W_roads <- road_info %>%
  filter(routeloc == "W") %>%
  # mutate(lanes = ifelse(mslink == 2780, 2, lanes),
  #         lanes = ifelse(mslink == 2707, 2, lanes),
  #         lanes = ifelse(mslink == 23284, 2, lanes),
  #         lanes = ifelse(mslink == 25731, 4, lanes)) %>% #manually adding lane numbers to rows that had missing values. Checked using google maps and road segments before and after in the database 
  group_by(SaltRt_Name) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m),
    sum_area = sum(surface_area_m2)
  ) 








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


