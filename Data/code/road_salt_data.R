# library(tidyverse)
# library(sf)

#the code to create these is below.
# E_Map_Geo <- read_rds("Data/code/E_Map_Geo2.rds")
# W_Map_Geo <- read_rds("Data/code/W_Map_Geo.rds")
# road_info <- read_rds("Data/code/road_info.rds")



#read in applicable layers from geodatabase
 Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement")
 SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")
# 
# #Combine SaltRoutes with Pavement to get number of lanes in each salting segment
# #They need to be converted to dataframes in order to use left_join
# #keep applicable columns so I don't have to deal with 97 variables...
# #Additionally a lot of manual work needed to ensure this dataset matches the PDF sent by City Streets
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
                                mslink == 25750 |  #route 21?
                                mslink == 6070 |
                                mslink == 6074 |
                                mslink == 6683 |
                                mslink == 6682 |
                                mslink == 6741 |
                                mslink == 6742|
                                mslink == 6743 |
                                mslink == 6744 |
                                segment_name.x == "ANN ST" |
                                segment_name.x == "PERRY ST" |
                                segment_name.x == "GILMORE ST" |
                                segment_name.x == "WESTERN AVE"
                              , "W Salt Route 5", SaltRt_Name)) %>% #manually adding segments for w route 5
  mutate(SaltRt_Name = ifelse(segment_name.x == "S MIDVALE BLVD" |
                                segment_name.x == "GLEN DR" |
                                segment_name.x == "S MIDVALE BLVD"|
                                mslink == 23108 |
                                mslink == 23107
                              , "W Salt Route 6", SaltRt_Name)) %>% #manually adding segments for w route 6
  mutate(SaltRt_Name = ifelse(segment_name.x == "MINERAL POINT RD" &
                                SaltRt_Name == "W Salt Route 4"
                              , "W Salt Route 7", SaltRt_Name)) %>%
  mutate(SaltRt_Name = ifelse(mslink == 23139 |
                                mslink == 5915 | #route 22
                                mslink == 23141 | #route 22
                                mslink == 23139 | #route 22
                                mslink == 5915 | #route 22
                                mslink == 5868 |
                                mslink == 5867 |
                                mslink == 5978 |
                                mslink == 5979 |
                                mslink == 5953 |
                                segment_name.x == "N FRANKLIN AVE" |
                                segment_name.x == "FARLEY AVE"
                              , "W Salt Route 7", SaltRt_Name)) %>% #manually adding segments for w route 7
  mutate(SaltRt_Name = ifelse(mslink == 	5993 |
                                mslink ==	6844 |
                                mslink == 6846 |
                                segment_name.x == "N WHITNEY WAY"
                                , "W Salt Route 8", SaltRt_Name)) %>% #manually adding segmetns for w route 8 from route 23??
  mutate(SaltRt_Name = ifelse(segment_name.x == "NEW WASHBURN WAY" |
                                segment_name.x == "WELTON DR" |
                                mslink == 6943 |
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
                                mslink == 630 |
                                segment_name.x == "EXCELSIOR DR" |
                                segment_name.x == "DEMING WAY" |
                                segment_name.x == "FOURIER DR" |
                                mslink ==	12078 |
                                segment_name.x == "SWALLOWTAIL DR" |
                                segment_name.x == "TIMBER WOLF TRL" |
                                segment_name.x == "SETTLERS RD" |
                                segment_name.x == "VALLEY VIEW RD"
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


#

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

E_Map_Geo2 <- E_Map_Geo %>%
  mutate(Salt_Route_no = paste("E Salt Route ", RouteNumber, sep = ""))


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
                                mslink == 25750 |  #route 21?
                                mslink == 6070 |
                                mslink == 6074 |
                                mslink == 6683 |
                                mslink == 6682 |
                                mslink == 6741 |
                                mslink == 6742|
                                mslink == 6743 |
                                mslink == 6744 |
                                segment_name == "ANN ST" |
                                segment_name == "PERRY ST" |
                                segment_name == "GILMORE ST" |
                                segment_name == "WESTERN AVE"
                              , "W Salt Route 5", SaltRt_Name)) %>% #manually adding segments for w route 5
  mutate(SaltRt_Name = ifelse(segment_name == "S MIDVALE BLVD" |
                                segment_name == "GLEN DR" |
                                segment_name == "S MIDVALE BLVD" |
                                mslink == 23108 |
                                mslink == 23107
                              , "W Salt Route 6", SaltRt_Name)) %>% #manually adding segments for w route 6
  mutate(SaltRt_Name = ifelse(segment_name == "MINERAL POINT RD" &
                                SaltRt_Name == "W Salt Route 4"
                              , "W Salt Route 7", SaltRt_Name)) %>%
  mutate(SaltRt_Name = ifelse(mslink == 23139 |
                                mslink == 5915 | #route 22
                                mslink == 23141 | #route 22
                                mslink == 23139 | #route 22
                                mslink == 5915 | #route 22
                                mslink == 5868 |
                                mslink == 5867 |
                                mslink == 5978 |
                                mslink == 5979 |
                                mslink == 5953 |
                                segment_name == "N FRANKLIN AVE" |
                                segment_name == "FARLEY AVE"
                              , "W Salt Route 7", SaltRt_Name)) %>% #manually adding segments for w route 7
  mutate(SaltRt_Name = ifelse(mslink == 5993	|
                                mslink ==	6844 |
                                mslink == 6846 |
                                segment_name == "N WHITNEY WAY"
                                , "W Salt Route 8", SaltRt_Name)) %>% #manually adding segmetns for w route 8 from route 23??
  mutate(SaltRt_Name = ifelse(segment_name == "NEW WASHBURN WAY" |
                                segment_name == "WELTON DR" |
                                mslink == 6943 |
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
                                mslink == 630 |
                                segment_name == "EXCELSIOR DR" |
                                segment_name == "DEMING WAY" |
                                segment_name == "FOURIER DR" |
                                mslink ==	12078 |
                                segment_name == "SWALLOWTAIL DR" |
                                segment_name == "TIMBER WOLF TRL" |
                                segment_name == "SETTLERS RD" |
                                segment_name == "VALLEY VIEW RD"
                              , "W Salt Route 16", SaltRt_Name))  #manually adding segments for w route 16 from route 30??
  detach(package:plyr)



#select the important things so it is easier to work with
E_Map_Geo <- E_Map_Geo2 %>%
  select(RouteNumber, Salt_Route_no, mslink, funct_class, calc_length_ft, Shape)

W_Map_Geo <- W_Map_Geo %>%
  mutate(RouteNumber = as.numeric(str_sub(SaltRt_Name, c(-1, -2)))) %>%
  rename(Salt_Route_no = SaltRt_Name) %>%
  select(RouteNumber, Salt_Route_no, mslink, funct_class, calc_length_ft, Shape)
  



# saveRDS(E_Map_Geo2, "Data/code/E_Map_Geo2.rds")
# saveRDS(W_Map_Geo, "Data/code/W_Map_Geo.rds")
# saveRDS(road_info, "Data/code/road_info.rds")

#average lane numbers for county roads later.
road_info2 <- road_info %>%
  select(funct_class.x, lanes) %>%
  group_by(funct_class.x) %>%
  summarise(mean(lanes, na.rm = TRUE))

rm(Pavement)
rm(SaltRoutes)
rm(westkey)
rm(eastkey)
