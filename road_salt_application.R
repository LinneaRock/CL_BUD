source("Data/code/road_salt_data.R")
library(readxl)

#Summarize salting route information to get lane miles per route
#east and west routes use RouteNumber and SaltRt_Name, respectively, to summarize the data because of the way the geodatabase was organized 
E_roads <- road_info %>%
  filter(RouteNumber != "NA") %>%
  group_by(RouteNumber) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m),
    sum_area = sum(surface_area_m2)
  ) %>%
  rename(ROUTE = RouteNumber) %>% #renamed for joining later with application data
mutate(ROUTE = as.character(ROUTE)) #assigned character here because it will not work in previous step for an unknown, dumb reason

W_roads <- road_info %>%
  filter(routeloc == "W") %>%
  group_by(SaltRt_Name) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m),
    sum_area = sum(surface_area_m2)
  ) %>%
  mutate(ROUTE = as.character(parse_number(SaltRt_Name))) #added for joining later with application data




#function to format road salt application data
#Convert tons and gallons to metric tonnes and liters
format_salt <- function(original) {
  df <- read_xlsx(original, sheet = "SHIFT TOTALS") %>%
    mutate(DATE = as.Date(as.character(DATE))) %>%
    filter(DATE < "2020-03-04") %>% #excel sheet had a lot of extra rows with no information
    mutate(Total_Salt_Mg = `TOTAL SALT TONS` * 0.907185,
           SALT_Mg = SALT_ton * 0.907185,
           SAND_Mg = SAND_ton * 0.907185,
           BRINE_l = BRINE_gal * 3.785411784) %>%
    dplyr::select(Total_Salt_Mg, DATE, SHIFT, ROUTE, SALT_load, SAND_load, BRINE_l, SALT_Mg, SAND_Mg)
  
}

#2019-20 East Madison salt application per route 
E2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx")
#2019-20 West Madison salt application per route per event
W2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx")


#2019-20 East Madison salt application per route per date
E19_perdate <- E2019 %>%
  group_by(DATE, ROUTE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))
#2019-20 West Madison salt application per route per date
W19_perdate <- W2019 %>%
  group_by(DATE, ROUTE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

#2019-20 East Madison salt application per route per season
E19_season <- E2019 %>%
  group_by(ROUTE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))
#2019-20 West Madison salt application per route per season
W19_season <- W2019 %>%
  group_by(ROUTE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

#2019-20 salt application per date for City
Winter19 <- E2019 %>%
  rbind(W2019) %>%
  group_by(DATE) %>%
  summarise(total_ton = sum(Total_Salt_Mg))

#Salt per lane mile at each salting route during an event
#East Madison Salt per Lane Mile
E_salt_lanemi_route <- left_join(E2019, E_roads, by = "ROUTE") %>%
  select(DATE, ROUTE, Total_Salt_Mg, sum_lane_miles) %>%
  mutate(app_rate = (Total_Salt_Mg / sum_lane_miles)*1000) #[kg lane mile^-1]

#West Madison Salt per Lane Mile 
W_salt_lanemi_route <- left_join(W2019, W_roads, by = "ROUTE") %>%
  select(DATE, ROUTE, Total_Salt_Mg, sum_lane_miles) %>%
  mutate(app_rate = (Total_Salt_Mg / sum_lane_miles)*1000) #[kg lane mile^-1]


#Salt per lane mile at each salting route during a day
#East Madison Salt per Lane Mile
E_salt_lanemi_day <- left_join(E19_perdate, E_roads, by = "ROUTE")%>%
  select(DATE, ROUTE, total_ton, sum_lane_miles) %>%
  mutate(app_rate = (total_ton / sum_lane_miles)*1000) #[kg lane mile^-1]

#West Madison Salt per Lane Mile 
W_salt_lanemi_day <- left_join(W19_perdate, W_roads, by = "ROUTE")%>%
  select(DATE, ROUTE, total_ton, sum_lane_miles) %>%
  mutate(app_rate = (total_ton / sum_lane_miles)*1000) #[kg lane mile^-1]


#Salt per lane mile at each salting route during season
#East Madison Salt per Lane Mile
E_salt_lanemi_season <- left_join(E19_season, E_roads, by = "ROUTE")%>%
  select(ROUTE, total_ton, sum_lane_miles) %>%
  mutate(app_rate = (total_ton / sum_lane_miles)) #[Mg lane mile^-1]

#West Madison Salt per Lane Mile 
W_salt_lanemi_season <- left_join(W19_season, W_roads, by = "ROUTE")%>%
  select(ROUTE, total_ton, sum_lane_miles) %>%
  mutate(app_rate = (total_ton / sum_lane_miles)) #[Mg lane mile^-1]
