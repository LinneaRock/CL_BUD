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
    sum_length_m = sum(calc_length_m)
    #sum_area = sum(surface_area_m2)
  ) %>%
  rename(ROUTE = RouteNumber) %>% #renamed for joining later with application data
mutate(ROUTE = as.character(ROUTE)) #assigned character here because it will not work in previous step for an unknown, dumb reason

W_roads <- road_info %>%
  filter(routeloc == "W") %>%
  group_by(SaltRt_Name) %>%
  summarise(
    sum_centerline_miles = sum(centerline_miles),
    sum_lane_miles = sum(lane_miles),
    sum_length_m = sum(calc_length_m)
    #sum_area = sum(surface_area_m2)
  ) %>%
  mutate(ROUTE = as.character(parse_number(SaltRt_Name))) #added for joining later with application data


#function to format road salt application data
#Convert tons and gallons to metric tonnes and liters
format_salt <- function(original) {
  df <- read_xlsx(original, sheet = "SHIFT TOTALS") %>%
    mutate(DATE = as.Date(as.character(DATE))) %>%
    drop_na(DATE) %>% #excel sheet had a lot of extra rows with no information
    mutate(Total_Salt_Mg = TOTAL * 0.907185,
           SALT_Mg = SALT_ton * 0.907185,
           SAND_Mg = SAND_ton * 0.907185,
           BRINE_l = BRINE_gal * 3.785411784) %>%
    dplyr::select(Total_Salt_Mg, DATE, SHIFT, ROUTE, SALT_load, SAND_load, BRINE_l, SALT_Mg, SAND_Mg)
  
}

#East Madison salt application per route 
E2017 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2017.xlsx") %>% mutate(year = "2017-2018") %>% mutate(route_des = "E")
E2018 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2018.xlsx") %>% mutate(year = "2018-2019") %>% mutate(route_des = "E")
E2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2019.xlsx") %>% mutate(year = "2019-2020") %>% mutate(route_des = "E")
E2020 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingEast2020.xlsx") %>% mutate(year = "2020-2021") %>% mutate(route_des = "E")

EAST_SALTING_FULL <- rbind(E2017, E2018, E2019, E2020)


#West Madison salt application per route per event
W2017 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2017.xlsx") %>% mutate(year = "2017-2018") %>% mutate(route_des = "W")
W2018 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2018.xlsx") %>% mutate(year = "2018-2019") %>% mutate(route_des = "W")
W2019 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2019.xlsx") %>% mutate(year = "2019-2020") %>% mutate(route_des = "W")
W2020 <- format_salt("Data/Road_Salt/Madison/MaterialUseTrackingWest2020.xlsx") %>% mutate(year = "2020-2021") %>% mutate(route_des = "W")

WEST_SALTING_FULL <- rbind(W2017, W2018, W2019, W2020)



ALL_SALT_FULL <- rbind(EAST_SALTING_FULL, WEST_SALTING_FULL) %>%
  mutate(Total_Salt_Mg = ifelse(is.na(Total_Salt_Mg), 0, Total_Salt_Mg))


TOTALS_BY_ROUTE <- ALL_SALT_FULL %>%
  group_by(year, DATE, ROUTE, route_des) %>%
  summarise(total_tonne = sum(Total_Salt_Mg))

TOTALS_BY_DATE <- ALL_SALT_FULL %>%
  group_by(year, DATE) %>%
  summarise(total_tonne = sum(Total_Salt_Mg))




WINTER_SALT_TOTALS <- read_xlsx("Data/Road_Salt/Madison/salt_totals.xlsx") %>%
  mutate(Total_Salt_Mg = TOTAL * 0.907185,
         SALT_Mg = SALT_ton * 0.907185,
         SAND_Mg = SAND_ton * 0.907185,
         BRINE_l = BRINE_gal * 3.785411784) %>%
  dplyr::select(Year, Total_Salt_Mg, BRINE_l, SALT_Mg, SAND_Mg)



ggplot(WINTER_SALT_TOTALS, aes(Year, Total_Salt_Mg)) +
  geom_bar(stat = "identity", color = "black",  fill = "#1C3668") +
  coord_flip() +
  theme_minimal() +
  labs(y = "Salt Applied (Mg)",
       x = "",
       caption = "Figure X. Metric tonnes (Mg) of salt applied per winter in the City of Madison from 2000-2021. Data from the City 
of Madison Streets Division.") +
  scale_y_reverse() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0))

ggsave("Plots/RoadSalt/madison_streets.png", height = 15, width = 20, units = "cm")






















