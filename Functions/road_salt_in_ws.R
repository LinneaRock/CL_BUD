
road_salt_in_ws <- function(roads_in_ws, watershed.shp, checkplotname) {

  #################salt in county (outside of Madison)
    
  #filter to get rid of the above impervious surfaces and residential and service roads
  roads1 <- roads_in_ws %>%
    filter(fclass != "track" &
             fclass != "track_grade1" &
             fclass != "track_grade2" &
             fclass != "track_grade3" &
             fclass != "track_grade5" &
             fclass != "living_street" &
             fclass != "bridleway" &
             fclass != "steps" &
             fclass != "pedestrian" &
             fclass != "path" &
             fclass != "footway" &
             fclass != "cycleway" &
             fclass != "residential" & #possibly salted outside of Madison
             fclass != "service")  #very probably salted by private companies
  
  #provide lanes based on average lanes from road_info2 dataframe
  all_roads <- roads1 %>%
    mutate(lanes = NA) %>%
    mutate(lanes = ifelse(fclass == "motorway", 8, lanes),
           lanes = ifelse(fclass == "motorway_link", 1, lanes),
           lanes = ifelse(fclass == "primary", 3, lanes),
           lanes = ifelse(fclass == "primary_link", 1, lanes),
           lanes = ifelse(fclass == "secondary", 2, lanes),
           lanes = ifelse(fclass == "secondary_link", 1, lanes),
           lanes = ifelse(fclass == "tertiary", 2, lanes),
           lanes = ifelse(fclass == "tertiary_link", 1, lanes),
           lanes = ifelse(fclass == "trunk", 4, lanes),
           lanes = ifelse(fclass == "trunk_link", 1, lanes),
           lanes = ifelse(fclass == "unclassified", 2, lanes)
    )
  
  library(rmapshaper)
  madison_shape <- st_read("C:/Users/linne/Downloads/City_Limit/City_Limit.shp") %>% st_buffer(dist = 0)
 
   roads_outside_Madison <- ms_erase(all_roads, madison_shape) %>%
    mutate(length = st_length(geometry)) %>%
    select(osm_id, fclass, name, ref, lanes, length, geometry)
  
  
  #Dane county 2019-20 salt application rate was 16.44 tons per lane mile 
  roads_outside_Madison20 <- roads_outside_Madison %>%
    mutate(lanemile = (length/1609) * lanes) %>%
    mutate(salt_app = (lanemile * 16.44) * 0.907185) #convert US tons to Mg (metric tonne) 
  
  winter1920_salt_outside_MadisonMg <- sum(roads_outside_Madison20$salt_app)  
  #Dane county 2020-21 salt application rate was 13.8 tons per lane mile 
  
  roads_outside_Madison21 <- roads_outside_Madison %>%
    mutate(lanemile = (length/1609) * lanes) %>%
    mutate(salt_app = (lanemile * 13.8) * 0.907185) #convert US tons to Mg (metric tonne) 
  
  winter2021_salt_outside_MadisonMg <- sum(roads_outside_Madison21$salt_app)
  
###########salt in City of Madison  
  
  E_map_Geo2 <- st_transform(E_Map_Geo, crs = 4326)
  W_Map_Geo2 <- st_transform(W_Map_Geo, crs = 4326)
  wsYS <- st_transform(wsYS, crs = 4326)
  
  Mad_W <- st_intersection(W_Map_Geo2, watershed.shp)
  Mad_E <- st_intersection(E_map_Geo2, watershed.shp)
  
  #Madison WEST routes
  Mad_W <- Mad_W %>%
    mutate(length = st_length(Shape)) %>%
    mutate(mile = length / 1609) %>% #meters to miles
    select(mslink, funct_class, Salt_Route_no, RouteNumber, length, mile, Shape)
  
  westkey <- road_info %>% select(mslink, lanes)
  
  Mad_W <- Mad_W %>%
    left_join(westkey, by = "mslink")
  
  Mad_W <- Mad_W %>%
    mutate(lanemi = lanes * mile)
  
  West20 <- left_join(Mad_W, (WEST_SALTING_FULL %>% select(Total_Salt_Mg, DATE, SHIFT, ROUTE, year) %>% mutate(ROUTE = as.numeric(ROUTE)) %>% filter(year == "2019-2020")), by = c("RouteNumber" = "ROUTE"))%>%
    left_join(E_roads %>% mutate(ROUTE = as.numeric(ROUTE)), by = c("RouteNumber" = "ROUTE")) %>%
    mutate(percentage = lanemi / sum_lane_miles) %>%
    mutate(totalper = percentage * Total_Salt_Mg)
  
  West_Mad_201920 <- sum(West20$totalper)
  
  West21 <- left_join(Mad_W, (WEST_SALTING_FULL %>% select(Total_Salt_Mg, DATE, SHIFT, ROUTE, year) %>% mutate(ROUTE = as.numeric(ROUTE)) %>% filter(year == "2020-2021")), by = c("RouteNumber" = "ROUTE"))%>%
    left_join(E_roads %>% mutate(ROUTE = as.numeric(ROUTE)), by = c("RouteNumber" = "ROUTE")) %>%
    mutate(percentage = lanemi / sum_lane_miles) %>%
    mutate(totalper = percentage * Total_Salt_Mg)
  
  West_Mad_202021 <- sum(West21$totalper)
  
  #Madison EAST routes
  Mad_E <- Mad_E %>%
    mutate(length = st_length(Shape)) %>%
    mutate(mile = length / 1609) %>% #meters to miles
    select(mslink, funct_class, Salt_Route_no, RouteNumber, length, mile, Shape) %>%
    mutate(RouteNumber = as.numeric(RouteNumber))
  
  eastkey <- road_info %>% select(mslink, lanes)
  
  Mad_E <- Mad_E %>%
    left_join(eastkey, by = "mslink")
  
  Mad_E <- Mad_E %>%
    mutate(lanemi = lanes * mile)
  
  
  East20 <- left_join(Mad_E, (EAST_SALTING_FULL %>% select(Total_Salt_Mg, DATE, SHIFT, ROUTE, year) %>% mutate(ROUTE = as.numeric(ROUTE)) %>% filter(year == "2019-2020")), by = c("RouteNumber" = "ROUTE"))%>%
    left_join(E_roads %>% mutate(ROUTE = as.numeric(ROUTE)), by = c("RouteNumber" = "ROUTE")) %>%
    mutate(percentage = lanemi / sum_lane_miles) %>%
    mutate(totalper = percentage * Total_Salt_Mg)
  
  East_Mad_201920 <- sum(East20$totalper)
  
  East21 <- left_join(Mad_E, (EAST_SALTING_FULL %>% select(Total_Salt_Mg, DATE, SHIFT, ROUTE, year) %>% mutate(ROUTE = as.numeric(ROUTE)) %>% filter(year == "2020-2021")), by = c("RouteNumber" = "ROUTE"))%>%
    left_join(E_roads %>% mutate(ROUTE = as.numeric(ROUTE)), by = c("RouteNumber" = "ROUTE")) %>%
    mutate(percentage = lanemi / sum_lane_miles) %>%
    mutate(totalper = percentage * Total_Salt_Mg)
  
  East_Mad_202021 <- sum(East21$totalper)
  
  
  
  test <- as.data.frame(winter1920_salt_outside_MadisonMg) %>%
    mutate(winter2021_salt_outside_MadisonMg = as.numeric(winter2021_salt_outside_MadisonMg)) %>%
    mutate(winter1920_salt_outside_MadisonMg = as.numeric(winter1920_salt_outside_MadisonMg)) %>%
    mutate(Madison_201920 = as.numeric(East_Mad_201920) + as.numeric(West_Mad_201920)) %>%
    mutate( Madison_202021 = as.numeric( East_Mad_202021) + as.numeric(West_Mad_202021))
  
  ggplot() +
    annotation_map_tile(type = world_gray, zoom = 12) +
    geom_sf(watershed.shp, mapping = aes(), color = "#E5C4A1", fill = "#E5C4A1") +
    geom_sf(Mad_W, mapping = aes(), color = "blue") +
    geom_sf(Mad_E, mapping = aes(), color = "red") +
    geom_sf(roads_outside_Madison, mapping = aes())
  ggsave(paste("Plots/RoadSalt/check/",checkplotname, ".png", sep = ""))
  
  return(test)
  
}
  




  
  
  

  
