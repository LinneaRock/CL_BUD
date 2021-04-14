


# wsME <- delineateWatershed(xlocation = -89.37031380616453, ylocation = 43.095060490629905, crs = 4326,
#                             includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsME)
# charsMe <- computeChars(workspaceID = wsME$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsME, layer = "ws_ME", dir = "Data/shapefiles/ME", what = "boundary")
wsME <- read_sf("Data/shapefiles/ME/ws_ME.shp")

#Road information in the catchments

WI_roads <- st_read("C:/Users/linne/Downloads/WIroads/wisconsin-latest-free.shp/gis_osm_roads_free_1.shp")
str(WI_roads) #classes sf and data.frame


road_classes <- WI_roads %>%
  as.data.frame()%>%
  dplyr::select(fclass) %>%
  unique()

#roads_in_wsME <- st_intersection(WI_roads, wsME)
write_rds(roads_in_wsME, "Data/shapefiles/ME/roads_in_wsME.rds")

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


  ggplot() +
    annotation_map_tile(type = world_gray, zoom = 12) +
    geom_sf(wsME, mapping = aes(), color = "#E5C4A1", fill = "#E5C4A1") +
    geom_sf(roads_in_wsME, mapping = aes()) +
    theme_bw() +
    theme(plot.caption = element_text(size = 10, hjust = 0),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  ggsave("Plots/Mendota_roads.png", width = 20, height = 15, units = "cm")  

  
  areaME <- st_area(wsME) / 10000 #[m^2 to ha]
  lengthroadsME <- sum(st_length(roads_in_wsME))
  road_densityME <- lengthroadsME/areaME

  
  ME_road_types <- roads_in_wsME %>%
    as.data.frame() %>%
    group_by(fclass) %>%
    tally() %>%
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
             fclass != "cycleway")
  
  ME_roads <- roads_in_wsME %>%
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
  
  all_roads <- ME_roads %>%
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
  
  E_map_Geo2 <- st_transform(E_Map_Geo, crs = 4326)
  W_map_Geo2 <- st_transform(W_Map_Geo, crs = 4326)
  wsME2 <- st_transform(wsME, crs = 4326)
  
  Mad_W <- st_intersection(W_map_Geo2, wsME2)  
  Mad_E <- st_intersection(E_map_Geo2, wsME2)
  
  ggplot() +
    annotation_map_tile(type = world_gray, zoom = 12) +
    geom_sf(wsME, mapping = aes(), color = "#E5C4A1", fill = "#E5C4A1") +
    geom_sf(Mad_W, mapping = aes(), color = "blue") +
    geom_sf(Mad_E, mapping = aes(), color = "red") +
    geom_sf(roads_outside_Madison, mapping = aes())
  
  #Dane county 2019-20 salt application rate was 16.44 tons per lane mile 
  roads_outside_Madison <- roads_outside_Madison %>%
    mutate(lanemile = (length/1609) * lanes) %>%
    mutate(salt_app = (lanemile * 16.44) * 0.907185) #convert to Mg (metric tonne) 
  
  #Madison West routes 
  Mad_W <- Mad_W %>%
    mutate(length = st_length(Shape)) %>%
    mutate(RouteNumber = str_sub(SaltRt_Name, -1)) %>%
    mutate(mile = length * 0.000621371) %>% #meters to miles
    select(mslink, funct_class, SaltRt_Name, RouteNumber, length, mile, Shape) 
  
  westkey2 <- westkey %>% select(mslink, lanes)
  
  Mad_W <- Mad_W %>%
    left_join(westkey2, by = "mslink")
  
  Mad_W <- Mad_W %>% 
    mutate(lanemi = lanes * mile)
 
  Mad_E <- Mad_E %>%
    mutate(length = st_length(Shape)) %>%
    mutate(mile = length * 0.000621371) %>% #meters to miles
    select(mslink, funct_class, SaltRt_Name, RouteNumber, length, mile, Shape)
  
 eastkey2 <- eastkey %>% select(mslink, lanes)
 
 Mad_E <- Mad_E %>% 
   left_join(eastkey2, by = "mslink") 
 
 Mad_E <- Mad_E %>% 
   mutate(lanemi = lanes * mile)
  
 
 West <- left_join(Mad_W, W_salt_lanemi_day, by = c("RouteNumber" = "ROUTE")) %>%
   select(-total_ton) %>%
   select(-sum_lane_miles)
 
 W_perday <- West %>%
   mutate(perday = lanemi * app_rate) %>%
   select(DATE, RouteNumber, SaltRt_Name, app_rate, perday)
 
 W_sum <- sum(W_perday$perday, na.rm = TRUE) * 0.001 #(tonnes)
 
 
East <- left_join(Mad_E, E_salt_lanemi_day, by = c("RouteNumber" = "ROUTE")) %>%
   select(-total_ton) %>%
   select(-sum_lane_miles)
 
 E_perday <- East %>%
   mutate(perday = lanemi * app_rate) %>%
   select(DATE, RouteNumber, SaltRt_Name, app_rate, perday)
 
 E_sum <- sum(E_perday$perday, na.rm = TRUE) * 0.001 #(tonnes)
 
 
 
 
 
 
 
  winter1920_salt_outside_Madison <- sum(roads_outside_Madison$salt_app)
  winter1920_salt_inside_Madison <- E_sum + W_sum
  all_salt201920 <- 14426 + 1639
  #chloride is 60.663% of the NaCl compound
  chloride_201920 <- all_salt201920 * 0.60663
  