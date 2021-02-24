source("Data/shapefiles/watersheds_tribs.R")
library(gt)
library(webshot)
library(tidyverse)
library(sf)
library(ggspatial)

# params <- charsWIC[["parameters"]] %>%
#   select(-value, - ID)
# write_rds(params, "Data/shapefiles/USGS_watershed_parameter_exp.rds")
params <- read_rds("Data/shapefiles/USGS_watershed_parameter_exp.rds")


build_dataset <- function(usgs_del_info, customName) {
  
  usgs_del_info <- usgs_del_info %>%
    dplyr::select(DRNAREA, CSL10_85, DEVNLCD01, FOREST, LC01CRPHAY, LC01HERB, LC01WATER, WETLAND, PRECIP, SNOFALL) %>%
    mutate(DRNAREA = DRNAREA * 258.999, #square miles to hectare
           PRECIP = PRECIP * 2.54, #inches to centimeters
           SNOFALL = SNOFALL * 2.54,
           CSL10_85 = CSL10_85 * 0.189394, #feet/mile to meters/kilometer
           name = customName)
  
}


usgs_ws_all <- build_dataset(wsYN, "YN") %>%
  bind_rows(build_dataset(wsYI, "YI")) %>%
  bind_rows(build_dataset(wsYS, "YS")) %>%
  bind_rows(build_dataset(wsSMC, "SMC")) %>%
  bind_rows(build_dataset(wsDC, "DC")) %>%
  bind_rows(build_dataset(wsPBMS, "PBMS")) %>%
  bind_rows(build_dataset(wsPBSF, "PBSF")) %>%
  bind_rows(build_dataset(wsWIC, "WIC")) %>%
  bind_rows(build_dataset(wsSW, "SW")) %>%
  bind_rows(build_dataset(wsSH, "SH")) %>%
  bind_rows(build_dataset(wsWC, "WC"))

write_rds(usgs_ws_all, "Data/usgs_ws_shapes.rds")

usgs_ws_all2 <- as.data.frame(usgs_ws_all) %>% dplyr::select(name,
                                                       DRNAREA,
                                                       LC01WATER,
                                                       DEVNLCD01,
                                                       FOREST,
                                                       LC01HERB,
                                                       LC01CRPHAY,
                                                       WETLAND,
                                                       PRECIP,
                                                       SNOFALL,
                                                       CSL10_85) %>% 
  left_join(wsroads, by = "name") %>%
  mutate_if(is.numeric, round, digits = 2) 
  

write_rds(usgs_ws_all2, "Data/usgs_ws_info_roads.rds")


# Make the tables
gt_tbl <- gt(usgs_ws_all2)
ws_usgs <- gt_tbl %>%
  cols_label(
    name = "Site ID",
    DRNAREA = "Drainage Area (ha)",
    LC01WATER = "Open Water Area (%)",
    DEVNLCD01 = "Developed Area (%)",
    #BarrenPerc = "Barren Area (%)",
    FOREST = "Forest Area (%)",
    LC01HERB = "Herbaceous Area (%)",
    LC01CRPHAY = "Cropland Area (%)",
    WETLAND = "Wetland Area (%)",
    PRECIP = "Average Annual Precipitation (cm)",
    SNOFALL = "Average Annual Snowfall (cm)",
    CSL10_85 = html("Stream Slope (m km<sup>-1<sup>)"),
    length_m = "Road Length (m)",
    road_density_mha = html("Road Density (m ha<sup>-1<sup>)")
    #TotalRoadDensity = html("Road Density m ha<sup>-1<sup>")
  ) %>%
  tab_header(
    title = "USGS Delineated Watershed Characteristics in the Upper Yahara River Watershed",
  ); ws_usgs

gtsave(data = ws_usgs, "Plots/USGS_Watershed/USGS_watershed_characteristics.png", expand = 10, zoom = 10)






#visualisations


#map of developed land gradient 
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = usgs_ws_all$geometry, aes(fill = usgs_ws_all$DEVNLCD01)) + 
  scale_fill_viridis_c(name = "Percentage of Developed Land (%)") +
  theme_bw() + # Hilary's default theme
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE)  # limit axes

ggsave('Plots/USGS_Watershed/USGSMap_Development.png', width = 6, height = 6, units = 'in') 




#map of ag land gradient 
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = usgs_ws_all$geometry, aes(fill = usgs_ws_all$LC01CRPHAY)) + 
  scale_fill_viridis_c(name = "Percentage of Cropland (%)") +
  theme_bw() + # Hilary's default theme
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE)  # limit axes

ggsave('Plots/USGS_Watershed/USGSMap_Cropland.png', width = 6, height = 6, units = 'in') 




#######################################################################################
#Road information in these catchments



WI_roads <- st_read("C:/Users/linne/Downloads/WIroads/wisconsin-latest-free.shp/gis_osm_roads_free_1.shp")
str(WI_roads) #classes sf and data.frame


road_classes <- WI_roads %>%
  as.data.frame()%>%
  select(fclass) %>%
  unique()


roads_in_wsDC <- st_intersection(WI_roads, wsDC)
roads_in_wsPBMS <- st_intersection(WI_roads, wsPBMS)
roads_in_wsPBSF <- st_intersection(WI_roads, wsPBSF)
roads_in_wsSH <- st_intersection(WI_roads, wsSH)
roads_in_wsSMC <- st_intersection(WI_roads, wsSMC)
roads_in_wsSW <- st_intersection(WI_roads, wsSW)
roads_in_wsWC <- st_intersection(WI_roads, wsWC)
roads_in_wsWIC <- st_intersection(WI_roads, wsWIC)
roads_in_wsYI <- st_intersection(WI_roads, wsYI)
roads_inwsYN <- st_intersection(WI_roads, wsYN)
roads_inwsYS <- st_intersection(WI_roads, wsYS)

roads_plot <- function(watershed, roads_in_watershed, name) {
  ggplot() +
    annotation_map_tile(type = world_gray, zoom = 12) +
    geom_sf(watershed, mapping = aes(), color = "#E5C4A1", fill = "#E5C4A1") +
    geom_sf(roads_in_watershed, mapping = aes()) 
  
  ggsave(paste('Plots/USGS_Watershed/', name,'.png', sep = ""), width = 6, height = 6, units = 'in') 
  
}

roads_plot(wsDC, roads_in_wsDC, "DC")
roads_plot(wsPBMS, roads_in_wsPBMS, "PBMS")
roads_plot(wsPBSF, roads_in_wsPBSF, "PBSF")
roads_plot(wsSH, roads_in_wsSH, "SH")
roads_plot(wsSMC, roads_in_wsSMC, "SMC")
roads_plot(wsSW, roads_in_wsSW, "SW")
roads_plot(wsWC, roads_in_wsWC, "WC")
roads_plot(wsWIC, roads_in_wsWIC, "WIC")
roads_plot(wsYI, roads_in_wsYI, "YI")
roads_plot(wsYN, roads_inwsYN, "YN")
roads_plot(wsYS, roads_inwsYS, "YS")




areaDC <- st_area(wsDC) / 10000 #[m^2 to ha]
lengthroadsDC <- sum(st_length(roads_in_wsDC))
road_densityDC <- lengthroadsDC/areaDC

areaPBMS <- st_area(wsPBMS) / 10000 #[m^2 to ha]
lengthroadsPBMS <- sum(st_length(roads_in_wsPBMS))
road_densityPBMS <- lengthroadsPBMS/areaPBMS

areaPBSF <- st_area(wsPBSF) / 10000 #[m^2 to ha]
lengthroadsPBSF <- sum(st_length(roads_in_wsPBSF))
road_densityPBSF <- lengthroadsPBSF/areaPBSF

areaSH <- st_area(wsSH) / 10000 #[m^2 to ha]
lengthroadsSH <- sum(st_length(roads_in_wsSH))
road_densitySH <- lengthroadsSH/areaSH

areaSW <- st_area(wsSW) / 10000 #[m^2 to ha]
lengthroadsSW <- sum(st_length(roads_in_wsSW))
road_densitySW <- lengthroadsSW/areaSW

areaSMC <- st_area(wsSMC) / 10000 #[m^2 to ha]
lengthroadsSMC <- sum(st_length(roads_in_wsSMC))
road_densitySMC <- lengthroadsSMC/areaSMC

areaWC <- st_area(wsWC) / 10000 #[m^2 to ha]
lengthroadsWC <- sum(st_length(roads_in_wsWC))
road_densityWC <- lengthroadsWC/areaWC

areaWIC <- st_area(wsWIC) / 10000 #[m^2 to ha]
lengthroadsWIC <- sum(st_length(roads_in_wsWIC))
road_densityWIC <- lengthroadsWIC/areaWIC

areaYI <- st_area(wsYI) / 10000 #[m^2 to ha]
lengthroadsYI <- sum(st_length(roads_in_wsYI))
road_densityYI <- lengthroadsYI/areaYI

areaYN <- st_area(wsYN) / 10000 #[m^2 to ha]
lengthroadsYN <- sum(st_length(roads_inwsYN))
road_densityYN <- lengthroadsYN/areaYN

areaYS <- st_area(wsYS) / 10000 #[m^2 to ha]
lengthroadsYS <- sum(st_length(roads_inwsYS))
road_densityYS <- lengthroadsYS/areaYS


name <- c("DC", "PBMS", "PBSF", "SH", "SMC", "SW", "WC", "WIC", "YI", "YN", "YS")
length <- c(lengthroadsDC, lengthroadsPBMS, lengthroadsPBSF, lengthroadsSH, lengthroadsSMC, lengthroadsSW, lengthroadsWC, lengthroadsWIC, lengthroadsYI, lengthroadsYN, lengthroadsYS)
road_density <- c(road_densityDC, road_densityPBMS, road_densityPBSF, road_densitySH, road_densitySMC, road_densitySW, road_densityWC, road_densityWIC, road_densityYI, road_densityYN, road_densityYS)

wsroads <- as.data.frame(name) %>%
  mutate(length_m = as.numeric(length)) %>%
  mutate(road_density_mha = as.numeric(road_density))
#add to the table on line 52

YS_road_types <- roads_inwsYS %>%
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


ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(YS_roads, mapping = aes(color = fclass))


YS_roads <- roads_inwsYS %>%
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


all_roads <- YS_roads %>%
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


# road_test <- all_roads %>%
#   select(osm_id, fclass, name, ref, lanes, geometry) %>%
#   st_set_crs(4326)
# 
# e_road_test <- E_Map_Geo %>%
#   select(mslink, segment_name, funct_class, Shape) %>%
#   rename(geometry = Shape) %>%
#   st_transform(crs = 4326)
# 
# new_test <- road_test %>%
#   st_join(e_road_test, by = "geometry")


library(rmapshaper)
madison_shape <- st_read("C:/Users/linne/Downloads/City_Limit/City_Limit.shp") %>% st_buffer(dist = 0)
roads_outside_Madison <- ms_erase(all_roads, madison_shape) %>%
  mutate(length = st_length(geometry)) %>%
  select(osm_id, fclass, name, ref, lanes, length, geometry)

#Dane county 2019-20 salt application rate was 16.44 tons per lane mile 

roads_outside_Madison <- roads_outside_Madison %>%
  mutate(lanemile = (length/1609) * lanes) %>%
  mutate(salt_app = (lanemile * 16.44) * 0.907185) #convert to Mg (metric tonne) 

winter1920_salt_outside_Madison <- sum(roads_outside_Madison$salt_app)
winter1920_salt_inside_Madison <- sum(Winter19$total_ton)
all_salt201920 <- winter1920_salt_inside_Madison + as.numeric(winter1920_salt_outside_Madison)
#chloride is 60.663% of the NaCl compound
chloride_201920 <- all_salt201920 * 0.60663


ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(madison_shape, mapping = aes(), fill = "light blue") +
  geom_sf(new_test3, mapping = aes(color = lanes))




