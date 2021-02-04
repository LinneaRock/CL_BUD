source("Data/shapefiles/watersheds_tribs.R")
library(gt)
library(webshot)

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
  mutate_if(is.numeric, round, digits = 2)

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
    CSL10_85 = html("Stream Slope (m km<sup>-1<sup>)")
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






