library(tidyverse)
library(sf)
library(ggspatial)
library(raster)

#Commented out code below is gathering shapefiles for HUC12 watersheds in UYRW. Saved the shapefiles as .rds files for future use to save space.

#HUC12.sf <- st_read("C:/Users/linne/Downloads/HU12/HU12.shp") 
#HUC12.sf.ME <- HUC12.sf %>%
#  filter(HUC12 == "070900020601" |
#           HUC12 == "070900020602" |
#           HUC12 == "070900020603" |
#           HUC12 == "070900020604" |
#           HUC12 == "070900020501" |
#           HUC12 == "070900020502" |
#           HUC12 == "070900020503" |
#           HUC12 == "070900020504")

#write_rds(HUC12.sf.ME, "Data/shapefiles/HUC12_ME.rds")

HUC12.sf.ME <- read_rds("Data/shapefiles/HUC12_ME.rds")

#HUC12.sf.MO <- HUC12.sf %>%
#  filter(HUC12 == "070900020701" |
#           HUC12 == "070900020702")

#write_rds(HUC12.sf.MO, "Data/shapefiles/HUC12_MO.rds")

HUC12.sf.MO <- read_rds("Data/shapefiles/HUC12_MO.rds")


#Plot the subwatersheds!
## Esri basemap URLs ####
#esri_land <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = HUC12.sf.ME, fill = NA, aes(color = "Mendota Watershed")) + 
  geom_sf(data = HUC12.sf.MO, fill = NA, aes(color = "Monona Watershed")) +
  geom_sf_label(HUC12.sf.ME, mapping = aes(label = HUC12), color = "#1C366B") +
  geom_sf_label(HUC12.sf.MO, mapping = aes(label = HUC12), color = "#F24D29") +
  scale_color_manual(values = c("#1C366B", "#F24D29")) +
  theme_bw() + # Hilary's default theme
  theme(legend.position = c(0.9,0.85), #This needs to change, legend is cut off of image
        legend.title = element_blank()) +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE)  # limit axes
 
ggsave('Plots/Huc12Map_label.png', width = 6, height = 6, units = 'in') #change name depending on if geom_sf_text/label is used

#Interlake watersheds shapefile (LAGOS delineated, not using)
iws.sf <- st_read("C:/Users/linne/Downloads/IWS/IWS.shp")
iws.sf.ME_MO <- iws.sf %>%
  filter(NHD_ID == "143249470" |
          NHD_ID == "143249640")


#map to compare HUC12s to IWS
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = iws.sf.ME_MO, aes(fill = NHD_ID)) + 
  geom_sf(data = HUC12.sf.MO, fill = NA, color = "#F24D29") + 
  geom_sf(data = HUC12.sf.ME, fill = NA, color = "#1C366B") +
  theme_bw() + 
  theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE) # limit axes

#Hydro Lines -- remove after using, it's a huge file to store in the global environment
hydro <- st_read("C:/Users/linne/Downloads/24k_Hydro_Flowlines__Rivers_Streams_-shp/24k_Hydro_Flowlines__Rivers_Streams_.shp")

#County shapefile if needed
county <- st_read("C:/Users/linne/Downloads/COUNTY/COUNTY.shp") %>%
  filter(STATE == "WI") %>%
  filter(Name == "Dane County")

#map of study area with hydrolines and watersheds
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = hydro, color = "#1DACE8") + 
  geom_sf(data = HUC12.sf.MO, fill = NA, color = "#F24D29") + 
  geom_sf(data = HUC12.sf.ME, fill = NA, color = "#1C366B") +
  theme_bw() + 
  theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE) # limit axes


ggsave('Plots/HydroMap.png', width = 6, height = 6, units = 'in')

#####################
unsure <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb/UWSaltLayers.gdb")

ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = unsure) +
  theme_bw() + 
  theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE) # limit axes
