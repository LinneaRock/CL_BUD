# Load packages
library(sf)
library(tidyverse)
library(ggspatial)

## Esri basemap URLs ####
esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Manually build a dataframe 
gage.bb = data.frame(site = c('FakeGage'),lat = c(43.15), lon = c(-89.4))
gage.bb.sf = st_as_sf(gage.bb, coords = c("lon", "lat"), 
                      crs = 4326)

#  Map #### 
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = gage.bb.sf, color = 'black', size = 1.6, shape = 17) + # USGS gages
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.5, -89.3), expand = FALSE) # limit axes

ggsave('Map.png', width = 5, height = 5, units = 'in')


