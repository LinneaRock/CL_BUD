# Load packages
library(sf)
library(tidyverse)
library(ggspatial)
library(patchwork)
library(raster)
library(ggrepel)

## Esri basemap URLs ####
esri_land <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# Manually build dataframes with cooridnates of sampling locations
gage.bb = data.frame(site = c('YN', '6MC', 'DC', 'PBMS', 'PBSF', 'YI'),lat = c(43.15083333, 43.14683333, 43.14027778, 43.10333333, 43.09861111, 43.08944444), lon = c(-89.40194444, -89.43694444, -89.44222222, -89.51166667, -89.52138889, -89.36083333))
gage.bb.sf = st_as_sf(gage.bb, coords = c("lon", "lat"), 
                      crs = 4326)

nogage.bb <- data.frame(site = c('SW', 'YS', 'WIC'), lat = c(43.09259, 43.04718, 43.05416667), lon = c(-89.33318, -89.33605, -89.379444))
nogage.bb.sf <- st_as_sf(nogage.bb, coords = c("lon", "lat"), 
                      crs = 4326)

lakebuoys <- data.frame(site = c('ME', 'MO'), lat = c(43.097951, 43.063516), lon = c(-89.404744, -89.360746))
lakebuoys.sf <- st_as_sf(lakebuoys, coords = c("lon", "lat"),
                         crs = 4326)

 sh <- data.frame(site = 'SH', lat = 43.079166, lon = -89.470833)
 sh.sf <- st_as_sf(sh, coords = c("lon", "lat"),
                          crs = 4326)
# 
# wc <- data.frame(site = 'WC', lat = 43.07679, lon = -89.42150)
# wc.sf <- st_as_sf(wc, coords = c("lon", "lat"),
#                   crs = 4326)

 #creating this for labels
 label <- bind_rows(gage.bb.sf, nogage.bb.sf, lakebuoys.sf, sh.sf) %>%
   mutate(lat = unlist(map(label$geometry, 1)),
          long = unlist(map(label$geometry, 2)))

 
#  Map #### 
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = gage.bb.sf, aes(shape = 'USGS-gaged 
river'), color = 'black', size = 1.6) + # USGS gages 
  geom_sf(data = nogage.bb.sf, aes(shape = 'Rivers'), color = 'black', size = 1.6) + #rivers without gages
  geom_sf(data = lakebuoys.sf, aes(shape = 'Lakes'), color = 'black', size = 1.6) + #lake sites
  geom_sf(data = sh.sf, aes(shape = 'USGS-gaged 
Storm Sewer'), color = 'black', size = 1.6) + #spring harbor 
   geom_text_repel(data = label, mapping = aes(lat, long, label = site)) + 
  #geom_sf(data = wc.sf, aes(shape = 'Storm Sewer'), size = 1.6) + #Willow creek
 # scale_shape_manual('Legend', values=c('USGS River' = 17, 'Rivers' = 19, 'Lakes' = 15, 'USGS Storm Sewer' = 24, 'Storm Sewer' = 21)) + 
  theme_bw() + # Hilary's default theme
  theme(legend.position = c(0.9,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 9)) +
   guides(shape=guide_legend(nrow=2,byrow=TRUE)) +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.55, -89.3), expand = FALSE) + # limit axes
  labs(caption = "Figure 7. Map of study sites. These are sites that were monitored from 2019 - 
2021, using data loggers, grab sampling, and some with USGS gaging.") +
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
   theme(plot.caption = element_text(size = 10, hjust = 0),
         axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())
 

ggsave('Plots/Map.png', width = 6.25, height = 4.25, units = 'in')

