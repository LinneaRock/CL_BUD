library(tidyverse)
library(sf)
library(ggspatial)
source("Functions/L_theme.R")

E_Map_Geo <- read_rds("Data/code/E_Map_Geo2.rds")
W_Map_Geo <- read_rds("Data/code/W_Map_Geo.rds")

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = W_Map_Geo, aes(color = SaltRt_Name)) +
  geom_sf(data = E_Map_Geo, aes(color = Salt_Route_no)) +
  scale_color_viridis_d(option = "inferno", name = "Salt Route") +
  theme_bw() + 
  #theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
  labs(caption = "Figure X. City of Madison road salt application routes (data from City of Madison).") +
  L_theme() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("Plots/RoadSalt/routes.png", width = 20, height = 15, units = "cm")
