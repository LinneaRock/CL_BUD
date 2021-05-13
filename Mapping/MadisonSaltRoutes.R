library(tidyverse)
library(sf)
library(ggspatial)
source("Functions/L_theme.R")

E_Map_Geo <- read_rds("Data/code/E_Map_Geo2.rds") %>% 
  dplyr::mutate(route = parse_number(Salt_Route_no)) %>% 
  arrange(route)
E_Map_Geo$label = factor(E_Map_Geo$Salt_Route_no, levels = unique(E_Map_Geo$Salt_Route_no))

W_Map_Geo <- read_rds("Data/code/W_Map_Geo.rds") %>% 
  dplyr::mutate(route = parse_number(Salt_Route_no)) %>% # pulling out route and then arranging in numerical order 
  arrange(route) 
W_Map_Geo$label = factor(W_Map_Geo$Salt_Route_no, levels = unique(W_Map_Geo$Salt_Route_no)) #changing to factor and setting levels based on numerical order

# Easier to combine them for labeling. Otherwise does annoying default ordering...
combo_rts = W_Map_Geo %>% dplyr::select(label) %>% rbind(dplyr::select(E_Map_Geo, label))

# CRS was working for me... I might have an older version of GDAL. You might not need this. 
st_crs(combo_rts) = '+proj=lcc +lat_1=43.23055555555555 +lat_2=42.90833333333333 +lat_0=41.75 +lon_0=-89.42222222222222 +x_0=247193.2943865888 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs '

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot(combo_rts) +
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = combo_rts, aes(color = label)) +
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
