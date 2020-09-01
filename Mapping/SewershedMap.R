library(tidyverse)
library(sf)
library(ggspatial)

#looking at datasets in geodatabase from City of Engineering
hydro <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "hydro")
Accessstructures <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Accessstructures") 
ImperviousParcelInt <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "ImperviousParcelInt")
#landuseDC2015 <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "landuseDC2015") #Huge dataset, not needed becasue LAGOS!
outfallbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "outfallbasins")
Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement") 
Pipes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pipes")
SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")
watersheds <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "watersheds")
WingraSubbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "WingraSubbasins")


ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = checkWroute) +
  theme_bw() + 
  #theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE) # limit axes



E_SaltRoute <- SaltRoutes %>%
  filter(str_detect(SaltRt_Name, "E ")) %>%
  mutate(routeno = parse_number(SaltRt_Name))


checkWroute <- SaltRoutes %>%
  filter(is.na(RouteNumber)) %>%
  filter(is.na(SaltRt_Name))
