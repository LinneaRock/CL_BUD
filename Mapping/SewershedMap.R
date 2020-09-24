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
  geom_sf(data = check, aes(color = SaltRt_Name)) +
  theme_bw() + 
  #theme(legend.position = "none") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(42.99, 43.39), xlim = c(-89.65, -89.1), expand = FALSE) # limit axes


EAST_SALT_ROUTES <- SaltRoutes %>%
  mutate(RouteNumber = ifelse(segment_name == "MILLPOND RD" |
                                segment_name == "LONG DR" |
                                segment_name == "SAVANNAH RD" |
                                segment_name == "EVAN ACRES RD" |
                                mslink == 4167 |
                                mslink == 4168 |
                                mslink == 4169 |
                                mslink == 4172 |
                                mslink == 4232,
                              15, RouteNumber)) %>% #manually adding segments to route 15
  mutate(RouteNumber = ifelse(segment_name == "DUNWOODY DR" |
                                segment_name == "BADGER LN" |
                                segment_name == "FELL RD" |
                                segment_name == "GOLDEN GATE WAY" |
                                segment_name == "MOORLAND RD" |
                                segment_name == "LAKE FARM RD" |
                                segment_name == "NOB HILL RD" |
                                segment_name == "E BADGER RD" |
                                segment_name == "WAUNONA WAY" |
                                segment_name == "HARRIMAN LN" |
                                segment_name == "ETHELWYN RD" |
                                segment_name == "GREENLEAF DR" |
                                segment_name == "ESTHER BEACH RD" |
                                segment_name == "FRAZIER AVE" |
                                segment_name == "FAYETTE AVE" |
                                segment_name == "HOBOKEN RD" |
                                segment_name == "LAKE POINT DR" |
                                segment_name == "BRIDGE RD" |
                                mslink == 14051 |
                                mslink == 4166 |
                                segment_name == "DUTCH MILL RD" |
                                segment_name == "E BROADWAY" |
                                segment_name == "E BROADWAY (EB)" |
                                segment_name == "COLLINS CT" |
                                mslink == 17102 |
                                mslink == 21272 |
                                mslink == 21273 |
                                mslink == 21274 |
                                segment_name == "MARSH RD" |
                                segment_name == "VOGES RD" |
                                segment_name == "OWL CREEK DR" |
                                segment_name == "GREAT GRAY DR" |
                                segment_name == "VALOR WAY" |
                                segment_name == "BRANDENBURG WAY" |
                                segment_name == "FREESE LN",
                              16, RouteNumber)) %>%
  mutate(RouteNumber = as.character(RouteNumber))


E_SaltRoute <- SaltRoutes %>%
  filter(str_detect(SaltRt_Name, "E ")) %>%
  mutate(routeno = parse_number(SaltRt_Name))


checkWroute <- SaltRoutes %>%
  filter(is.na(RouteNumber)) %>%
  filter(is.na(SaltRt_Name))
