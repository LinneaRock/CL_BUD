library(tidyverse)
library(sf)


WI_roads <- st_read("C:/Users/linne/Downloads/WIroads/wisconsin-latest-free.shp/gis_osm_roads_free_1.shp")
str(WI_roads)


road_classes <- WI_roads %>%
  as.data.frame()%>%
  select(fclass) %>%
  unique()


