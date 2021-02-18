library(tidyverse)
library(sf)
library(ggspatial)
source("Data/shapefiles/watersheds_tribs.R")
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

#load usgs_ws_all from USGSdelWatersheds.R in Mapping folder!


WI_roads <- st_read("C:/Users/linne/Downloads/WIroads/wisconsin-latest-free.shp/gis_osm_roads_free_1.shp")
str(WI_roads) #classes sf and data.frame


road_classes <- WI_roads %>%
  as.data.frame()%>%
  select(fclass) %>%
  unique()


# roads_in_wsDC <- lapply(st_intersection(WI_roads, wsDC), function(x) length(x) > 0)
# roads_in_wsDC2 <- WI_roads[unlist(roads_in_wsDC),]

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
