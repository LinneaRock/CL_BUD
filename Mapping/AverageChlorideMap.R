library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(viridisLite)


ME.sf <- read_rds("Data/shapefiles/ME.rds") %>%
  mutate(chloride = mean(labME$chloride_mgL))

MO.sf <- read_rds("Data/shapefiles/MO.rds") %>%
  mutate(chloride = mean(labMO$chloride_mgL))

YN.sf <- read_rds("Data/shapefiles/YN.rds") %>%
  mutate(chloride = mean(labYN$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River North")

YI.sf <- read_rds("Data/shapefiles/YI.rds")  %>%
  mutate(chloride = mean(labYI$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River Isthmus")

YS.sf <- read_rds("Data/shapefiles/YS.rds") %>%
  mutate(chloride = mean(labYS$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River South")

SMC.sf <- read_rds("Data/shapefiles/SMC.rds") %>%
  mutate(chloride = mean(lab6MC$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

DC.sf <- read_rds("Data/shapefiles/DC.rds") %>%
  mutate(chloride = mean(labDC$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

PBMS.sf <- read_rds("Data/shapefiles/PBMS.rds") %>%
  mutate(chloride = mean(labPBMS$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch Main Stem")

PBSF.sf <- read_rds("Data/shapefiles/PBSF.rds") %>%
  mutate(chloride = mean(labPBSF$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch South Fork")

WIC.sf <- read_rds("Data/shapefiles/WIC.rds") %>%
  mutate(chloride = mean(labWIC$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_)

SW.sf <- read_rds("Data/shapefiles/SW.rds") %>%
  mutate(chloride = mean(labSW$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_)

tribs.sf <- bind_rows(YN.sf, YI.sf, YS.sf, SMC.sf, DC.sf, PBMS.sf, PBSF.sf, WIC.sf, SW.sf) 
lakes.sf <- bind_rows(ME.sf, MO.sf)

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

all.chloride <- as.data.frame(lakes.sf) %>%
  dplyr::select(NAME, chloride, geometry) 

tribs <- as.data.frame(tribs.sf)%>%
  dplyr::select(NAME, chloride, geometry)

all.chloride <- all.chloride %>%
  rbind(tribs)

all.chloride <- all.chloride[!duplicated(all.chloride$NAME), ]

all.chloride <- st_sf(all.chloride)

ggplot(ME.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = tribs.sf, aes(color = chloride)) + 
  geom_sf(data = lakes.sf, aes(color = chloride)) +
  geom_sf_label(data = all.chloride, mapping = aes(label = round(chloride,2))) +
  scale_color_viridis_c() +
  #scale_fill_viridis_c() +
  theme_bw() + 
  labs(color = "Average Chloride Concentration (mg/L)") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.29), xlim = c(-89.55, -89.25), expand = FALSE) # limit axes
