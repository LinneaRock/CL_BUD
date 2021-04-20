library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(viridisLite)


ME.sf <- read_rds("Data/shapefiles/ME.rds") %>%
  mutate(chloride_alltime = mean(labME$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labME %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labME %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))

MO.sf <- read_rds("Data/shapefiles/MO.rds") %>%
  mutate(chloride_alltime = mean(labMO$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labMO %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labMO %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))


YN.sf <- read_rds("Data/shapefiles/YN/YN.rds") %>%
  mutate(chloride_alltime = mean(labYN$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labYN %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labYN %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River North")

YI.sf <- read_rds("Data/shapefiles/YI/YI.rds")  %>%
  mutate(chloride_alltime = mean(labYI$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labYI %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labYI %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River Isthmus")

YS.sf <- read_rds("Data/shapefiles/YS/YS.rds") %>%
  mutate(chloride_alltime = mean(labYS$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labYS %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labYS %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River South")

SMC.sf <- read_rds("Data/shapefiles/SMC/SMC.rds") %>%
  mutate(chloride_alltime = mean(lab6MC$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((lab6MC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((lab6MC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

DC.sf <- read_rds("Data/shapefiles/DC/DC.rds") %>%
  mutate(chloride_alltime = mean(labDC$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labDC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labDC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

PBMS.sf <- read_rds("Data/shapefiles/PBMS/PBMS.rds")%>%
  mutate(chloride_alltime = mean(labPBMS$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labPBMS %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labPBMS %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch Main Stem")

PBSF.sf <- read_rds("Data/shapefiles/PBSF/PBSF.rds") %>%
  mutate(chloride_alltime = mean(labPBSF$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labPBSF %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labPBSF %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch South Fork")

WIC.sf <- read_rds("Data/shapefiles/WIC/WIC.rds")%>%
  mutate(chloride_alltime = mean(labWIC$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labWIC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labWIC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

SW.sf <- read_rds("Data/shapefiles/SW/SW.rds") %>%
  mutate(chloride_alltime = mean(labSW$chloride_mgL, na.rm = TRUE),
         chloride_summer = mean((labSW %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = mean((labSW %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))
#%>%
 # rename(NAME = RIVER_SYS_)

tribs.sf <- bind_rows(YN.sf, YI.sf, YS.sf, SMC.sf, DC.sf, PBMS.sf, PBSF.sf, WIC.sf, SW.sf)  %>%
  dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter, geometry) 
lakes.sf <- bind_rows(ME.sf, MO.sf) %>%
  dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter, geometry) 

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# all.chloride <- as.data.frame(lakes.sf) %>%
#   dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter, geometry) 
# 
# tribs.sf2 <- as.data.frame(tribs.sf)%>%
#   dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter) #%>%
#   #distinct()
# 
# tribs.geom = data.frame(NAME = c('Yahara River North', 'Sixmile Creek', 'Dorn Creek', 'Pheasant Branch Main Stem', 'Pheasant Branch South Fork', 'Yahara River Isthmus', 'Starkweather Creek', 'Yahara River South', 'Wingra Creek'),lat = c(43.15083333, 43.14683333, 43.145262, 43.10333333, 43.075910, 43.08944444, 43.104292, 43.04718, 43.048883), lon = c(-89.40194444, -89.43694444, -89.475643, -89.51166667, -89.519880, -89.36083333, -89.334455, -89.33605, -89.394132))
# tribs.geom <- tribs.geom %>%
#   left_join(tribs, by = "NAME")

#need to fix this!!
tribs.sf = st_as_sf(tribs.geom, 
                      crs = 4326)

all.chloride <- lakes.sf %>%
  rbind(tribs.sf)


all.chloride <- st_sf(all.chloride) %>%
  arrange(chloride_alltime)

all.chloride$label =  factor(all.chloride$chloride_alltime, levels = unique(all.chloride$chloride_alltime))
pal = wes_palette("Darjeeling1", 11, "continuous")

ggplot(ME.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = all.chloride, aes(color = label)) + 
 # geom_sf(data = lakes.sf, aes(color = label)) +
 #geom_sf(data = ME.sf, aes(fill = chloride_alltime)) +
  #geom_sf(data = MO.sf, aes(fill = chloride_alltime)) +
  geom_sf_label(data = all.chloride, mapping = aes(label = round(chloride_alltime,2))) +
  scale_color_manual(values = wes_palette("Darjeeling1", 11, "continuous")) +
  #scale_color_viridis_c(option = "inferno") +
  #scale_fill_viridis_c(option = "inferno") +
  theme_bw() + 
  labs(color = "Average Chloride Concentration (mg/L)", x = "", y = "") +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.29), xlim = c(-89.55, -89.25), expand = FALSE) # limit axes

ggsave('Plots/averagechloride_map.png', width = 12, height = 12, units = 'in')
