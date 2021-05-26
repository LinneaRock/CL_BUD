library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
#library(wesanderson)
library(viridisLite)


ME.sf <- read_rds("Data/shapefiles/ME.rds") %>%
  mutate(chloride_alltime = median(labME$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labME %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labME %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))

MO.sf <- read_rds("Data/shapefiles/MO.rds") %>%
  mutate(chloride_alltime = median(labMO$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labMO %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labMO %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))


YN.sf <- read_rds("Data/shapefiles/YN/YN.rds") %>%
  mutate(chloride_alltime = median(labYN$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labYN %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labYN %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE)) %>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River North")

YI.sf <- read_rds("Data/shapefiles/YI/YI.rds")  %>%
  mutate(chloride_alltime = median(labYI$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labYI %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labYI %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River Isthmus")

YS.sf <- read_rds("Data/shapefiles/YS/YS.rds") %>%
  mutate(chloride_alltime = median(labYS$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labYS %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labYS %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Yahara River South")

SMC.sf <- read_rds("Data/shapefiles/SMC/SMC.rds") %>%
  mutate(chloride_alltime = median(lab6MC$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((lab6MC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((lab6MC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

DC.sf <- read_rds("Data/shapefiles/DC/DC.rds") %>%
  mutate(chloride_alltime = median(labDC$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labDC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labDC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

PBMS.sf <- read_rds("Data/shapefiles/PBMS/PBMS.rds")%>%
  mutate(chloride_alltime = median(labPBMS$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labPBMS %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labPBMS %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch Main Stem")

PBSF.sf <- read_rds("Data/shapefiles/PBSF/PBSF.rds") %>%
  mutate(chloride_alltime = median(labPBSF$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labPBSF %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labPBSF %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_) %>%
  mutate(NAME = "Pheasant Branch South Fork")

WIC.sf <- read_rds("Data/shapefiles/WIC/WIC.rds")%>%
  mutate(chloride_alltime = median(labWIC$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labWIC %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labWIC %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))%>%
  rename(NAME = RIVER_SYS_)

SW.sf <- read_rds("Data/shapefiles/SW/SW.rds") %>%
  mutate(chloride_alltime = median(labSW$chloride_mgL, na.rm = TRUE),
         chloride_summer = median((labSW %>% filter(season == "April - October"))$chloride_mgL, na.rm = TRUE),
         chloride_winter = median((labSW %>% filter(season == "November - March"))$chloride_mgL, na.rm = TRUE))

SH.sf <- st_read("Data/shapefiles/SH/ws_SH.shp") %>%
  mutate(name = "Spring Harbor") %>%
  mutate(chloride_alltime = median(SH_ts_mass$chloride_use_mgL, na.rm = TRUE),
         chloride_summer = median((SH_ts_mass %>% filter(season == "April - October"))$chloride_use_mgL, na.rm = TRUE),
         chloride_winter = median((SH_ts_mass %>% filter(season == "November - March"))$chloride_use_mgL, na.rm = TRUE))

tribs.sf <- bind_rows(YN.sf, YI.sf, YS.sf, SMC.sf, DC.sf, PBMS.sf, PBSF.sf, WIC.sf, SW.sf, SH.sf)  %>%
  dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter, geometry) 
lakes.sf <- bind_rows(ME.sf, MO.sf) %>%
  dplyr::select(NAME, chloride_alltime, chloride_summer, chloride_winter, geometry) 

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

#setting crs because I cannot set the other dataset for some reason
lakes.sf1 <- lakes.sf %>%
  st_transform(3071)

all.chloride <- rbind(lakes.sf1, tribs.sf)%>%
  arrange(chloride_alltime) #put in ascending order

#create levels so it appropriately applies colors and puts concentrations in numeric order
all.chloride$label =  factor(all.chloride$chloride_alltime, levels = unique(all.chloride$chloride_alltime))
#pal = wes_palette("Darjeeling1", 11, "continuous")

#for labels
map_labs <- all.chloride[!duplicated(all.chloride$NAME), ]



#map of average chloride over entire study period
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = all.chloride, aes(color = chloride_alltime, fill = chloride_alltime)) + 
  #geom_sf_label(data = map_labs, mapping = aes(label = round(chloride_alltime,2))) +
  #scale_color_manual(values = wes_palette("Darjeeling1", 11, "continuous")) +
  #scale_fill_manual(values = wes_palette("Darjeeling1", 11, "continuous")) +
  scale_color_viridis_c(option = "inferno") +
  scale_fill_viridis_c(option = "inferno") +
  theme_bw() + 
  labs(color = "Median Chloride Concentration (mg/L)", x = "", y = "") +
  theme(legend.position= "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical)# + # North Arrow
  #coord_sf(datum = NA, ylim = c(43.0, 43.29), xlim = c(-89.55, -89.25), expand = FALSE) # limit axes

ggsave('Plots/figsforpres/spatial_distribution/averagechloride_map.png', width = 12, height = 12, units = 'in')

#changing for just winter concentrations
all.chloride <- all.chloride %>%
arrange(chloride_winter)

all.chloride$label =  factor(all.chloride$chloride_winter, levels = unique(all.chloride$chloride_winter))


ggplot(ME.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = all.chloride, aes(color = label, fill = label)) + 
  geom_sf_label(data = map_labs, mapping = aes(label = round(chloride_winter,2))) +
  #scale_color_manual(values = wes_palette("Darjeeling1", 12, "continuous")) +
  #scale_fill_manual(values = wes_palette("Darjeeling1", 12, "continuous")) +
  theme_bw() + 
  labs(color = "Average Chloride Concentration (mg/L)", x = "", y = "") +
  theme(legend.position= "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical)# + # North Arrow
#coord_sf(datum = NA, ylim = c(43.0, 43.29), xlim = c(-89.55, -89.25), expand = FALSE) # limit axes

ggsave('Plots/figsforpres/spatial_distribution/averagewinterchloride_map.png', width = 12, height = 12, units = 'in')


#changing for just summer concentrations
all.chloride <- all.chloride %>%
  arrange(chloride_summer)

all.chloride$label =  factor(all.chloride$chloride_summer, levels = unique(all.chloride$chloride_summer))


ggplot(ME.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = all.chloride, aes(color = label, fill = label)) + 
  geom_sf_label(data = map_labs, mapping = aes(label = round(chloride_summer,2))) +
  #scale_color_manual(values = wes_palette("Darjeeling1", 11, "continuous")) +
  #scale_fill_manual(values = wes_palette("Darjeeling1", 11, "continuous")) +
  theme_bw() + 
  labs(color = "Average Chloride Concentration (mg/L)", x = "", y = "") +
  theme(legend.position= "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                          pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical)# + # North Arrow
coord_sf(datum = NA, ylim = c(43.0, 43.29), xlim = c(-89.55, -89.25), expand = FALSE) # limit axes

ggsave('Plots/figsforpres/spatial_distribution/averagesummerchloride_map.png', width = 12, height = 12, units = 'in')


#median map 
ggplot(gage.bb.sf) + 
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(data = all.chloride, aes(color = chloride_alltime, fill = chloride_alltime)) + 
  geom_sf(data = SH.sf, aes(color = chloride_alltime, fill = chloride_alltime)) +
  #geom_sf_label(data = map_labs, mapping = aes(label = round(chloride_alltime,2))) +
  scale_color_viridis_c(option = "inferno", "Median Chloride Concentration (mg/L)") +
  scale_fill_viridis_c(option = "inferno", guide = FALSE) +
  theme_bw() + # Hilary's default theme
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.55, -89.3), expand = FALSE) + # limit axes
  theme(plot.caption = element_text(size = 10, hjust = 0)) +
  labs(caption = "Figure X. Map of median chloride concentrations across the Upper Yahara River Watershed.")

ggsave("Plots/medianmap.png", width = 6.25, height = 4.25, units = "in")
