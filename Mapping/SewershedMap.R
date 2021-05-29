library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(readxl)
library(lubridate)
library(patchwork)
library(gt)
library(webshot)
source("Functions/L_theme.R")

#looking at datasets in geodatabase from City of Engineering
hydro <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "hydro")
#Accessstructures <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Accessstructures") 
ImperviousParcelInt <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "ImperviousParcelInt")
#landuseDC2015 <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "landuseDC2015") #Huge dataset, not needed becasue LAGOS!
outfallbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "outfallbasins")
outfallbasins <- outfallbasins %>%
  rename(geometry = Shape)

Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement") 
Pipes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pipes")
SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")
#watersheds <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "watersheds")
WingraSubbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "WingraSubbasins")

#watershed shapefile
wsYS <- read_sf("Data/shapefiles/YS/ws_YS.shp")
wsSH <- read_sf("Data/shapefiles/SH/ws_SH.shp")
wsWC <- read_sf("Data/shapefiles/WC/ws_WC.shp")


## Esri basemap URLs ####
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ws_outfallbasins <- outfallbasins %>%
  filter(watershed == "LAKE MENDOTA" |
           watershed == "LAKE MONONA" |
           watershed == "LAKE WINGRA" |
           watershed == "PHEASANT BRANCH" |
           watershed == "SIX MILE CREEK" |
           watershed == "SPRING CREEK" |
           watershed == "STARKWEATHER CREEK" |
           watershed == "TOKEN CREEK" |
           watershed == "UPPER  YAHARA" |
           watershed == "UPPER YAHARA" |
           watershed == "YAHARA") %>%
  mutate(watershed = ifelse(watershed == "UPPER  YAHARA", "UPPER YAHARA", watershed),
         watershed = ifelse(watershed == "SPRING CREEK", "DORN CREEK", watershed))


st_crs(outfallbasins)
st_crs(wsYS)

#map of all outfall basins in the UYRW and which water they discharge to
ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(ws_outfallbasins, mapping = aes(fill = watershed)) +
  theme_bw() + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
  labs(caption = "Figure X. Outfall basins of sewersheds within the Upper Yahara River Watershed. Legend and 
colors indicate which waterbody the outfall basins discharges into. Note: data outside of Dane 
County were not available (data from City of Madison).") + L_theme() +
  scale_fill_viridis_d(option = "inferno", name = "Subwatershed") + #colors may not be distinguishable enough
  #scale_fill_manual(values = zis1, name = "Subwatershed") + #colors are not distinguishable enough :( ##color palettes created in WQPMaps.R
  theme(plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

ggsave("Plots/sewershed/ws_sewersheds.png", width = 6.25, height = 4.25, units = "in")


#map of spring harbor sewershed
SH_map <- ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(ws_outfallbasins, mapping = aes(), color = "#1C366B", fill = NA) +
  geom_sf(wsSH, mapping = aes(), fill = "#F24D29") +
  theme_bw() + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
#   labs(caption = "Figure X. Map showing the full sewershed for the Spring Harbor (SH) storm sewer monitored continuously by 
# USGS. SH sewershed is overlain on all relevant outfall basins in the UYRW.") + L_theme() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("Plots/sewershed/SH_within_sewersheds.png", width = 20, height = 15, units = "cm")



#map of Willow Creek sewershed
WC_map <- ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(ws_outfallbasins, mapping = aes(), color = "#1C366B", fill = NA) +
  geom_sf(wsWC, mapping = aes(), fill = "#F24D29") +
  theme_bw() + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
#   labs(caption = "Figure X. Map showing the full sewershed for Willow Creek (WC) overlain on all relevant 
# outfall basins in the UYRW.") + L_theme() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("Plots/sewershed/WC_within_sewersheds.png", width = 20, height = 15, units = "cm")

#################################################################################################
#illicit discharge detection and elimination monitoring 
idde <- read_xlsx("Data/Historical_External/stormsewer/IDDE.xlsx") %>%
  as.data.frame()
discharges <- read_xlsx("Data/Historical_External/stormsewer/discharges.xlsx")
summary(lm(as.numeric(Cl_mgL)~as.numeric(Cond_umhos), discharges))
library(tmaptools)
discharges_geo <- as.data.frame(discharges)
location = discharges$Site
discharges_geo <- discharges_geo %>%
  geocode_OSM(location)

#matching IDDE data to the spatial data
pipe_idde <- full_join(mutate(Pipes, i =1),
                       mutate(idde, i = 1)) %>%
  dplyr::select(-i) %>%
  filter(str_detect(Asset_ID, Site))

dummy <- idde %>%
  dplyr::select(Cond, Site) #wanted to keep Site info but the last bit of code took a bit to run so just adding it back in to pipe_idde1

pipe_idde <- pipe_idde %>%
  filter(Cond > 0) %>%
  dplyr::select(Asset_ID, Dn_pipe_break, Up_pipe_break, Date, chloride, Cond, mslink, Year_retired, Lead_pipe, Shape)

pipe_idde1 <- pipe_idde %>%
  left_join(dummy, by = "Cond") %>%
  mutate(Year = year(Date))

idde_basins <- st_intersection(pipe_idde1, ws_outfallbasins) #pipe lines
basins_idde <- st_intersection(ws_outfallbasins, pipe_idde1)

basins_idde <- as.data.frame(basins_idde) %>%
  dplyr::select(outfall_ba, watershed, Date, chloride, Cond, Site, Year) %>%
  unique()

basins_idde <- basins_idde %>%
  left_join(ws_outfallbasins, by = "outfall_ba")
  
basins_idde1 <- st_as_sf(basins_idde)

#how many observations? (there are more with the spatial data becuase it maps out the pipes) 93
count_idde <- idde_basins %>%
  as.data.frame() %>%
  dplyr::select(Date, Site, chloride, Cond) %>%
  unique()

#how many sites? 71
count_sites <- count_idde %>%
  dplyr::select(Site) %>% unique()

#checking out linear regression
ggplot(count_idde, aes(Cond, chloride)) + geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1)) +
  L_theme()
splot("cl_cond_linear_regression/", "idde")
idde_fit <- lm(chloride~Cond, pipe_idde)
summary(idde_fit) #R2 = 0.99, p < 2.2e-16, intercept = -2.530e+02, slope = 3.350e-01

#get chloride estimates for each basin using regression on previous line
basins_idde2 <- basins_idde1 #%>%
 # mutate(chloride_mgL = ifelse(is.na(chloride), (Cond * 3.083e-01) - 1.687e+02, chloride))

#map of monitored storm sewer pipe outfall basins
pipes_map <- ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  #geom_sf(ws_outfallbasins, mapping = aes(), color = "#1C366B", fill = NA) +
  geom_sf(basins_idde1, mapping = aes(fill = chloride), color = "#F24D29") +
  #geom_sf(idde_basins, mapping = aes(), color = "black") +
  theme_bw() + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
#   labs(caption = "Figure X. Map showing the basins of storm sewer pipes with data from 2011-2013 overlain on all relevant 
# outfall basins in the UYRW.") + L_theme() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("Plots/sewershed/idde_basins.png", width = 20, height = 15, units = "cm")



(SH_map | WC_map | pipes_map) +
  plot_annotation(tag_levels = 'a',tag_suffix = ')',
                  caption = 'Sewersheds of a) Spring Harbor , b) Willow Creek, and c) storm sewer pipes with data.',
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0)))
ggsave("Plots/sewershed/sewersheds_patched.png", width = 20, height = 15, units = "cm")

#######################################################################################################
#point source water discharges 1988-2019
#I don't have lat longs for these and I think it would requre too much manual work at this point. 
#If there is an easy way to get the geocoded informaion, I really want to use this data.
discharge <- read_xlsx("Data/Historical_External/stormsewer/discharges.xlsx")
discharge1 <- discharge %>%
  mutate(Cl_mgL = as.numeric(Cl_mgL)) %>%
  mutate(Cond_umhos = as.numeric(Cond_umhos)) %>%
  drop_na(Cl_mgL) %>%
  drop_na(Date)

ggplot(discharge1, aes(Cond_umhos, Cl_mgL)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
discharge_fit <- lm(Cl_mgL~Cond_umhos, discharge1)
summary(discharge_fit) #R2 = 99.4, p < 2.2e-16, intercept = -1.687e+02, slope = 3.083e-01
