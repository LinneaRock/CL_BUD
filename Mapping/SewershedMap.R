library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
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



#point source water discharges
discharge <- read_xlsx("Data/Historical_External/stormsewer/discharges.xlsx")
discharge1 <- discharge %>%
  mutate(Cl_mgL = as.numeric(Cl_mgL)) %>%
  drop_na(Cl_mgL) %>%
  drop_na(Date)

#illicit discharge detection and elimination monitoring 
idde <- read_xlsx("Data/Historical_External/stormsewer/IDDE.xlsx") %>%
  as.data.frame()

ggplot(idde, aes(Cond, chloride)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
idde_fit <- lm(chloride~Cond, idde)
summary(idde_fit)

#watershed shapefile
wsYS <- read_sf("Data/shapefiles/YS/ws_YS.shp")


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


ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(ws_outfallbasins, mapping = aes(fill = watershed)) +
 # geom_sf(ws, mapping = aes(), fill = NA) + 
  theme_bw() + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + 
  labs(caption = "Figure X. Sewersheds within the UYRW. Note data outside of Dane County were not available (data from City of Madison).") + L_theme() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("Plots/sewershed/ws_sewersheds.png", width = 20, height = 15, units = "cm")



pipe_idde <- Pipes %>%
  full_join(idde, by = c("Dn_pipe_break" = "Site"))

test <- pipe_idde %>%
  filter(Cond > 0)
          