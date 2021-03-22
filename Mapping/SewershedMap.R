library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)

#looking at datasets in geodatabase from City of Engineering
hydro <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "hydro")
#Accessstructures <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Accessstructures") 
ImperviousParcelInt <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "ImperviousParcelInt")
#landuseDC2015 <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "landuseDC2015") #Huge dataset, not needed becasue LAGOS!
outfallbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "outfallbasins")
Pavement <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pavement") 
Pipes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "Pipes")
SaltRoutes <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "SaltRoutes")
#watersheds <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "watersheds")
WingraSubbasins <- st_read("C:/Users/linne/OneDrive/Documents/SALT/SALT/UWSaltLayers.gdb", layer = "WingraSubbasins")

discharge <- read_xlsx("Data/Historical_External/stormsewer/discharges.xlsx")
discharge1 <- discharge %>%
  mutate(Cl_mgL = as.numeric(Cl_mgL)) %>%
  drop_na(Cl_mgL) %>%
  drop_na(Date)

idde <- read_xlsx("Data/Historical_External/stormsewer/IDDE.xlsx")
idde_pipe <- idde %>%
  left_join(Pipes, by = c("Site" = "Dn_pipe_break"))

ggplot(idde, aes(Cond, chloride)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
idde_fit <- lm(chloride~Cond, idde)
summary(idde_fit)

## Esri basemap URLs ####
world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


ggplot() +
  annotation_map_tile(type = world_gray, zoom = 12) +
  geom_sf(idde_pipe, mapping = aes(geometry = Shape)) +
  theme_bw() 


          