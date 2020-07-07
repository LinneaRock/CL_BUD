
library(tidyverse)
library(sf)
library(tidygeocoder)
library(readxl)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(lubridate)


#read in pool data
pools <- read_xlsx("Data/Historical_External/pool.xlsx")

#geocode from addresses
pool_locations <- pools %>%
  geocode(address, lat = latitude, long = longitude) 

#create a simple dataframe with the geocoded info 
geocoded <- pool_locations %>%
  filter(!is.na(latitude)) %>%
  select(Site, latitude, longitude) %>%
  distinct()

#Figure out which addresses did not geocode and manually enter lat, long information... ugh
#create a simple dataframe to match the geocoded dataframe
location_na <- pool_locations %>%
  filter(is.na(latitude)) %>%
  select(Site, latitude, longitude) %>%
  distinct() %>%
  mutate(latitude = ifelse(Site == "5607 Summer Shine Dr", 43.121650, latitude),
         longitude = ifelse(Site == "5607 Summer Shine Dr",  -89.283655, longitude),
         latitude = ifelse(Site == "1080 N Pleasant View Rd", 43.080517, latitude),
         longitude = ifelse(Site == "1080 N Pleasant View Rd", -89.537356, longitude),
         latitude = ifelse(Site == "1602 W Beltline Hwy", 43.035780, latitude),
         longitude = ifelse(Site == "1602 W Beltline Hwy", -89.409167, longitude),
         latitude = ifelse(Site == "1624 Fordem Ave", 43.093629, latitude),
         longitude = ifelse(Site == "1624 Fordem Ave", -89.366627, longitude),
         latitude = ifelse(Site == "3177 E Washington Ave", 43.108462, latitude),
         longitude = ifelse(Site == "3177 E Washington Ave", -89.335858, longitude),
         latitude = ifelse(Site == "401 N Thompson Dr2", 43.103363, latitude),
         longitude = ifelse(Site == "401 N Thompson Dr2", -89.297451, longitude),
         latitude = ifelse(Site == "4602 Eastpark Blvd", 43.155020, latitude),
         longitude = ifelse(Site == "4602 Eastpark Blvd", -89.299994, longitude),
         latitude = ifelse(Site == "5020 Pendleton Dr", 43.154203, latitude),
         longitude = ifelse(Site == "5020 Pendleton Dr", -89.284118, longitude),
         latitude = ifelse(Site == "5711 Slate Dr", 43.153464, latitude),
         longitude = ifelse(Site == "5711 Slate Dr", -89.280674, longitude),
         latitude = ifelse(Site == "639 Pleasant View Rd", 43.073630, latitude),
         longitude = ifelse(Site == "639 Pleasant View Rd", -89.536106, longitude),
         latitude = ifelse(Site == "8310 Globe Dr", 43.071656, latitude),
         longitude = ifelse(Site == "8310 Globe Dr", -89.526051, longitude),
         latitude = ifelse(Site == "9328 Silverstone Ln", 43.030638, latitude),
         longitude = ifelse(Site == "9328 Silverstone Ln", -89.547916, longitude),
         latitude = ifelse(Site == "9603 Paragon St", 43.064658, latitude),
         longitude = ifelse(Site == "9603 Paragon St", -89.553901, longitude))

#create a list of sites 
sites <- rbind(geocoded, location_na) 

#bind the lat, long info to the data 
All_pools <- left_join(pools, sites, by = "Site")

#create shapefile object out of sites
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"),
                     crs = 4326, agr = "constant")

str(sites_sf)         

#add shapefiles to All_pools
All_pools <- left_join(All_pools, sites_sf, by = "Site") %>%
  mutate(year = year(Date))

pools_2019 <- All_pools %>%
  filter(year == 2019)

#getting base map sf
world <- ne_countries(scale = "medium", returnclass = "sf")

## Esri basemap URLs ####
#esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


#mapping out pool loactions in Madison
ggplot(world) +
  geom_sf() +
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(sites_sf, mapping = aes()) +
  #theme_bw() +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.58, -89.25), expand = FALSE)


#making dataframe for receiving waters
receive <- All_pools %>%
  select(Receiving) %>%
  distinct()

rivers <- st_read("C:/Users/linne/Downloads/Rivers_and_Streams-shp/Rivers_and_Streams.shp")
lakes <- st_read("C:/Users/linne/Downloads/Lakes_and_Ponds-shp/Lakes_and_Ponds.shp")

Mendota <- lakes %>%
  filter(NAME == "Lake Mendota" |
         NAME == "Lake Monona" |
         NAME == "Starkweather Creek")

ggplot(world) +
  geom_sf() +
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(Mendota, mapping = aes()) +
  #theme_bw() +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.58, -89.25), expand = FALSE)
