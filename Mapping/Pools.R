
library(tidyverse)
library(sf)
library(tidygeocoder)
library(readxl)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(lubridate)
source("Functions/splot.R")



#read in pool data
pools <- read_xlsx("Data/Historical_External/pool.xlsx") %>%
  rename(chloride = 'Cl-')

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



#making dataframe to see all receiving waters
receive <- All_pools %>%
  select(Receiving) %>%
  distinct()



#getting base map sf
world <- ne_countries(scale = "medium", returnclass = "sf")

## Esri basemap URLs ####
#esri_land <-    paste0('https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/${z}/${y}/${x}.jpeg')
world_gray <-   paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')


#mapping out pool loactions in Madison
ggplot(world) +
  geom_sf() +
  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
  geom_sf(All_pools$geometry, mapping = aes(color = All_pools$Receiving)) +
  #theme_bw() +
  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.5,'in'), width = unit(0.5,'in'),
                         style = north_arrow_nautical) + # North Arrow
  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.58, -89.25), expand = FALSE) +
  scale_color_discrete("Recieving Waters")

splot("swimming_pools/", "pool_locations")


#boxplots of pool discharges/year, faceted to receiving waters
ggplot(All_pools) +
  geom_boxplot(aes(year, chloride, group = year))+
  facet_wrap(~Receiving, ncol = 5, scales = "free_y") +
  labs(x = "",
       y = "Chloride concentration of pool effluent"~(mg~L^-1),
       caption = "Aggregated chloride concentration data from yearly samples of swimming pools in the Upper Yahara River watershed.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0))
 
ggsave("Plots/swimming_pools/cl_data.png", height = 8, width = 12)

















#ggplot(world) +
#  geom_sf() +
#  annotation_map_tile(type = world_gray, zoom = 12) + # Esri Basemap (zoom sets level of detail, higher = higherRes)
#  geom_sf(e.stark, mapping = aes(), color = "magenta") +
#  geom_sf(low.stark, mapping = aes(), color = "purple") +
#  geom_sf(ME.sf, mapping = aes(), color = "blue") +
#  geom_sf(MO.sf, mapping = aes(), color = "red") +
#  geom_sf(YR.sf, mapping = aes(), color = "black") +
#  geom_sf(Wingra.sf, mapping = aes(), color = "dark orange") +
#  annotation_scale(location = "br", width_hint = 0.5,height = unit(0.05,'in')) + # Scale bar
#  annotation_north_arrow(location = "bl", which_north = "true", 
#                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
#                         height = unit(0.5,'in'), width = unit(0.5,'in'),
#                         style = north_arrow_nautical) + # North Arrow
#  coord_sf(datum = NA, ylim = c(43.0, 43.2), xlim = c(-89.58, -89.25), expand = FALSE)
