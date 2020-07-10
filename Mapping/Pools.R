
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
  rename(chloride = 'Cl-') %>%
  mutate(Volume = Volume * 3.785411784) %>% #convert volume in gallons to liters 
  mutate(year = year(Date)) 


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
All_pools <- left_join(All_pools, sites_sf, by = "Site") 


#making dataframe to see all receiving waters and pools
receive <- All_pools %>%
  select(Receiving, Site, location) %>%
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
  facet_wrap(~Receiving, ncol = 4, scales = "free_y") +
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


#Finding annual contribution (mass) of fall draining of outdoor pools

#Find average pool volume using the 27 pools that provided pool volume and apply that volume to remaining 
vol <- All_pools %>%
  filter(!is.na(Volume)) 
average_pool_vol <- mean(vol$Volume)

Outdoor_Pools_ave_Vol <- All_pools %>% 
  filter(location == "outdoor") %>%
  mutate(Volume = ifelse(is.na(Volume), average_pool_vol, Volume)) %>% #adding average volume for pools when it is not given
  mutate(full_drain = (chloride * Volume)/1000000) %>% #if 100% of the pool is drained, the mass in Kg of chloride
  mutate(half_drain = full_drain/2) #mass if half the pool is drained

#aggregating all pools by the recieving waterbodies to calculate annual mass
annual_drainage_table <- Outdoor_Pools_ave_Vol %>%
  select(year, Receiving, full_drain, half_drain) %>%
  group_by(year, Receiving) %>%
  mutate(total_full = sum(full_drain)) %>% 
  mutate(total_half = sum(half_drain)) %>%
  ungroup() %>%
  select(year, Receiving, total_full, total_half) %>%
  distinct()

#aggregating all pools by year to calculate annual mass
watershed_annual_drain <- annual_drainage_table %>%
  select(!Receiving) %>%
  group_by(year) %>%
  mutate(Fullvol = sum(total_full),
         Halfvol = sum(total_half)) %>%
  select(year, Fullvol, Halfvol) %>%
  distinct()

#aggregated masses for each recieving water
ggplot(annual_drainage_table) +
  geom_bar(aes(year, total_full, fill = "Full pool volume drained"), stat = "identity") +
  geom_bar(aes(year, total_half, fill = "Half pool volume drained"), stat = "identity") +
  facet_wrap(~Receiving, ncol = 4, scales = "free_y") +
  labs(x = "",
       y = "Chloride mass of pool effluent"~(Kg),
       caption = "Aggregated chloride mass in Kg from swimming pools. Calculated using the actual or average volumes of the pools and annual chloride concentration samples.
After each summer, the pools are drained of at least half of their volume. This figure represents the range of chloride mass by draining half volume or full volume of the pools.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#F24D29", "#1C366B"))


ggsave("Plots/swimming_pools/drain_mass.png", height = 8, width = 12)


#Figure of draining mass Cl for entire watershed
ggplot(watershed_annual_drain) +
  geom_bar(aes(year, Fullvol, fill = "Full pool volume drained"), stat = "identity") +
  geom_bar(aes(year, Halfvol, fill = "Half pool volume drained"), stat = "identity") +
  labs(x = "",
       y = "Chloride mass of pool effluent"~(Kg),
       caption = "Aggregated chloride mass in Kg from swimming pools. Calculated using the actual or average volumes of the pools and annual chloride concentration samples.
After each summer, the pools are drained of at least half of their volume. This figure represents the range of chloride mass to the study area by draining half volume or 
full volume of the pools.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#F24D29", "#1C366B"))


splot("swimming_pools/", "watershed_drain_mass")



#Pool backflushing scenario 75.70824 L/min for 5-10 mins - chloride mass in Kg
backflush <- function(concentration, duration) {
  (75.70824 * concentration * duration)/1000000
}

flushing <- All_pools %>%
  mutate(backflush_5min = backflush(chloride, 5), #5 minute backflushing chloride mass contribution
         backflush_10min = backflush(chloride, 10)) %>% #10 minute backflushing chloride mass contribution
  mutate(annual_weekly_5min = ifelse(location == "indoor", backflush_5min *52, backflush_5min * 14),
         annual_weekly_10min = ifelse(location =="indoor", backflush_10min *52, backflush_10min * 14),
         annual_biweekly_5min = ifelse(location == "indoor", backflush_5min *26, backflush_5min * 7),
         annual_biweekly_10min = ifelse(location =="indoor", backflush_10min *26, backflush_10min * 7))


#aggregating all pools by the recieving waterbodies to calculate annual mass
annual_backflush_table <- flushing %>%
  select(year, Receiving, annual_weekly_5min, annual_weekly_10min, annual_biweekly_5min, annual_biweekly_10min) %>%
  group_by(year, Receiving) %>%
  mutate(total_weekly_5min = sum(annual_weekly_5min)) %>% 
  mutate(total_weekly_10min = sum(annual_weekly_10min)) %>%
  mutate(total_biweekly_5min = sum(annual_biweekly_5min)) %>%
  mutate(total_biweekly_10min = sum(annual_biweekly_10min)) %>%
  ungroup() %>%
  select(year, Receiving, total_weekly_10min, total_biweekly_10min, total_biweekly_5min, total_weekly_5min) %>%
  distinct()

#aggregating all pools by year to calculate annual mass
watershed_backflush <- annual_backflush_table %>%
  select(year, total_weekly_10min, total_biweekly_5min) %>%
  group_by(year) %>%
  mutate(high = sum(total_weekly_10min),
         low = sum(total_biweekly_5min)) %>%
  select(year, high, low) %>%
  distinct()

#aggregated masses for each recieving water
ggplot(annual_backflush_table) +
  geom_bar(aes(year, total_weekly_10min, fill = "Weekly backflushes for 10 min"), stat = "identity") +
  geom_bar(aes(year, total_weekly_5min), stat = "identity") +
  geom_bar(aes(year, total_biweekly_10min, fill = "Biweekly backflushes for 10 min
or Weekly backflushes for 5 min"), stat = "identity") +
  geom_bar(aes(year, total_biweekly_5min, fill = "Biweekly backflushes for 5 min"), stat = "identity") +
  facet_wrap(~Receiving, ncol = 4, scales = "free_y") +
  labs(x = "",
       y = "Chloride mass of pool effluent"~(Kg),
       caption = "Aggregated chloride mass in Kg from swimming pools. Calculated using the actual or average volumes of the pools and annual chloride concentration samples.
This figure represents the range of chloride mass during regular weekly or biweekly backflushing. Indoor pools are assumed maintained year round (52 weeks), outdoor pools 
assumed maintained Memorial Day to Labor Day (average 14 weeks). This figure is likely conservative since backflushing occurs at higher frequency during periods of heavy use.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#E5C4A1", "#1C366B", "#F24D29"))


ggsave("Plots/swimming_pools/backflush_mass.png", height = 8, width = 12)

#Figure of backflushing mass Cl for entire watershed
ggplot(watershed_backflush) +
  geom_bar(aes(year, high, fill = "High backflush estimate"), stat = "identity") +
  geom_bar(aes(year, low, fill = "Low backflush estimate"), stat = "identity") +
  labs(x = "",
       y = "Chloride mass of pool effluent"~(Kg),
       caption = "Aggregated chloride mass in Kg from swimming pools. Calculated using the actual or average volumes of the pools and annual chloride concentration samples.
This figure represents the range of chloride mass during regular weekly or biweekly backflushing. Indoor pools are assumed maintained year round (52 weeks), outdoor pools 
assumed maintained Memorial Day to Labor Day (average 14 weeks). This figure is likely conservative since backflushing occurs at higher frequency during periods of heavy use.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#F24D29", "#1C366B"))


splot("swimming_pools/", "watershed_backflush_mass")

#Total annual chloride load due to swimming pools
Chloride_Mass_Load_SP <- left_join(annual_backflush_table, annual_drainage_table, by = c("Receiving", "year")) %>%
  rowwise() %>%
  mutate(Low_end = sum(total_biweekly_5min, total_half, na.rm = TRUE),
         High_end = sum(total_weekly_10min, total_full, na.rm = TRUE))


#aggregated masses for each recieving water
ggplot(Chloride_Mass_Load_SP) +
  geom_bar(aes(year, High_end, fill = "High end estimate"), stat = "identity") +
  geom_bar(aes(year, Low_end, fill = "Low end estimate"), stat = "identity") +
  facet_wrap(~Receiving, ncol = 4, scales = "free_y") +
  labs(x = "",
       y = "Chloride mass of pool effluent"~(Kg),
       caption = "Aggregated chloride mass in Kg from swimming pools. This figure represents the range of annual chloride mass contributions from swimming pools.
  This figure is likely conservative since backflushing occurs at higher frequency during periods of heavy use.")+
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("#F24D29", "#1C366B"))

ggsave("Plots/swimming_pools/mass_range.png", height = 8, width = 12)
























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
