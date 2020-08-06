library(tidyverse)
library(LAGOSNE)


#Download LAGOS dataset
#lagosne_get(dest_folder = lagos_path())


lagos <- lagosne_load()
names(lagos)

#get LAGOS info for ME and MO
ME_info <- lake_info(name = "Mendota", state = "Wisconsin")
MO_info <- lake_info(name = "Monona", state = "Wisconsin")

#Pull data
locus <- lagos$locus %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

HUC12 <- lagos$hu12 %>%
  filter(hu12_zoneid == "HU12_11991" | 
           hu12_zoneid == "HU12_11992" |
           hu12_zoneid == "HU12_11993" | 
           hu12_zoneid == "HU12_11994" |
           hu12_zoneid == "HU12_11995" |
           hu12_zoneid == "HU12_11996" |
           hu12_zoneid == "HU12_11997" |
           hu12_zoneid == "HU12_11998" |
           hu12_zoneid == "HU12_12000" | 
           hu12_zoneid == "HU12_11999" ) %>%
  select(hu12, hu12_ha_in_usa)

geo <- lagos$lakes.geo %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

conn <- lagos$hu12.conn %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

chag <- lagos$hu12.chag %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

#Getting land use data for Mendota watershed by HUC 12
ME <- read_rds("Data/shapefiles/HUC12_ME.rds") %>%
  select(Name, HUC12, ZoneID)

ME.HUC12.lulc <- lagos$hu12.lulc %>%
  select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
  filter(hu12_zoneid == "HU12_11991" | 
           hu12_zoneid == "HU12_11992" |
           hu12_zoneid == "HU12_11993" | 
           hu12_zoneid == "HU12_11994" |
           hu12_zoneid == "HU12_11995" |
           hu12_zoneid == "HU12_11996" |
           hu12_zoneid == "HU12_11997" |
           hu12_zoneid == "HU12_11998" ) %>%
  left_join(ME, c("hu12_zoneid" = "ZoneID")) %>%
  left_join(HUC12, c("HUC12" = "hu12")) %>%
  select(-geometry)

write.csv(ME.HUC12.lulc, "Mendota.csv")

#same for Monona
MO <- read_rds("Data/shapefiles/HUC12_MO.rds") %>%
  select(Name, HUC12, ZoneID)

MO.HUC12.lulc <- lagos$hu12.lulc %>%
  select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
  filter(hu12_zoneid == "HU12_12000" | 
           hu12_zoneid == "HU12_11999" ) %>%
  left_join(MO, c("hu12_zoneid" = "ZoneID")) %>%
  left_join(HUC12, c("HUC12" = "hu12")) %>%
  select(-geometry)

#Lake Mendota Watershed Land Use Calculations
#calculate area of all 6 HUC12s:
Mendota.Watershed.Area <- ME.HUC12.lulc %>%
  summarise(sum(hu12_ha_in_usa))

OpenWater <- ME.HUC12.lulc %>%
  mutate(OpenWater = sum(hu12_nlcd2011_ha_11)) %>%
  select(OpenWater) %>%
  distinct()
  
  

OpenWaterPerc <- OpenWater/Mendota.Watershed.Area * 100



