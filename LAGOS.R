library(tidyverse)
library(LAGOSNE)
source("Functions/LandUse.R")


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
  dplyr::select(hu12, hu12_ha_in_usa)

geo <- lagos$lakes.geo %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

conn <- lagos$hu12.conn %>%
  filter(hu12_zoneid == "HU12_11991" | 
           hu12_zoneid == "HU12_11992" |
           hu12_zoneid == "HU12_11993" | 
           hu12_zoneid == "HU12_11994" |
           hu12_zoneid == "HU12_11995" |
           hu12_zoneid == "HU12_11996" |
           hu12_zoneid == "HU12_11997" |
           hu12_zoneid == "HU12_11998" |
           hu12_zoneid == "HU12_12000" | 
           hu12_zoneid == "HU12_11999" )

chag <- lagos$hu12.chag %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

#Getting land use data for Mendota watershed by HUC 12
ME <- read_rds("Data/shapefiles/HUC12_ME.rds") %>%
  dplyr::select(Name, HUC12, ZoneID)

ME.HUC12.lulc <- lagos$hu12.lulc %>%
  dplyr::select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
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
  dplyr::select(-geometry)


#same for Monona
MO <- read_rds("Data/shapefiles/HUC12_MO.rds") %>%
  dplyr::select(Name, HUC12, ZoneID)

MO.HUC12.lulc <- lagos$hu12.lulc %>%
  dplyr::select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
  filter(hu12_zoneid == "HU12_12000" | 
           hu12_zoneid == "HU12_11999" ) %>%
  left_join(MO, c("hu12_zoneid" = "ZoneID")) %>%
  left_join(HUC12, c("HUC12" = "hu12")) %>%
  dplyr::select(-geometry)

#Land Use Percentages for all applicable watersheds that I want to write about
Mendota.Watershed <- LandUse(ME.HUC12.lulc)
Simple.Mendota.Watershed <- SimpleLandUse(Mendota.Watershed)



YaharaHeadwaters.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11991" | 
                                                                 hu12_zoneid == "HU12_11992" |
                                                                 hu12_zoneid == "HU12_11993" | 
                                                                 hu12_zoneid == "HU12_11994" ))
Simple.Headwaters.Watershed <- SimpleLandUse(YaharaHeadwaters.Watershed)



DC_6MC.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11995" |
                                                       hu12_zoneid == "HU12_11996"))
Simple.DC_6MC.Watershed <- SimpleLandUse(DC_6MC.Watershed)



PB.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11997"))
Simple.PB.Watershed <- SimpleLandUse(PB.Watershed)



StormSewer_isthmus_Lake.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11998"))
Simple.StormSewer.ME <- SimpleLandUse(StormSewer_Lake)



Monona.Watershed <- LandUse(MO.HUC12.lulc)
Simple.Monona.Watershed <- SimpleLandUse(Monona.Watershed)



YaharaSouth_Lake.Watershed <- LandUse(MO.HUC12.lulc %>% filter(hu12_zoneid == "HU12_12000"))
Simple.YaharaS.MO <- SimpleLandUse(Yahara_Lake)



Starkweather <- LandUse(MO.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11999"))
Simple.Starkweather.Watershed <- SimpleLandUse(Starkweather) %>%
  





###LAGOS table information#########

#Lake Mendota HUC12 Watershed Table

# Make the table
HUC12_INFO <- data.frame(
  #River = c("Yahara River North", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Yahara River Isthmus", "Wingra Creek", "Starkweather Creek", "Yahara River South"),
  HUC12 = ME.HUC12.lulc$HUC12,
  
)


gt_tbl <- gt(River_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    River = "River Name",
    Slope = "Slope",
    Intercept = "Intercept",
    Adjusted_R2 = html("R<sup>2<sup>"),
    P_value = "P-Value"
  ) %>%
  tab_header(
    title = "Chloride - Specific Conductivity Linear Regression Statistics",
  ); simpleregtable

