library(tidyverse)
library(LAGOSNE)
library(gt)
library(webshot)
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


###LAGOS table information#########
lake_info_withgeom <- read_rds("Data/shapefiles/HUC12_ME.rds") %>%
  bind_rows(read_rds("Data/shapefiles/HUC12_MO.rds")) %>%
  dplyr::select(Name, HUC12, ZoneID)

HUC12_INFO <- lagos$hu12.lulc %>%
  dplyr::select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
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
  left_join(lake_info_withgeom, c("hu12_zoneid" = "ZoneID")) %>%
  left_join(HUC12, c("HUC12" = "hu12")) %>%
  mutate(site = c("YN", "YN", "YN", "YN", "SMC, DC", "SMC, DC", "PBMS, PBSF", "YI, Storm Sewers", "SW" , "YS, WIC, Storm Sewers"))

#HUC12 Watershed Table
HUC12_ws_chars <- LandUseAll(HUC12_INFO) 
HUC12_ws_chars_sum <- LandUseSum(HUC12_ws_chars)
#land use as percentages
HUC12_ws_chars_perc <- landuse_percent(HUC12_ws_chars)

#make table to match information from USGS subwatersheds
HUC12_table <- left_join( HUC12_ws_chars_sum, HUC12_ws_chars_perc, by = "HUC12") %>%
  mutate(undeveloped = BarrenPerc + ForestPerc + HerbPerc + WetlandPerc) %>%
  select(HUC12, WatershedArea, OpenWaterPerc, DevelopmentPerc, AgPerc, undeveloped, TotalRoadDensity)

names <- data.frame(
  site = c("", "", "",  "YN", "", "SMC, DC", "PBMS, PBSF", "ME, YI, SH", "SW" , "MO, YS, WIC"),
  HUC12 = HUC12_table$HUC12
) 

HUC12_table <- left_join(names, HUC12_table, by = "HUC12")

gt_tbl <- gt(HUC12_table)
ws_table <- gt_tbl %>%
  cols_label(
    HUC12 = "HUC12",
    site = "Site ID",
    WatershedArea = "Drainage Area ha",
    OpenWaterPerc = "Open Water Area %",
    DevelopmentPerc = "Developed Area %",
    AgPerc = "Cropland Area %",
    undeveloped = "Undeveloped Area %",
    TotalRoadDensity = html("Road Density m ha<sup>-1<sup>")
  ) %>%
  tab_header(
    title = "Watershed characteristics of HUC12 subwatersheds",
  ) %>%
  tab_source_note(
    "Table 2. Note: undeveloped area includes barren, wetland, forest, and herbacious land. Site ID indicates study sites that are located within that HUC12 boundary."
  ) %>%
  tab_footnote(footnote = "No study site within boundary; part of YN",
               locations = cells_body(columns = 1, rows = 1:3)) %>%
  tab_footnote(footnote = "No study site within boundary; part of SMC and DC",
               locations = cells_body(columns = 1, rows = 5)) %>%
  opt_footnote_marks(marks = "extended"); ws_table

gtsave(data = ws_table, "Plots/HUC12_Watershed/watershedchars_simple.png", expand = 10, zoom = 10)








###older tables

# Make the tables
gt_tbl <- gt(HUC12_ws_chars_perc)
ws_perc_table <- gt_tbl %>%
  cols_label(
    HUC12 = "HUC12 ID",
    site = "Site ID",
    OpenWaterPerc = "Open Water Area (%)",
    DevelopmentPerc = "Developed Area (%)",
    BarrenPerc = "Barren Area (%)",
    ForestPerc = "Forest Area (%)",
    HerbPerc = "Herbaceous Area (%)",
    AgPerc = "Cropland Area (%)",
    WetlandPerc = "Wetland Area (%)",
    TotalRoadDensity = html("Road Density m ha<sup>-1<sup>")
  ) %>%
  tab_header(
    title = "Land Use Percentages of Subwatersheds (HUC12) in the Upper Yahara River Watershed",
  ); ws_perc_table

gtsave(data = ws_perc_table, "Plots/HUC12_Watershed/HUC12_watershed_characteristics%.png", expand = 10, zoom = 10)



gt_tbl <- gt(HUC12_ws_chars_sum)
ws_perc_table <- gt_tbl %>%
  cols_label(
    HUC12 = "HUC12 ID",
    WatershedArea = "Watershed Area (ha)",
    OpenWater = "Open Water Area (ha)",
    Development = "Developed Area (ha)",
    Barren = "Barren Area (ha)",
    Forest = "Forest Area (ha)",
    Herb = "Herbaceous Area (ha)",
    Ag = "Cropland Area (ha)",
    Wetland = "Wetland Area (ha)",
    TotalRoadLength = "Road Length (m)"
  ) %>%
  tab_header(
    title = "Land Use Areas of Subwatersheds (HUC12) in the Upper Yahara River Watershed",
  ); ws_perc_table

gtsave(data = ws_perc_table, "Plots/HUC12_Watershed/HUC12_watershed_characteristics_area.png", expand = 10, zoom = 10)

















# #Getting land use data for Mendota watershed by HUC 12
# ME <- read_rds("Data/shapefiles/HUC12_ME.rds") %>%
#   dplyr::select(Name, HUC12, ZoneID)
# 
# ME.HUC12.lulc <- lagos$hu12.lulc %>%
#   dplyr::select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
#   filter(hu12_zoneid == "HU12_11991" | 
#            hu12_zoneid == "HU12_11992" |
#            hu12_zoneid == "HU12_11993" | 
#            hu12_zoneid == "HU12_11994" |
#            hu12_zoneid == "HU12_11995" |
#            hu12_zoneid == "HU12_11996" |
#            hu12_zoneid == "HU12_11997" |
#            hu12_zoneid == "HU12_11998" ) %>%
#   left_join(ME, c("hu12_zoneid" = "ZoneID")) %>%
#   left_join(HUC12, c("HUC12" = "hu12")) %>%
#   dplyr::select(-geometry)
# 
# 
# #same for Monona
# MO <- read_rds("Data/shapefiles/HUC12_MO.rds") %>%
#   dplyr::select(Name, HUC12, ZoneID)
# 
# MO.HUC12.lulc <- lagos$hu12.lulc %>%
#   dplyr::select(hu12_zoneid, hu12_nlcd2011_ha_0:hu12_roaddensity_density_mperha) %>%
#   filter(hu12_zoneid == "HU12_12000" | 
#            hu12_zoneid == "HU12_11999" ) %>%
#   left_join(MO, c("hu12_zoneid" = "ZoneID")) %>%
#   left_join(HUC12, c("HUC12" = "hu12")) %>%
#   dplyr::select(-geometry)
# 
# #Land Use Percentages for all applicable watersheds that I want to write about
# Mendota.Watershed <- LandUse(ME.HUC12.lulc)
# Simple.Mendota.Watershed <- SimpleLandUse(Mendota.Watershed)
# 
# 
# 
# YaharaHeadwaters.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11991" | 
#                                                                  hu12_zoneid == "HU12_11992" |
#                                                                  hu12_zoneid == "HU12_11993" | 
#                                                                  hu12_zoneid == "HU12_11994" ))
# Simple.Headwaters.Watershed <- SimpleLandUse(YaharaHeadwaters.Watershed)
# 
# 
# 
# DC_6MC.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11995" |
#                                                        hu12_zoneid == "HU12_11996"))
# Simple.DC_6MC.Watershed <- SimpleLandUse(DC_6MC.Watershed)
# 
# 
# 
# PB.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11997"))
# Simple.PB.Watershed <- SimpleLandUse(PB.Watershed)
# 
# 
# 
# StormSewer_isthmus_Lake.Watershed <- LandUse(ME.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11998"))
# Simple.StormSewer.ME <- SimpleLandUse(StormSewer_Lake)
# 
# 
# 
# Monona.Watershed <- LandUse(MO.HUC12.lulc)
# Simple.Monona.Watershed <- SimpleLandUse(Monona.Watershed)
# 
# 
# 
# YaharaSouth_Lake.Watershed <- LandUse(MO.HUC12.lulc %>% filter(hu12_zoneid == "HU12_12000"))
# Simple.YaharaS.MO <- SimpleLandUse(Yahara_Lake)
# 
# 
# 
# Starkweather <- LandUse(MO.HUC12.lulc %>% filter(hu12_zoneid == "HU12_11999"))
# Simple.Starkweather.Watershed <- SimpleLandUse(Starkweather)

