library(tidyverse)
library(LAGOSNE)

lagosne_get(dest_folder = lagos_path())


lagos <- lagosne_load()
names(lagos)

ME_info <- lake_info(name = "Mendota", state = "Wisconsin")
MO_info <- lake_info(name = "Monona", state = "Wisconsin")

# Get Lagos Data
#lagos <- lagosne_load("1.087.1") #Location data 
locus <- lagos$locus %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

geo <- lagos$lakes.geo %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

lulc <- lagos$hu12.lulc %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

res <- coordinatize(lulc$hu12_zoneid)




iws = lagos$iws %>% dplyr::select(lagoslakeid,iws_nhdid,iws_ha)
iws.lulc = lagos$iws.lulc %>% dplyr::select(lagoslakeid,iws_nlcd2011_ha_0:iws_roaddensity_density_mperha)  %>% 
  select(-contains('_ha_'))
iws.100 = lagos$buffer100m.lulc %>% dplyr::select(lagoslakeid,buffer100m_nlcd2011_pct_0:buffer100m_roaddensity_density_mperha) %>% 
  select(-contains('_ha_'))
iws.500 = lagos$buffer500m.lulc %>% dplyr::select(lagoslakeid,buffer500m_nlcd2011_pct_0:buffer500m_roaddensity_density_mperha) %>% 
  select(-contains('_ha_'))
lakes_limno = lagos$lakes_limno %>% dplyr::select(lagoslakeid,maxdepth)
locus = lagos$locus #%>% dplyr::select(lagoslakeid:lake_area_ha,edu_zoneid,state_zoneid)
lakes.geo = lagos$lakes.geo %>% dplyr::select(lagoslakeid,lakeconnection,latewisconsinglaciation_glacial:wlconnections_allwetlands_contributing_area_ha)



