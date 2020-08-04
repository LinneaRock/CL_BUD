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

geo <- lagos$lakes.geo %>%
  filter(lagoslakeid == 5371 |
           lagoslakeid == 4559)

lulc <- lagos$hu12.lulc %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

conn <- lagos$hu12.conn %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")

chag <- lagos$hu12.chag %>%
  filter(hu12_zoneid == "HU12_12000" |
           hu12_zoneid == "HU12_11998")


