
library(gt)
library(webshot)
library(tidyverse)
library(sf)
library(ggspatial)

source("Data/shapefiles/watersheds_tribs.R") #subwatershed shapefiles
source("Data/code/road_salt_data.R") #essential roads info (lane miles, geometry, etc.)

world_gray <- paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

#Roads cut to subwatersheds
roads_in_wsDC <- read_rds("Data/shapefiles/DC/roadsinDC.rds")
roads_in_wsPBMS <- read_rds("Data/shapefiles/PBMS/roadsinPBMS.rds")
roads_in_wsPBSF <- read_rds("Data/shapefiles/PBSF/roadsinPBSF.rds")
roads_in_wsSH <- read_rds("Data/shapefiles/SH/roadsinSH.rds")
roads_in_wsSMC <- read_rds("Data/shapefiles/SMC/roadsinSMC.rds")
roads_in_wsSW <- read_rds("Data/shapefiles/SW/roadsinSW.rds")
roads_in_wsWC <- read_rds("Data/shapefiles/WC/roadsinWC.rds")
roads_in_wsWIC <- read_rds("Data/shapefiles/WIC/roadsinWIC.rds")
roads_in_wsYI <- read_rds("Data/shapefiles/YI/roadsinYI.rds")
roads_in_wsYN <- read_rds("Data/shapefiles/YN/roadsinYN.rds")
roads_in_wsYS <- read_rds("Data/shapefiles/YS/roadsinYS.rds")

#load E_roads and W_roads from road_salt_application.R

dorn <- road_salt_in_ws(roads_in_wsDC, wsDC, "DC")
pbms <- road_salt_in_ws(roads_in_wsPBMS, wsPBMS, "PBMS")
pbsf <- road_salt_in_ws(roads_in_wsPBSF, wsPBSF, "PBSF")
#sh <- road_salt_in_ws(roads_in_wsSH, wsSH, "SH")
smc <- road_salt_in_ws(roads_in_wsSMC, wsSMC, "SMC")
sw <- road_salt_in_ws(roads_in_wsSW, wsSW, "SW")
wc <- road_salt_in_ws(roads_in_wsWC, wsWC, "WC")
wic <- road_salt_in_ws(roads_in_wsWIC, wsWIC, "WIC")
yi <- road_salt_in_ws(roads_in_wsYI, wsYI, "YI")
yn <- road_salt_in_ws(roads_in_wsYN, wsYN, "YN")
ys <- road_salt_in_ws(roads_in_wsYS, wsYS, "YS")
