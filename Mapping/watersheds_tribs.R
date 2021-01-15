library(sf)
#library(streamstats)


# wsYN <- delineateWatershed(xlocation = -89.40194444, ylocation = 43.15083333, crs = 4326, 
#                           includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsYN)
# charsYN <- computeChars(workspaceID = wsYN$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsYN, layer = "ws_YN", dir = "Data/shapefiles/YN", what = "boundary")
wsYN <- read_sf("Data/shapefiles/YN/ws_YN.shp")
# 
# 
# wsSMC <- delineateWatershed(xlocation = -89.43694444, ylocation = 43.14683333, crs = 4326, 
#                            includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsSMC)
# charsSMC <- computeChars(workspaceID = wsSMC$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsSMC, layer = "ws_SMC", dir = "Data/shapefiles/SMC", what = "boundary")
wsSMC <- read_sf("Data/shapefiles/SMC/ws_SMC.shp")
#



# wsDC <- delineateWatershed(xlocation = -89.44222222, ylocation = 43.14027778, crs = 4326, 
#                             includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsDC)
# charsDC <- computeChars(workspaceID = wsDC$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsDC, layer = "ws_DC", dir = "Data/shapefiles/DC", what = "boundary")
wsDC <- read_sf("Data/shapefiles/DC/ws_DC.shp")
# 
# 
# wsPBMS <- delineateWatershed(xlocation = -89.51166667, ylocation = 43.10333333, crs = 4326, 
#                            includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsPBMS)
# charsPBMS <- computeChars(workspaceID = wsPBMS$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsPBMS, layer = "ws_PBMS", dir = "Data/shapefiles/PBMS", what = "boundary")
wsPBMS <- read_sf("Data/shapefiles/PBMS/ws_PBMS.shp")
# 
# 
# wsPBSF <- delineateWatershed(xlocation = -89.52138889, ylocation = 43.09861111, crs = 4326, 
#                              includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsPBSF)
# charsPBSF <- computeChars(workspaceID = wsPBSF$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsPBSF, layer = "ws_PBSF", dir = "Data/shapefiles/PBSF", what = "boundary")
wsPBSF <- read_sf("Data/shapefiles/PBSF/ws_PBSF.shp")
# 
# 
# wsYI <- delineateWatershed(xlocation = -89.36083333, ylocation = 43.08944444, crs = 4326, 
#                            includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsYI)
# charsYI <- computeChars(workspaceID = wsYI$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsYI, layer = "ws_YI", dir = "Data/shapefiles/YI", what = "boundary")
wsYI <- read_sf("Data/shapefiles/YI/ws_YI.shp")
# 
# 
# 
# wsSW <- delineateWatershed(xlocation = -89.33318, ylocation = 43.09259, crs = 4326, 
#                            includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsSW)
# charsSW <- computeChars(workspaceID = wsSW$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsSW, layer = "ws_SW", dir = "Data/shapefiles/SW", what = "boundary")
wsSW <- read_sf("Data/shapefiles/SW/ws_SW.shp")


# wsWIC <- delineateWatershed(xlocation = -89.379444, ylocation = 43.05416667, crs = 4326, 
#                            includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsWIC)
# charsWIC <- computeChars(workspaceID = wsWIC$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsWIC, layer = "ws_WIC", dir = "Data/shapefiles/WIC", what = "boundary")
wsWIC <- read_sf("Data/shapefiles/WIC/ws_WIC.shp")


# wsYS <- delineateWatershed(xlocation = -89.33605, ylocation = 43.04718, crs = 4326, 
#                             includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsYS)
# charsYS <- computeChars(workspaceID = wsYS$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsYS, layer = "ws_YS", dir = "Data/shapefiles/YS", what = "boundary")
wsYS <- read_sf("Data/shapefiles/YS/ws_YS.shp")




# wsSH <- delineateWatershed(xlocation = -89.470833, ylocation = 43.079166, crs = 4326,
#                             includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsSH)
# charsSH <- computeChars(workspaceID = wsSH$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsSH, layer = "ws_SH", dir = "Data/shapefiles/SH", what = "boundary")
wsSH <- read_sf("Data/shapefiles/SH/ws_SH.shp")




# wsWC <- delineateWatershed(xlocation = -89.42150, ylocation = 43.07679, crs = 4326,
#                             includeparameters = "true", includeflowtypes = "true")
# leafletWatershed(wsWC)
# charsWC <- computeChars(workspaceID = wsWC$workspaceID, rcode = "WI")
# writeShapefile(watershed = wsWC, layer = "ws_WC", dir = "Data/shapefiles/WC", what = "boundary")
wsWC <- read_sf("Data/shapefiles/WC/ws_WC.shp")
