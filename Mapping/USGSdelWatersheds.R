source("Data/shapefiles/watersheds_tribs.R")
library(gt)
library(webshot)

# params <- charsWIC[["parameters"]] %>%
#   select(-value, - ID)
# write_rds(params, "Data/shapefiles/USGS_watershed_parameter_exp.rds")
params <- read_rds("Data/shapefiles/USGS_watershed_parameter_exp.rds")


build_dataset <- function(usgs_del_info, customName) {
  
  usgs_del_info <- usgs_del_info %>%
    select(DRNAREA, CSL10_85, DEVNLCD01, FOREST, LC01CRPHAY, LC01HERB, LC01WATER, WETLAND, PRECIP, SNOFALL) %>%
    mutate(DRNAREA = DRNAREA * 258.999, #square miles to hectare
           PRECIP = PRECIP * 2.54, #inches to centimeters
           SNOFALL = SNOFALL * 2.54,
           CSL10_85 = CSL10_85 * 0.189394, #feet/mile to meters/kilometer
           name = customName)
  
}


usgs_ws_all <- build_dataset(wsYN, "YN") %>%
  bind_rows(build_dataset(wsYI, "YI")) %>%
  bind_rows(build_dataset(wsYS, "YS")) %>%
  bind_rows(build_dataset(wsSMC, "SMC")) %>%
  bind_rows(build_dataset(wsDC, "DC")) %>%
  bind_rows(build_dataset(wsPBMS, "PBMS")) %>%
  bind_rows(build_dataset(wsPBSF, "PBSF")) %>%
  bind_rows(build_dataset(wsWIC, "WIC")) %>%
  bind_rows(build_dataset(wsSW, "SW")) %>%
  bind_rows(build_dataset(wsSH, "SH")) %>%
  bind_rows(build_dataset(wsWC, "WC"))

usgs_ws_all2 <- as.data.frame(usgs_ws_all) %>% select(name,
                                                       DRNAREA,
                                                       LC01WATER,
                                                       DEVNLCD01,
                                                       FOREST,
                                                       LC01HERB,
                                                       LC01CRPHAY,
                                                       WETLAND,
                                                       PRECIP,
                                                       SNOFALL,
                                                       CSL10_85) %>% 
  mutate_if(is.numeric, round, digits = 2)

# Make the tables
gt_tbl <- gt(usgs_ws_all2)
ws_usgs <- gt_tbl %>%
  cols_label(
    name = "Site ID",
    DRNAREA = "Drainage Area (ha)",
    LC01WATER = "Open Water Area (%)",
    DEVNLCD01 = "Developed Area (%)",
    #BarrenPerc = "Barren Area (%)",
    FOREST = "Forest Area (%)",
    LC01HERB = "Herbaceous Area (%)",
    LC01CRPHAY = "Cropland Area (%)",
    WETLAND = "Wetland Area (%)",
    PRECIP = "Average Annual Precipitation (cm)",
    SNOFALL = "Average Annual Snowfall (cm)",
    CSL10_85 = html("Stream Slope (m km<sup>-1<sup>)")
    #TotalRoadDensity = html("Road Density m ha<sup>-1<sup>")
  ) %>%
  tab_header(
    title = "USGS Delineated Watershed Characteristics in the Upper Yahara River Watershed",
  ); ws_usgs

gtsave(data = ws_usgs, "Plots/USGS_Watershed/USGS_watershed_characteristics.png", expand = 10, zoom = 10)



