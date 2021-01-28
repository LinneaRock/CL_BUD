source("Data/shapefiles/watersheds_tribs.R")

# params <- charsWIC[["parameters"]] %>%
#   select(-value, - ID)
# write_rds(params, "Data/shapefiles/USGS_watershed_parameter_exp.rds")
params <- read_rds("Data/shapefiles/USGS_watershed_parameter_exp.rds")
