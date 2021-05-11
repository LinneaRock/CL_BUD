

usgs_ws_info_roads <- read_rds("Data/usgs_ws_info_roads.rds")
#combining wetlands, forest, and herbaceous land into 'undeveloped' category
usgs_ws_info_roads_aggregated <- usgs_ws_info_roads %>%
  mutate(undeveloped = FOREST + LC01HERB + WETLAND) %>%
  dplyr::select(name, DRNAREA, LC01WATER, DEVNLCD01, LC01CRPHAY, undeveloped, road_density_mha) %>%
  filter(name != "WC") %>% #I don't know if Willow Creek will be meaningfully used in my study besides a mention to WRM study
  arrange(name)

#other info
Mendota_loads_by_ws <- readRDS("Data/sources_loads_by_ws.rds") %>% 
  drop_na() %>% 
  #filter(watershed != "PBSF") %>%
  mutate(trib = ifelse(watershed == "YI", "out", "in")) %>%
  dplyr::select(c(-val2020, -val2021, -ratio_cl_drainage2020, -ratio_cl_drainage2021, -salt_ratio))

#chloride info

chloride <- data.frame(
  watershed = c("YI", "YN", "SMC", "DC", "PBMS", "SH", "PBSF", "WIC", "YS", "SW"),
  ave_conc = c(mean(YI_ts_mass$chloride_mgL, na.rm = TRUE), mean(YN_ts_mass$chloride_mgL, na.rm = TRUE), mean(SMC_ts_mass$chloride_mgL, na.rm = TRUE), mean(DC_ts_mass$chloride_mgL, na.rm = TRUE), mean(PBMS_ts_mass$chloride_mgL, na.rm = TRUE), mean(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), mean(PBSF_ts_mass$chloride_mgL, na.rm = TRUE), mean(WIC_ts_mass$chloride_mgL, na.rm = TRUE), mean(YS_ts_mass$chloride_mgL, na.rm = TRUE), mean(SW_ts_mass$chloride_mgL, na.rm = TRUE)),
  max_conc = c(max(YI_ts_mass$chloride_mgL, na.rm = TRUE), max(YN_ts_mass$chloride_mgL, na.rm = TRUE), max(SMC_ts_mass$chloride_mgL, na.rm = TRUE), max(DC_ts_mass$chloride_mgL, na.rm = TRUE), max(PBMS_ts_mass$chloride_mgL, na.rm = TRUE), max(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), max(PBSF_ts_mass$chloride_mgL, na.rm = TRUE), max(WIC_ts_mass$chloride_mgL, na.rm = TRUE), max(YS_ts_mass$chloride_mgL, na.rm = TRUE), max(SW_ts_mass$chloride_mgL, na.rm = TRUE)),
  median_conc = c(median(YI_ts_mass$chloride_mgL, na.rm = TRUE), median(YN_ts_mass$chloride_mgL, na.rm = TRUE), median(SMC_ts_mass$chloride_mgL, na.rm = TRUE), median(DC_ts_mass$chloride_mgL, na.rm = TRUE), median(PBMS_ts_mass$chloride_mgL, na.rm = TRUE), median(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), median(PBSF_ts_mass$chloride_mgL, na.rm = TRUE), median(WIC_ts_mass$chloride_mgL, na.rm = TRUE), median(YS_ts_mass$chloride_mgL, na.rm = TRUE), median(SW_ts_mass$chloride_mgL, na.rm = TRUE))
  )


all_info <- usgs_ws_info_roads %>%
  left_join(Mendota_loads_by_ws) %>%
  left_join(chloride, by = c("name" = "watershed"))

all_info <- all_info %>%
  dplyr::select(name, DRNAREA, DEVNLCD01, road_density_mha,16:19, 21:25,29:35) %>%
  filter(name != "WC")


summary(lm(median_conc~DEVNLCD01, all_info)) #r = .59, p = 0.006
summary(lm(median_conc~road_density_mha, all_info)) #r = .61, p = 0.004

ggplot(all_info, aes(road_density_mha, median_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5)
  

