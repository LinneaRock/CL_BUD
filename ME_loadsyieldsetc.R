Mendota_loads_by_ws <- readRDS("Data/sources_loads_by_ws.rds") %>% 
  drop_na() %>% 
  #filter(watershed != "PBSF") %>%
  mutate(trib = ifelse(watershed == "YI", "out", "in")) %>%
  dplyr::select(c(-val2020, -val2021, -ratio_cl_drainage2020, -ratio_cl_drainage2021, -salt_ratio))

YI_discharge <- YI_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = runningmeandis * timestep)

  
YN_discharge <- YN_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = runningmeandis * timestep)

SMC_discharge <- SMC_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = runningmeandis * timestep)

DC_discharge <- DC_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = runningmeandis * timestep)

PBMS_discharge <- PBMS_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = runningmeandis * timestep)

SH_discharge_cond <- SH_discharge_cond %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = discharge * timestep)

PBSF_discharge <- PBSF_discharge %>%
  add_season() %>%
  mutate(timestep = (date - lag(date)) * 60) %>% #timestep in seconds
  mutate(water_load = discharge * timestep)

concentrations <- data.frame(
  watershed = c("YI", "YN", "SMC", "DC", "PBMS", "SH", "PBSF"),
  ave_conc = c(mean(YI_ts_mass$chloride_mgL, na.rm = TRUE), mean(YN_ts_mass$chloride_mgL, na.rm = TRUE), mean(SMC_ts_mass$chloride_mgL, na.rm = TRUE), mean(DC_ts_mass$chloride_mgL, na.rm = TRUE), mean(PBMS_ts_mass$chloride_mgL, na.rm = TRUE), mean(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), mean(PBSF_ts_mass$chloride_mgL, na.rm = TRUE)),
  max_conc = c(max(YI_ts_mass$chloride_mgL, na.rm = TRUE), max(YN_ts_mass$chloride_mgL, na.rm = TRUE), max(SMC_ts_mass$chloride_mgL, na.rm = TRUE), max(DC_ts_mass$chloride_mgL, na.rm = TRUE), max(PBMS_ts_mass$chloride_mgL, na.rm = TRUE), max(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), max(PBSF_ts_mass$chloride_mgL, na.rm = TRUE)),
  ave_discharge = c(mean(YI_discharge$runningmeandis), mean(YN_discharge$runningmeandis), mean(SMC_discharge$runningmeandis), mean(DC_discharge$runningmeandis, na.rm = TRUE), mean(PBMS_discharge$runningmeandis, na.rm = TRUE), mean(SH_discharge_cond$discharge, na.rm = TRUE), mean(PBSF_discharge$runningmeandis, na.rm = TRUE)),
  ave_dis_salting = c(mean((YI_discharge%>% filter(season == "November - March"))$runningmeandis), mean((YN_discharge%>% filter(season == "November - March"))$runningmeandis), mean((SMC_discharge%>% filter(season == "November - March"))$runningmeandis), mean((DC_discharge%>% filter(season == "November - March"))$runningmeandis, na.rm = TRUE), mean((PBMS_discharge%>% filter(season == "November - March"))$runningmeandis, na.rm = TRUE), mean((SH_discharge_cond %>% filter(season == "November - March"))$discharge, na.rm = TRUE), mean((PBSF_discharge%>% filter(season == "November - March"))$runningmeandis, na.rm = TRUE)),
  ave_dis_non = c(mean((YI_discharge%>% filter(season == "April - October"))$runningmeandis), mean((YN_discharge%>% filter(season == "April - October"))$runningmeandis), mean((SMC_discharge%>% filter(season == "April - October"))$runningmeandis), mean((DC_discharge%>% filter(season == "April - October"))$runningmeandis, na.rm = TRUE), mean((PBMS_discharge%>% filter(season == "April - October"))$runningmeandis, na.rm = TRUE), mean((SH_discharge_cond %>% filter(season == "April - October"))$discharge, na.rm = TRUE), mean((PBSF_discharge%>% filter(season == "April - October"))$runningmeandis, na.rm = TRUE))
  )

Mendota_loads_by_ws <- Mendota_loads_by_ws %>%
  left_join(concentrations, by = "watershed")


