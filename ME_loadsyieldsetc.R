Mendota_loads_by_ws <- readRDS("Data/sources_loads_by_ws.rds") %>% 
  drop_na() %>% 
  filter(watershed != "PBSF") %>%
  mutate(trib = ifelse(watershed == "YI", "out", "in"))

