YN_flow_norm <- YN_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(YN_flow_norm %>% filter(normalized_conc < 100), mapping = aes(date, normalized_conc))


DC_flow_norm <- DC_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(DC_flow_norm  %>% filter(normalized_conc < 300), mapping = aes(date, normalized_conc))


SMC_flow_norm <- SMC_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(SMC_flow_norm, mapping = aes(date, normalized_conc))


PBMS_flow_norm <- PBMS_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(PBMS_flow_norm, mapping = aes(date, normalized_conc))



YI_flow_norm <- YI_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(YI_flow_norm, mapping = aes(date, normalized_conc))


PBSF_flow_norm <- PBSF_ts_mass %>%
  mutate(normalized_conc = ifelse(runningmeandis > 0, chloride_predict/runningmeandis, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(PBSF_flow_norm, mapping = aes(date, normalized_conc))



SH_flow_norm <- SH_ts_mass %>%
  mutate(normalized_conc = ifelse(discharge > 0, chloride_predict/discharge, NA)) %>%
  dplyr::select(date, normalized_conc)

ggplot()+
  geom_line(SH_flow_norm, mapping = aes(date, normalized_conc))
