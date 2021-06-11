library(tidyverse)

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



options(scipen = 999)

#flow normalized daily average SC and chloride
flow_normalize <- function(ts, id) {
  
  ts1 <- ts %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(discharge_cms = mean(runningmeandis, na.rm = TRUE), 
              SpCond_uScm = mean(sp.cond, na.rm = TRUE),
              chloride_mgL = mean(chloride_predict, na.rm = TRUE)) %>%
    mutate(normalized_SpCond = SpCond_uScm / discharge_cms) %>%
    mutate(normalized_chloride = chloride_mgL / discharge_cms) %>%
    mutate(zscore_cond = (normalized_SpCond - mean(normalized_SpCond, na.rm = TRUE)) / sd(normalized_SpCond, na.rm = TRUE)) %>%
    mutate(zscore_chloride = (normalized_chloride - mean(normalized_chloride, na.rm = TRUE)) / sd(normalized_chloride, na.rm = TRUE)) %>%
    mutate(ID = id)
}

SH_flow_norm <- SH_ts_mass %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(discharge_cms = mean(discharge, na.rm = TRUE), 
            SpCond_uScm = mean(sp.cond, na.rm = TRUE),
            chloride_mgL = mean(chloride_predict, na.rm = TRUE)) %>%
  mutate(normalized_SpCond = SpCond_uScm / discharge_cms) %>%
  mutate(normalized_chloride = chloride_mgL / discharge_cms) %>%
  mutate(ID = 'SH')

All_flow_norm <- flow_normalize(YN_ts_mass, "YN") %>%
  bind_rows(flow_normalize(DC_ts_mass, "DC"),
            flow_normalize(SMC_ts_mass, "SMC"),
            flow_normalize(PBMS_ts_mass, "PBMS"),
           # flow_normalize(PBSF_ts_mass, "PBSF"),
          # SH_flow_norm,
            flow_normalize(YI_ts_mass, "YI"))


ggplot(All_flow_norm) +
  geom_line(aes(date, zscore_cond, group = ID, color = ID))



















