#run data in flow_events.R first
source("Baseflow Separation/sq_analysis_plots_saltingseason_fct.R")

##event-averaged stormflow cq elationships #####
YN_each_eventSS <- each_event_cqSS(YN_events_bf)
YI_each_eventSS <- each_event_cqSS(YI_events_bf)
SMC_each_eventSS <- each_event_cqSS(SMC_events_bf)
DC_each_eventSS <- each_event_cqSS(DC_events_bf)
PBMS_each_eventSS <- each_event_cqSS(PBMS_events_bf)
SH_each_eventSS <- each_event_cqSS(SH_events_bf)
PBSF_each_eventSS <- each_event_cqSS(PBSF_events_bf)

Averaged_seasonalSS <- averaged_seasonal_cqSS(YN_each_eventSS, "YN") %>%
  bind_rows(averaged_seasonal_cqSS(YI_each_eventSS, "YI"),
            averaged_seasonal_cqSS(SMC_each_eventSS, "SMC"),
            averaged_seasonal_cqSS(DC_each_eventSS, "DC"),
            averaged_seasonal_cqSS(PBMS_each_eventSS, "PBMS"),
            averaged_seasonal_cqSS(SH_each_eventSS, "SH"),
            averaged_seasonal_cqSS(PBSF_each_eventSS, "PBSF"))


a<- ggplot(Averaged_seasonalSS) + 
  geom_point(aes(slope, season, color = trib, fill = trib), size = 2, shape = 21) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", end = 0.9) +
  scale_fill_viridis_d(option = "inferno", end = 0.9, alpha = 0.5) +
  labs(x = "SC-Discharge Slope",
       y = "Season") +
  theme(legend.title = element_blank()) +
  labs(title = "Stormflow - 
event averaged") +
  theme(legend.position = "bottom")  +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

##averaged baseflow cq relationships #####
Averaged_seasonal_bfSS <- seasonal_baseflowSS(YN_events_bf, "YN") %>%
  bind_rows(seasonal_baseflowSS(YI_events_bf, "YI"),
            seasonal_baseflowSS(SMC_events_bf, "SMC"),
            seasonal_baseflowSS(DC_events_bf, "DC"),
            seasonal_baseflowSS(PBMS_events_bf, "PBMS"),
            seasonal_baseflowSS(SH_events_bf, "SH"),
            seasonal_baseflowSS(PBSF_events_bf, "PBSF"))


b <- ggplot(Averaged_seasonal_bfSS %>% mutate(season = ifelse(season == "November - March", "Nov-Mar", season),
                       season = ifelse(season == "April - October", "Apr-Oct", season))) + 
  geom_point(mapping = aes(slope, season, color = trib, fill = trib), size = 2, shape = 24) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", end = 0.9) +
  scale_fill_viridis_d(option = "inferno", end = 0.9, alpha = 0.5) +
  labs(x = "SC-Discharge Slope",
       y = "") +
  theme(legend.position = "none") +
  labs(title= "Baseflow") 



#Bulk Stormflow CQ slopes, intercepts etc. #####
Bulk_StormflowSS <- YN_bulkSS <- bulk_stormflowSS(YN_events_bf, "YN") %>%
  bind_rows(YI_bulkSS <- bulk_stormflowSS(YI_events_bf, "YI")) %>% #calculation done manually and separately because YI does not exhibit stormflow in each month and R code errors.
  bind_rows(DC_bulkSS <- bulk_stormflowSS(DC_events_bf, "DC")) %>%
  bind_rows(SMC_bulkSS <- bulk_stormflowSS(SMC_events_bf, "SMC")) %>%
  bind_rows(PBMS_bulkSS <- bulk_stormflowSS(PBMS_events_bf, "PBMS")) %>%
  bind_rows(SH_bulkSS <- bulk_stormflowSS(SH_events_bf, "SH")) %>%
  bind_rows(PBSF_bulkSS <- bulk_stormflowSS(PBSF_events_bf, "PBSF"))  


c <- ggplot(Bulk_StormflowSS) +
  geom_point(aes(slope, season, color = trib, fill = trib), size = 2, shape = 23) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", end = 0.9) +
  scale_fill_viridis_d(option = "inferno", end = 0.9, alpha = 0.5) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", begin = 0.25, end = 0.9) +
  labs(x = "SC-Discharge Slope",
       y = "Season") +
  theme(legend.position = "none") +
  labs(title = "Stormflow - 
bulk averaged") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

c



#seasonal and annual cq slopes for baseflow, stormflow averaged by event, stormflow bulk averaged ######
(b | a | c) +
  plot_annotation(tag_levels = 'a',tag_suffix = ')',
                  caption = "Figure 64. Specific conductivity (SC) - discharge (Q) slopes for a) baseflow (triangles), b) event-
averaged stormflow (circles), and c) bulk-averaged stormflow (diamonds). Slopes are categorized 
by salting season (November-March), non-salting season (April-October), and annual. Colors 
indicate tributary. Grey boxes indicate chemostatic behavior.",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0),
                                legend.position = "bottom")) +plot_layout(guides = "collect")
ggsave("Plots/QC_plots/Eckhardt_Method/SaltingSeason/slopes.png", height = 4.25, width = 6.25, units = "in")




#figure of all event sloeps, entire cq slope
all_eventsSS <- bind_rows(YN_each_eventSS %>% mutate(trib = "YN"),
                        YI_each_eventSS %>% mutate(trib = "YI"), 
                        SMC_each_eventSS %>% mutate(trib = "SMC"), 
                        DC_each_eventSS %>% mutate(trib = "DC"), 
                        PBMS_each_eventSS %>% mutate(trib = "PBMS"),
                        SH_each_eventSS %>% mutate(trib = "SH"), 
                        PBSF_each_eventSS %>% mutate(trib = "PBSF"))


ggplot() +
  labs(x = "SC-Q Slope", y = "") + 
  geom_point(all_eventsSS, mapping = aes(slope, trib, color = season, fill = season), size = 2.5, shape = 21, alpha = 0.5) +
   scale_color_manual(labels = c("April - October", "November - March"),
                      values = c("#F24D29", "#1C366B")) +
  scale_fill_manual(labels = c("April - October", "November - March"),
                     values = c("#F24D29", "#1C366B")) +
  theme(legend.title = element_blank()) +
  L_theme() +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  geom_point(all_cq_stats, mapping = aes(Slope, River), shape = "|", size = 6) +
  geom_point(Averaged_seasonal_bfSS %>% filter(season == "annual"), mapping = aes(slope, trib), shape = "|", size = 6, color = "#1DACE8") +
  labs(caption = "Figure 63. Specific conductivity (SC) - Discharge (Q) slopes for stormflow event 
represented by circles. Colors of circles indicate season in which stormfow 
event occured, with salting season being November-March and non-salting season 
April-October. Black lines are the SC-Q slopes for all SC and flow data over the
entire study period. Blue lines are the SC-Q slopes for SC and flow data during 
baseflow only. Grey box indicates chemostatic behavior.")

ggsave("Plots/QC_plots/Eckhardt_Method/SaltingSeason/event_slopes.png", height = 4.25, width = 6.25, units = "in")




library(gt)
library(webshot)
BFI_tbl <- data.frame(
  Trib = c(
    "YN",
    "SMC",
    "DC",
    "PBMS",
    "PBSF",
    "SH",
    "YI"
  ),
  BFI = c(calc_bfi(YN_d)$BFI * 100,
          calc_bfi(SMC_d)$BFI * 100,
          calc_bfi(DC_d)$BFI * 100,
          calc_bfi(PBMS_d)$BFI * 100,
          calc_bfi(PBSF_d)$BFI * 100,
          calc_bfi(SH_d)$BFI * 100,
          calc_bfi(YI_d)$BFI * 100
  ),
  stormflow_pos = c(count_mobilization_events(all_eventsSS, "YN"),
                    count_mobilization_events(all_eventsSS, "SMC"),
                    count_mobilization_events(all_eventsSS, "DC"),
                    count_mobilization_events(all_eventsSS, "PBMS"),
                    count_mobilization_events(all_eventsSS, "PBSF"),
                    count_mobilization_events(all_eventsSS, "SH"),
                    count_mobilization_events(all_eventsSS, "YI")
  ),
  stormflow_stat = c(count_chemostatic_events(all_eventsSS, "YN"),
                     count_chemostatic_events(all_eventsSS, "SMC"),
                     count_chemostatic_events(all_eventsSS, "DC"),
                     count_chemostatic_events(all_eventsSS, "PBMS"),
                     count_chemostatic_events(all_eventsSS, "PBSF"),
                     count_chemostatic_events(all_eventsSS, "SH"),
                     count_chemostatic_events(all_eventsSS, "YI")
    
  ),
  stormflow_neg = c(count_dilution_events(all_eventsSS, "YN"),
                    count_dilution_events(all_eventsSS, "SMC"),
                    count_dilution_events(all_eventsSS, "DC"),
                    count_dilution_events(all_eventsSS, "PBMS"),
                    count_dilution_events(all_eventsSS, "PBSF"),
                    count_dilution_events(all_eventsSS, "SH"),
                    count_dilution_events(all_eventsSS, "YI")
    
  )
    )

gt_tbl <- gt(BFI_tbl)
simpleregtable <- gt_tbl %>%
  cols_label(
    Trib = "Tributary",
    BFI = "Eckhardt 
Baseflow Index (%)",
    stormflow_neg = "No. Dilution Events",
    stormflow_stat = "No. Chemostatic Events",
    stormflow_pos = "No. Mobilization Events"
  ) %>%
  tab_header(
    title = "Specific Conductivity - Discharge Relationships",
    subtitle = "Log(SC) - Log(Discharge) for all SC-Q data") %>%
  tab_source_note(source_note = "Table 7."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/QC_Plots/Eckhardt_Method/flow_stats.png", expand = 10, zoom = 10)



