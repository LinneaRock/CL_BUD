library(tidyverse)

#read in discharge data, filter NAs, only include discharge >=0 (steps necessary to run baseflow separation and to identify storm events)
YI_d<- read_rds("Data/discharge/YI_discharge.rds") %>%
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

YN_d <- read_rds("Data/discharge/YN_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

SMC_d <- read_rds("Data/discharge/SMC_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

DC_d<- read_rds("Data/discharge/DC_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

PBMS_d <- read_rds("Data/discharge/PBMS_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

PBSF_d <- read_rds("Data/discharge/PBSF_discharge.rds")%>%
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0)

SH_d <- d.sc.SH %>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) %>%
  rename(runningmeandis = discharge)


#run baseflow calculations, get baseflow index, & plot 
source("Baseflow Separation/baseflow_separation_fct.R")
YN_d <- get_eckhardt_bf("05427850", YN_d)
calc_bfi(YN_d) #bfi = 48%
plot_bf_qt(YN_d, "YN")

YI_d <- get_eckhardt_bf("05428500", YI_d)
calc_bfi(YI_d) #bfi = 82%
plot_bf_qt(YI_d, "YI")

DC_d <- get_eckhardt_bf("05427930", DC_d)
calc_bfi(DC_d) #bfi = 76%
plot_bf_qt(DC_d, "DC")

SMC_d <- get_eckhardt_bf("05427910", SMC_d)
calc_bfi(SMC_d) #bfi = 83%
plot_bf_qt(SMC_d, "SMC")

PBMS_d <- get_eckhardt_bf("05427948", PBMS_d)
calc_bfi(PBMS_d) #bfi = 50%
plot_bf_qt(PBMS_d, "PBMS")

PBSF_d <- get_eckhardt_bf("054279465", PBSF_d)
calc_bfi(PBSF_d) #bfi = 17%
plot_bf_qt(PBSF_d, "PBSF")

SH_d <- get_eckhardt_bf("05427965", SH_d)
calc_bfi(SH_d) #bfi = 55%
plot_bf_qt(SH_d, "SH")



#identify storm flow events based on the following criteria:
  #The upward slope must be > 1x10^-6 cms
  #The difference between the peak discharge and the Eckhardt baseflow must be at least half the total average discharge
  #The downward slope must be < 0 cms and the event is over when the slope changes to >=0 cms
source("Baseflow Separation/find_peaks_eventduration.R")
 timestamp <- 'date'
 plot_var <- 'runningmeandis'
 sb_pk_thresh <- 0.000001
 sf_pk_thresh <- 0

YN_events_bf <- find.peaks(YN_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, YN_cond_data)
SMC_events_bf <- find.peaks(SMC_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, SMC_cond_data)
DC_events_bf <- find.peaks(DC_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, DC_cond_data)
PBMS_events_bf <- find.peaks(PBMS_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, PBMS_cond_data)
SH_events_bf <- find.peaks(SH_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, d.sc.SH %>% dplyr::select(date, sp.cond) %>% rename(runningmean = sp.cond))
YI_events_bf <- find.peaks(YI_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, YI_cond_data)
PBSF_events_bf <- find.peaks(PBSF_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, PBSF_cond_data)

#plots
source("Baseflow Separation/cq_analysis_plots_fct.R")
#timeseries
grid(YN_events_bf, YN_cond_data, "56", "YN")
grid(YI_events_bf, YI_cond_data, "62", "YI")
grid(DC_events_bf, DC_cond_data, "58", "DC")
grid(SMC_events_bf, SMC_cond_data, "57", "SMC")
grid(PBMS_events_bf, PBMS_cond_data, "59", "PBMS")
grid(SH_events_bf, d.sc.SH %>% dplyr::select(date, sp.cond) %>% rename(runningmean = sp.cond), "61", "SH")
grid(PBSF_events_bf, PBSF_cond_data, "60", "PBSF")

#SC vs Q for all data (log-log axes)
a <- all_cq(YN_events_bf, "YN")
b <- all_cq(YI_events_bf, "YI")
c <- all_cq(SMC_events_bf, "SMC")
d <- all_cq(DC_events_bf, "DC")
e <- all_cq(PBMS_events_bf, "PBMS")
f <- all_cq(SH_events_bf, "SH")
g <- all_cq(PBSF_events_bf, "PBSF")

options(scipen = 999)

(a | b | c | d) / (e | f | g) +
  plot_annotation(
                  caption = "Figure X. Concentration-discharge plots on log-log axes for each study site. Data includes all
specific conductivity (SC) and discharge over the entire study period. Corresponding statistics
are in Table X.",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0)))

ggsave("Plots/QC_plots/Eckhardt_Method/all_cq/figure.png", height = 4.25, width = 6.25, units = "in")

#table to accompany the above graphic
fitYN <- all_cq_stats(YN_events_bf)
fitYI <- all_cq_stats(YI_events_bf)
fitSMC <- all_cq_stats(SMC_events_bf)  
fitDC <- all_cq_stats(DC_events_bf)
fitPBMS <- all_cq_stats(PBMS_events_bf)
fitSH <- all_cq_stats(SH_events_bf)
fitPBSF <- all_cq_stats(PBSF_events_bf)
#####
library(gt)
library(webshot)
all_cq_stats <- data.frame(
  River = c(
   "YN",
   "YI",
   "SMC",
   "DC",
   "PBMS",
   "SH",
   "PBSF"
  ),
  Slope = c(
    slope_cq(fitYN),
    slope_cq(fitYI),
    slope_cq(fitSMC),
    slope_cq(fitDC),
    slope_cq(fitPBMS),
    slope_cq(fitSH),
    slope_cq(fitPBSF)
  ),
  Intercept = c(
    intercept_cq(fitYN),
    intercept_cq(fitYI),
    intercept_cq(fitSMC),
    intercept_cq(fitDC),
    intercept_cq(fitPBMS),
    intercept_cq(fitSH),
    intercept_cq(fitPBSF)
  ),
  # Adjusted_R2 = c(
  #   r.sqr.lm_cq(fitYN),
  #   r.sqr.lm_cq(fitYI),
  #   r.sqr.lm_cq(fitSMC),
  #   r.sqr.lm_cq(fitDC),
  #   r.sqr.lm_cq(fitPBMS),
  #   r.sqr.lm_cq(fitSH),
  #   r.sqr.lm_cq(fitPBSF)
    
 # ),
  P_value = c("<0.0001", "<0.0001", "<0.0001", "<0.0001", "<0.0001", "<0.0001", "<0.0001"
              # pvalue(labYN, fieldcondYN, YN_cond_data),
              # pvalue(lab6MC, fieldcond6MC, SMC_cond_data),
              # pvalue(labDC, fieldcondDC, DC_cond_data),
              # pvalue(labPBMS, fieldcondPBMS, PBMS_cond_data),
              # pvalue(labPBSF, fieldcondPBSF, PBSF_cond_data),
              # pvalue(labYI, fieldcondYI, YI_cond_data),
              # pvalue(labWIC, fieldcondWIC, WIC_cond_data),
              # pvalue(labSW, fieldcondSW, SW_cond_data),
              # pvalue(labYS, fieldcondYS, YS_cond_data),
              # SH.p
  )
)


gt_tbl <- gt(all_cq_stats)
simpleregtable <- gt_tbl %>%
  cols_label(
    River = "Tributary",
    Slope = "Slope",
    Intercept = "Intercept",
    #Adjusted_R2 = html("R<sup>2<sup>"),
    P_value = "P-Value"
  ) %>%
  tab_header(
    title = "Specific Conductivity - Discharge Regression Statistics",
    subtitle = "Log(SC) - Log(Discharge) for all C-Q data") %>%
  tab_source_note(source_note = "Table X."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/QC_Plots/Eckhardt_Method/all_cq/stats.png", expand = 10, zoom = 10)













##event-averaged stormflow cq elationships #####
YN_each_event <- each_event_cq(YN_events_bf)
YI_each_event <- each_event_cq(YI_events_bf)
SMC_each_event <- each_event_cq(SMC_events_bf)
DC_each_event <- each_event_cq(DC_events_bf)
PBMS_each_event <- each_event_cq(PBMS_events_bf)
SH_each_event <- each_event_cq(SH_events_bf)
PBSF_each_event <- each_event_cq(PBSF_events_bf)

Averaged_seasonal <- averaged_seasonal_cq(YN_each_event, "YN") %>%
  bind_rows(averaged_seasonal_cq(YI_each_event, "YI"),
            averaged_seasonal_cq(SMC_each_event, "SMC"),
            averaged_seasonal_cq(DC_each_event, "DC"),
            averaged_seasonal_cq(PBMS_each_event, "PBMS"),
            averaged_seasonal_cq(SH_each_event, "SH"),
            averaged_seasonal_cq(PBSF_each_event, "PBSF"))

Averaged_seasonal$season = factor(Averaged_seasonal$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "annual"))

a<- ggplot(Averaged_seasonal) + 
  geom_point(aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 21) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", end = 0.9) +
  scale_fill_viridis_d(option = "inferno", end = 0.9, alpha = 0.5) +
  labs(x = "SC-Discharge Slope",
       y = "Season") +
  theme(legend.title = element_blank()) +
  labs(title = "Stormflow - 
seasonally averaged") +
  theme(legend.position = "bottom")  +
  guides(color = guide_legend(nrow = 1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

##averaged baseflow cq relationships #####
Averaged_seasonal_bf <- seasonal_baseflow(YN_events_bf, "YN") %>%
  bind_rows(seasonal_baseflow(YI_events_bf, "YI"),
            seasonal_baseflow(SMC_events_bf, "SMC"),
            seasonal_baseflow(DC_events_bf, "DC"),
            seasonal_baseflow(PBMS_events_bf, "PBMS"),
            seasonal_baseflow(SH_events_bf, "SH"),
            seasonal_baseflow(PBSF_events_bf, "PBSF"))
Averaged_seasonal_bf$season = factor(Averaged_seasonal_bf$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "annual"))

b <- ggplot() + 
  geom_point(Averaged_seasonal_bf, mapping = aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 24) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_viridis_d(option = "inferno", end = 0.9) +
  scale_fill_viridis_d(option = "inferno", end = 0.9, alpha = 0.5) +
  labs(x = "SC-Discharge Slope",
       y = "Season") +
  theme(legend.position = "none") +
  labs(title= "Baseflow") 



#Bulk Stormflow CQ slopes, intercepts etc. #####
Bulk_Stormflow <- YN_bulk <- bulk_stormflow(YN_events_bf, "YN") %>%
  bind_rows(YI_bulk) %>% # <- bulk_stormflow(YI_events_bf, "YI")) %>% #calculation done manually and separately because YI does not exhibit stormflow in each month and R code errors.
  bind_rows(DC_bulk <- bulk_stormflow(DC_events_bf, "DC")) %>%
  bind_rows(SMC_bulk <- bulk_stormflow(SMC_events_bf, "SMC")) %>%
  bind_rows(PBMS_bulk <- bulk_stormflow(PBMS_events_bf, "PBMS")) %>%
  bind_rows(SH_bulk <- bulk_stormflow(SH_events_bf, "SH")) %>%
  bind_rows(PBSF_bulk <- bulk_stormflow(PBSF_events_bf, "PBSF"))  
Bulk_Stormflow$season = factor(Bulk_Stormflow$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "annual"))

c <- ggplot(Bulk_Stormflow) + 
  geom_point(aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 23) +
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





#seasonal and annual cq slopes for baseflow, stormflow averaged by event, stormflow bulk averaged ######
(b | a | c) +
  plot_annotation(tag_levels = 'a',tag_suffix = ')',
                  caption = "Figure X. Specific conductivity (SC) - discharge (Q) slopes for a) baseflow (squares), b) event-
averaged stormflow (circles), and c) bulk-averaged stormflow (diamonds). Slopes are categorized 
by season and as annual. Colors indicate tributary. Grey boxes indicate chemostatic behavior.",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0),
                                legend.position = "bottom")) +plot_layout(guides = "collect")
ggsave("Plots/QC_plots/Eckhardt_Method/slopes.png", height = 4.25, width = 6.25, units = "in")




#figure of all event sloeps, entire cq slope
all_events <- bind_rows(YN_each_event %>% mutate(trib = "YN"),
                        YI_each_event %>% mutate(trib = "YI"), 
                        SMC_each_event %>% mutate(trib = "SMC"), 
                        DC_each_event %>% mutate(trib = "DC"), 
                        PBMS_each_event %>% mutate(trib = "PBMS"),
                        SH_each_event %>% mutate(trib = "SH"), 
                        PBSF_each_event %>% mutate(trib = "PBSF"))

all_events$season = factor(all_events$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"))



ggplot() +
  labs(x = "SC-Q Slope", y = "Tributary") + 
  geom_point(all_events, mapping = aes(slope, trib, color = season), size = 2.5, shape = 21) +
   scale_color_manual(labels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
                      values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")) +
   # scale_fill_manual(labels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
   #                    values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")) +
  theme(legend.title = element_blank()) +
  L_theme() +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  geom_point(all_cq_stats, mapping = aes(Slope, River), shape = "|", size = 6) +
  geom_point(Averaged_seasonal_bf %>% filter(season == "annual"), mapping = aes(slope, trib), shape = "|", size = 6, color = "red") +
  labs(caption = "Figure X. Specific conductivity (SC) - Discharge (Q) slopes for stormflow event represented by 
circles. Colors of circles indicate season in which stormfow event occured. Black lines are the
SC-Q slopes for all SC and flow data over the entire study period. Red lines are the SC-Q slopes
for SC and flow data during baseflow only. Grey box indicates chemostatic behavior.")

ggsave("Plots/QC_plots/Eckhardt_Method/event_slopes.png", height = 4.25, width = 6.25, units = "in")

