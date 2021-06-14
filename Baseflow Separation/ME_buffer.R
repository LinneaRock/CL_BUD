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




#identify storm flow events based on the following criteria:
#The upward slope must be > 1x10^-6 cms
#The difference between the peak discharge and the Eckhardt baseflow must be at least half the total average discharge
#The downward slope must be < 0 cms and the event is over when the slope changes to >=0 cms
source("Baseflow Separation/find_peaks_eventduration.R")
timestamp <- 'date'
plot_var <- 'runningmeandis'
sb_pk_thresh <- 0.000001
sf_pk_thresh <- 0

YN_events_bf <- find.peaks(YN_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, YN_ts_mass)
SMC_events_bf <- find.peaks(SMC_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, SMC_ts_mass)
DC_events_bf <- find.peaks(DC_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, DC_ts_mass)
PBMS_events_bf <- find.peaks(PBMS_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, PBMS_ts_mass)
YI_events_bf <- find.peaks(YI_d, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, YI_ts_mass)

#plots
source("Baseflow Separation/cq_analysis_plots_CHLORIDE_fct.R")
#timeseries
grid(YN_events_bf, YN_cond_data, "56", "YN")
grid(YI_events_bf, YI_cond_data, "62", "YI")
grid(DC_events_bf, DC_cond_data, "58", "DC")
grid(SMC_events_bf, SMC_cond_data, "57", "SMC")
grid(PBMS_events_bf, PBMS_cond_data, "59", "PBMS")

#SC vs Q for all data (log-log axes)
a <- all_cq(YN_events_bf, "YR-I")
b <- all_cq(YI_events_bf, "YR-O")
c <- all_cq(SMC_events_bf, "SMC")
d <- all_cq(DC_events_bf, "DC")
e <- all_cq(PBMS_events_bf, "PBMS")


options(scipen = 999)
library(patchwork)
(c | d | e ) / (a | b) +
  plot_annotation(
    caption = "Figure X. Concentration-discharge plots on log-log axes for each river. Data includes all
chloride concentration (C) and discharge over the entire study period. Corresponding statistics
are in Table X.",
    theme = theme(plot.tag = element_text(size = 10), 
                  plot.caption = element_text(size = 10, hjust = 0)))

ggsave("Plots/QC_plots/Eckhardt_Method/all_cq/justMErivers.png", height = 4.25, width = 6.25, units = "in")

#table to accompany the above graphic
fitYN <- all_cq_stats(YN_events_bf)
fitYI <- all_cq_stats(YI_events_bf)
fitSMC <- all_cq_stats(SMC_events_bf)  
fitDC <- all_cq_stats(DC_events_bf)
fitPBMS <- all_cq_stats(PBMS_events_bf)

#####
library(gt)
library(webshot)
all_cq_stats <- data.frame(
  River = c(
    "YR-I",
    "SMC",
    "DC",
    "PBMS",
    "YR-O"
  ),
  Slope = c(
    slope_cq(fitYN),
    slope_cq(fitSMC),
    slope_cq(fitDC),
    slope_cq(fitPBMS),
    slope_cq(fitYI)
  ),
  Intercept = c(
    intercept_cq(fitYN),
    intercept_cq(fitSMC),
    intercept_cq(fitDC),
    intercept_cq(fitPBMS),
    intercept_cq(fitYI)
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
  P_value = c("<0.0001", "<0.0001", "<0.0001", "<0.0001", "<0.0001"
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
    title = "Chloride Concentration - Discharge Regression Statistics",
    subtitle = "Log(Chloride) - Log(Discharge) for all C-Q data") %>%
  tab_source_note(source_note = "Table X."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/QC_Plots/Eckhardt_Method/all_cq/ME-bufferstats.png", expand = 10, zoom = 10)



#full record seasonal c-q
cq_all_seasonal <- seasonal_fullrecord(YN_events_bf, "YR-I") %>%
  bind_rows(seasonal_fullrecord(YI_events_bf, "YR-O"),
            seasonal_fullrecord(DC_events_bf, "DC"),
            seasonal_fullrecord(SMC_events_bf, "SMC"),
            seasonal_fullrecord(PBMS_events_bf, "PBMS")) %>%
  mutate(season = ifelse(season == "annual", "Full Record", season))
cq_all_seasonal$season = factor(cq_all_seasonal$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Full Record"))

a <- ggplot() + 
  geom_point(cq_all_seasonal, mapping = aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 21) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "C-Q Slope",
       y = "") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1)) +
  labs(title= "Full Record") 


# ##event-averaged stormflow cq elationships #####
# YN_each_event <- each_event_cq(YN_events_bf)
# YI_each_event <- each_event_cq(YI_events_bf)
# SMC_each_event <- each_event_cq(SMC_events_bf)
# DC_each_event <- each_event_cq(DC_events_bf)
# PBMS_each_event <- each_event_cq(PBMS_events_bf)
# 
# 
# Averaged_seasonal <- averaged_seasonal_cq(YN_each_event, "YR-I") %>%
#   bind_rows(averaged_seasonal_cq(YI_each_event, "YR-O"),
#             averaged_seasonal_cq(SMC_each_event, "SMC"),
#             averaged_seasonal_cq(DC_each_event, "DC"),
#             averaged_seasonal_cq(PBMS_each_event, "PBMS")) %>%
#   mutate(season = ifelse(season == "annual", "Full Record", season))
# 
# Averaged_seasonal$season = factor(Averaged_seasonal$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Full Record"))
# 
# a<- ggplot(Averaged_seasonal) + 
#   geom_point(aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 21) +
#   annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
#   L_theme() +
#   scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
#   scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
#   labs(x = "C-Q Slope",
#        y = "Season") +
#   theme(legend.title = element_blank()) +
#   labs(title = "Stormflow - 
# event averaged") +
#   theme(legend.position = "bottom")  +
#   guides(color = guide_legend(nrow = 1)) +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) 

##averaged baseflow cq relationships #####
Averaged_seasonal_bf <- seasonal_baseflow(YN_events_bf, "YR-I") %>%
  bind_rows(seasonal_baseflow(YI_events_bf, "YR-O"),
            seasonal_baseflow(SMC_events_bf, "SMC"),
            seasonal_baseflow(DC_events_bf, "DC"),
            seasonal_baseflow(PBMS_events_bf, "PBMS")) %>%
  mutate(season = ifelse(season == "annual", "Full Record", season))
Averaged_seasonal_bf$season = factor(Averaged_seasonal_bf$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Full Record"))

b <- ggplot() + 
  geom_point(Averaged_seasonal_bf, mapping = aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 24) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  labs(x = "C-Q Slope",
       y = "") +
  theme(legend.position = "none") +
  labs(title= "Baseflow") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 





#Bulk Stormflow CQ slopes, intercepts etc. #####
Bulk_Stormflow <- YN_bulk <- bulk_stormflow(YN_events_bf, "YR-I") %>%
  bind_rows(YI_bulk %>% mutate(trib = "YR-O")) %>% # <- bulk_stormflow(YI_events_bf, "YI")) %>% #calculation done manually and separately because YI does not exhibit stormflow in each month and R code errors.
  bind_rows(DC_bulk <- bulk_stormflow(DC_events_bf, "DC")) %>%
  bind_rows(SMC_bulk <- bulk_stormflow(SMC_events_bf, "SMC")) %>%
  bind_rows(PBMS_bulk <- bulk_stormflow(PBMS_events_bf, "PBMS"))  %>%
  mutate(season = ifelse(season == "annual", "Full Record", season))
Bulk_Stormflow$season = factor(Bulk_Stormflow$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Full Record"))

c <- ggplot() + 
  geom_point(Bulk_Stormflow, mapping = aes(slope, reorder(season, desc(season)), color = trib, fill = trib), size = 2, shape = 23) +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey" ) +
  L_theme() +
  scale_color_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  L_theme() +
  labs(x = "C-Q Slope",
       y = "Season") +
  theme(legend.position = "none") +
  labs(title = "Stormflow - 
bulk averaged") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 




library(patchwork)
#seasonal and annual cq slopes for baseflow, stormflow averaged by event, stormflow bulk averaged ######
(a | b | c) +
  plot_annotation(tag_levels = 'a',tag_suffix = ')',
                  caption = "Figure X. Chloride concentration (C) - discharge (Q) slopes for a) the full record (circles), 
b) baseflow (triangles), and c) bulk-averaged stormflow (diamonds). Slopes are categorized 
by season and for the full record. Colors indicate tributary. Grey boxes indicate chemostatic 
behavior.",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0),
                                legend.position = "bottom")) +plot_layout(guides = "collect")
ggsave("Plots/QC_plots/Eckhardt_Method/ME-bufferslopes.png", height = 4.25, width = 6.25, units = "in")




#figure of all event sloeps, entire cq slope
all_events <- bind_rows(YN_each_event %>% mutate(trib = "YR-I"),
                        YI_each_event %>% mutate(trib = "YR-O"), 
                        SMC_each_event %>% mutate(trib = "SMC"), 
                        DC_each_event %>% mutate(trib = "DC"), 
                        PBMS_each_event %>% mutate(trib = "PBMS"))

all_events$season = factor(all_events$season, levels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"))



ggplot() +
  labs(x = "SC-Q Slope", y = "") + 
  geom_point(all_events, mapping = aes(slope, trib, color = season), size = 2.5, shape = 21) +
  scale_color_manual(labels = c("Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep"),
                     values = c("#1DACE8", "#1C366B", "#F24D29", "#E5C4A1")) +
  theme(legend.title = element_blank()) +
  L_theme() +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  geom_point(all_cq_stats, mapping = aes(Slope, River), shape = "|", size = 6) +
  geom_point(Averaged_seasonal_bf %>% filter(season == "Full Record"), mapping = aes(slope, trib), shape = "|", size = 6, color = "red") +
  labs(caption = "Figure X. Chloride concentration (C) - Discharge (Q) slopes for stormflow event 
represented by circles. Colors of circles indicate season in which stormfow event 
occured. Black lines are the C-Q slopes for all C and Q data over the entire 
study period. Red lines are the C-Q slopes for C and Q data during baseflow 
only. Grey box indicates chemostatic behavior.")

ggsave("Plots/QC_plots/Eckhardt_Method/ME-bufferevent_slopes.png", height = 4.25, width = 6.25, units = "in")

BFI_tbl <- data.frame(
  Trib = c(
    "YR-I",
    "SMC",
    "DC",
    "PBMS",
    "YR-O"
  ),
  BFI = c(calc_bfi(YN_d)$BFI * 100,
          calc_bfi(SMC_d)$BFI * 100,
          calc_bfi(DC_d)$BFI * 100,
          calc_bfi(PBMS_d)$BFI * 100,
          calc_bfi(YI_d)$BFI * 100
  ),
  stormflow_pos = c(count_mobilization_events(all_events, "YR-I"),
                    count_mobilization_events(all_events, "SMC"),
                    count_mobilization_events(all_events, "DC"),
                    count_mobilization_events(all_events, "PBMS"),
                    count_mobilization_events(all_events, "YR-O")
  ),
  stormflow_stat = c(count_chemostatic_events(all_events, "YR-I"),
                     count_chemostatic_events(all_events, "SMC"),
                     count_chemostatic_events(all_events, "DC"),
                     count_chemostatic_events(all_events, "PBMS"),
                     count_chemostatic_events(all_events, "YR-O")
                     
  ),
  stormflow_neg = c(count_dilution_events(all_events, "YR-I"),
                    count_dilution_events(all_events, "SMC"),
                    count_dilution_events(all_events, "DC"),
                    count_dilution_events(all_events, "PBMS"),
                    count_dilution_events(all_events, "YR-O")
                    
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
    title = "Chloride Concentration - Discharge Relationships",
    subtitle = "Log(C) - Log(Q) for all C-Q data") %>%
  tab_source_note(source_note = "Table X."
  ); simpleregtable

# whitespace can be set, zoom sets resolution
gtsave(data = simpleregtable, "Plots/QC_Plots/Eckhardt_Method/flow_statsMEbuffer.png", expand = 10, zoom = 10)




library(lubridate)
chloride_mass_monthly <- timeseries_mass %>%
  filter(ID != "SH") %>%
  mutate(ID = ifelse(ID == "YN", "YR-I", ID),
         ID = ifelse(ID == "YI", "YR-O", ID)) %>%
  mutate(mon = months.POSIXt(date)) %>%
  mutate(yr = year(date)) %>%
  mutate(season = NA) %>%
  mutate(season = ifelse(
    mon == "October" |
      mon == "November" |
      mon == "December" , "2020 Oct-Dec", season),
    season =  ifelse(
      mon == "January" & yr == 2020 |
        mon == "February" & yr == 2020 |
        mon == "March" &
        yr == 2020, "2020 Jan-Mar", season),
    season =  ifelse(
      mon == "January" & yr == 2021 |
        mon == "February" & yr == 2021 |
        mon == "March" &
        yr == 2021, "2021 Jan-Mar", season),
    season = ifelse(
      mon == "April" |
        mon == "May" |
        mon == "June", "2020 Apr-Jun", season),
    season = ifelse(
      mon == "July" |
        mon == "August" |
        mon == "September", "2020 Jul-Sep", season))

cl_entire <- chloride_mass_monthly %>%
  filter(yr != 2019) %>%
  filter(date < as.Date("2021-04-01")) %>%
  group_by(ID) %>%
  summarise(cl_load_Mg = sum(monthly_mass_Mg)) %>%
  mutate(season = "Full Record")

chloride_mass_seasonal <- chloride_mass_monthly %>%
  filter(yr != 2019) %>%
  filter(date < as.Date("2021-04-01")) %>%
  group_by(ID, yr, season) %>%
  summarise(cl_load_Mg = sum(monthly_mass_Mg))


chloride_mass_mebuffer <- bind_rows(chloride_mass_seasonal, cl_entire)
chloride_mass_mebuffer$season = factor(chloride_mass_mebuffer$season, levels = c("2020 Jan-Mar", "2020 Apr-Jun", "2020 Jul-Sep", "2020 Oct-Dec", "2021 Jan-Mar", "Full Record"))

ggplot(chloride_mass_mebuffer %>% filter(season != "Full Record")) +
geom_bar(aes(fill = ID, x = reorder(season, desc(season)), y = cl_load_Mg), position = "stack", stat = "identity") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 5, type = "discrete")) +
  L_theme() + theme(legend.title = element_blank()) +
  labs(y = "Mass of Chloride (Mg)", x = "",
       caption = "Figure X. Seasonal mass balance of chloride mass in metric tonnes (Mg) entering 
and exiting Lake Mendota.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave("Plots/QC_plots/Eckhardt_Method/massbalance.png", width = 6.25, height = 4.26, units = "in")




