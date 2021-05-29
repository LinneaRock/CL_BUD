
source("Functions/chloride_mass_tribs.R")
options(scipen = 999) #turn off scientific notation for graph


#YN chloride mass loading calculations
YN_ts_mass <- chloride_ts_mass(labYN, YN_cond_data, YN_discharge)
YN_daily_mass <- chloride_daily_mass(YN_ts_mass)
YN_monthly_mass <- chloride_monthly_load(YN_ts_mass)
YN_seasonal_mass <- chloride_seasonal_load(YN_ts_mass)
YN_annual_mass <- chloride_annual_load(YN_ts_mass)

#YN chloride mass loading plots
YN <- concentration_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(YN_daily_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(YN_daily_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(YN_monthly_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(YN_seasonal_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/seasonal_load.png", height = 15, width = 20, units = "cm")




#YI chloride mass loading calculations
YI_ts_mass <- chloride_ts_mass(labYI, YI_cond_data, YI_discharge)
YI_daily_mass <- chloride_daily_mass(YI_ts_mass)
YI_monthly_mass <- chloride_monthly_load(YI_ts_mass)
YI_seasonal_mass <- chloride_seasonal_load(YI_ts_mass)
YI_annual_mass <- chloride_annual_load(YI_ts_mass)

#YI chloride mass loading plots
YI <- concentration_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(YI_daily_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(YI_daily_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(YI_monthly_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(YI_seasonal_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/seasonal_load.png", height = 15, width = 20, units = "cm")





#YS chloride mass loading calculations
YS_ts_mass <- chloride_ts_mass(labYS, YS_cond_data, YS_discharge)
YS_daily_mass <- chloride_daily_mass(YS_ts_mass)
YS_monthly_mass <- chloride_monthly_load(YS_ts_mass)
YS_seasonal_mass <- chloride_seasonal_load(YS_ts_mass)
YS_annual_mass <- chloride_annual_load(YS_ts_mass)

#YS chloride mass loading plots
YS <- concentration_ts(YS_ts_mass, "Yahara South") 
ggsave("Plots/chloride_loading/YS/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(YS_daily_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(YS_daily_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(YS_monthly_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(YS_seasonal_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/seasonal_load.png", height = 15, width = 20, units = "cm")





#SMC chloride mass loading calculations
SMC_ts_mass <- chloride_ts_mass(lab6MC, SMC_cond_data, SMC_discharge)
SMC_daily_mass <- chloride_daily_mass(SMC_ts_mass)
SMC_monthly_mass <- chloride_monthly_load(SMC_ts_mass)
SMC_seasonal_mass <- chloride_seasonal_load(SMC_ts_mass)
SMC_annual_mass <- chloride_annual_load(SMC_ts_mass)

#SMC chloride mass loading plots
SMC <- concentration_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(SMC_daily_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(SMC_daily_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(SMC_monthly_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(SMC_seasonal_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/seasonal_load.png", height = 15, width = 20, units = "cm")





#DC chloride mass loading calculations
DC_ts_mass <- chloride_ts_mass(labDC, DC_cond_data, DC_discharge)
DC_daily_mass <- chloride_daily_mass(DC_ts_mass)
DC_monthly_mass <- chloride_monthly_load(DC_ts_mass)
DC_seasonal_mass <- chloride_seasonal_load(DC_ts_mass)
DC_annual_mass <- chloride_annual_load(DC_ts_mass)

#DC chloride mass loading plots
DC <- concentration_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(DC_daily_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(DC_daily_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(DC_monthly_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(DC_seasonal_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/seasonal_load.png", height = 15, width = 20, units = "cm")





#PBMS chloride mass loading calculations
PBMS_ts_mass <- chloride_ts_mass(labPBMS, PBMS_cond_data, PBMS_discharge)
PBMS_daily_mass <- chloride_daily_mass(PBMS_ts_mass)
PBMS_monthly_mass <- chloride_monthly_load(PBMS_ts_mass)
PBMS_seasonal_mass <- chloride_seasonal_load(PBMS_ts_mass)
PBMS_annual_mass <- chloride_annual_load(PBMS_ts_mass)

#PBMS chloride mass loading plots
PBMS <- concentration_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(PBMS_daily_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(PBMS_daily_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(PBMS_monthly_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(PBMS_seasonal_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/seasonal_load.png", height = 15, width = 20, units = "cm")





#PBSF chloride mass loading calculations
PBSF_ts_mass <- chloride_ts_mass(labPBSF, PBSF_cond_data, PBSF_discharge)
PBSF_daily_mass <- chloride_daily_mass(PBSF_ts_mass)
PBSF_monthly_mass <- chloride_monthly_load(PBSF_ts_mass)
PBSF_seasonal_mass <- chloride_seasonal_load(PBSF_ts_mass)
PBSF_annual_mass <- chloride_annual_load(PBSF_ts_mass)

#PBSF chloride mass loading plots
PBSF<- concentration_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(PBSF_daily_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(PBSF_daily_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(PBSF_monthly_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(PBSF_seasonal_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/seasonal_load.png", height = 15, width = 20, units = "cm")






# #SW chloride mass loading calculations
 SW_ts_mass <- chloride_ts_mass(labSW, SW_cond_data, WIC_discharge)
 SW_daily_mass <- chloride_daily_mass(SW_ts_mass)
# SW_monthly_mass <- chloride_monthly_load(SW_ts_mass)
# SW_seasonal_mass <- chloride_seasonal_load(SW_ts_mass)
# SW_annual_mass <- chloride_annual_load(SW_ts_mass)
# 
# #SW chloride mass loading plots
SW <- concentration_ts(SW_ts_mass, "Starkweather Creek")
 ggsave("Plots/chloride_loading/SW/concentration_ts.png", height = 15, width = 20, units = "cm")
# 
# rate_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/rate_ts.png", height = 15, width = 20, units = "cm")
# 
# load_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/load_ts.png", height = 15, width = 20, units = "cm")
# 
# cumulative_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/cumulative_ts.png", height = 15, width = 20, units = "cm")
# 
# daily_load(SW_daily_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/daily_load.png", height = 15, width = 20, units = "cm")
# 
 daily_ave_conc(SW_daily_mass, "Starkweather Creek")
 ggsave("Plots/chloride_loading/SW/daily_ave_conc.png", height = 15, width = 20, units = "cm")
# 
# monthly_load(SW_monthly_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/monthly_load.png", height = 15, width = 20, units = "cm")
# 
# seasonal_load(SW_seasonal_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/seasonal_load.png", height = 15, width = 20, units = "cm")







#WIC chloride mass loading calculations
WIC_ts_mass <- chloride_ts_mass(labWIC, WIC_cond_data, WIC_discharge)
WIC_daily_mass <- chloride_daily_mass(WIC_ts_mass)
WIC_monthly_mass <- chloride_monthly_load(WIC_ts_mass)
WIC_seasonal_mass <- chloride_seasonal_load(WIC_ts_mass)
WIC_annual_mass <- chloride_annual_load(WIC_ts_mass)

#WIC chloride mass loading plots
WIC <- concentration_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(WIC_daily_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(WIC_daily_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(WIC_monthly_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(WIC_seasonal_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/seasonal_load.png", height = 15, width = 20, units = "cm")




####SH loading -- run the code in WC&SH.R first
SH_ts_mass <- SH_ts_mass %>% 
  filter(date < as.POSIXct("2021-04-10 12:00:00")) %>%
  filter(date > as.POSIXct("2019-12-18 00:00:00"))
SH_daily_mass <- chloride_daily_mass(SH_ts_mass) 
SH_monthly_mass <- chloride_monthly_load(SH_ts_mass)
SH_seasonal_mass <- chloride_seasonal_load(SH_ts_mass)
SH_annual_mass <- chloride_annual_load(SH_ts_mass)

#SH chloride mass loading plots
SH<-concentration_ts(SH_ts_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/concentration_ts.png", height = 15, width = 20, units = "cm")

rate_ts(SH_ts_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/rate_ts.png", height = 15, width = 20, units = "cm")

load_ts(SH_ts_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/load_ts.png", height = 15, width = 20, units = "cm")

cumulative_ts(SH_ts_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/cumulative_ts.png", height = 15, width = 20, units = "cm")

daily_load(SH_daily_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/daily_load.png", height = 15, width = 20, units = "cm")

daily_ave_conc(SH_daily_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/daily_ave_conc.png", height = 15, width = 20, units = "cm")

monthly_load(SH_monthly_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/monthly_load.png", height = 15, width = 20, units = "cm")

seasonal_load(SH_seasonal_mass, "Spring Harbor Storm Sewer")
ggsave("Plots/chloride_loading/SH/seasonal_load.png", height = 15, width = 20, units = "cm")





#concentrations plot - Lake Mendota Watershed inlets
TS_ME <- bind_rows(SH_ts_mass %>% mutate(ID = "SH"), PBSF_ts_mass %>% mutate(ID = "PBSF"), PBMS_ts_mass %>% mutate(ID = "PBMS"), YN_ts_mass %>% mutate(ID = "YN"), DC_ts_mass %>% mutate(ID = "DC"), SMC_ts_mass %>% mutate(ID = "SMC")) %>%
  filter(date < as.POSIXct("2021-04-10 12:00:00")) %>%
  filter(date > as.POSIXct("2019-12-18 00:00:00"))


ggplot(TS_ME) +
  geom_line(aes(date, chloride_predict)) +
  facet_wrap(~ID, scales = "free_y") +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Estimated chloride concentration timeseries for waters flowing into Lake 
Mendota. Note: PBSF flows into PBMS, rather than being a direct tributary to Lake 
Mendota.") +
  L_theme() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
ggsave("Plots/chloride_time_series/ME_inlets.png", height = 4.25, width = 6.25, units = "in")

#concentrations plot - Lake Monona inlets
TS_MO <- bind_rows(YI_ts_mass %>% mutate(ID = "YI"), SW_ts_mass %>% mutate(ID = "SW"), WIC_ts_mass %>% mutate(ID = "WIC"))  %>%
  filter(date < as.POSIXct("2021-04-10 12:00:00")) %>%
  filter(date > as.POSIXct("2019-12-18 00:00:00"))


ggplot(TS_MO) +
  geom_line(aes(date, chloride_predict)) +
  facet_wrap(~ID, scales = "free_y") +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Estimated chloride concentration timeseries for waters flowing into Lake 
Monona. Note: YI is also the outlfow of Lake Mendota.") +
  L_theme() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Plots/chloride_time_series/MO_inlets.png", height = 4.25, width = 6.25, units = "in")

#outlet lake Monona
ggplot(YS_ts_mass) +
  geom_line(aes(date, chloride_predict)) +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Estimated chloride concentration timeseries for the Yahara River outflow of
Lake Monona.") +
  L_theme() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Plots/chloride_time_series/MO_outlet(YI).png", height = 4.25, width = 6.25, units = "in")



#histograms of concentrations
all_ts <- bind_rows(TS_ME, TS_MO, YS_ts_mass) %>%
  group_by(ID) %>%
  mutate(median = median(chloride_predict, na.rm = TRUE)) %>%
  ungroup()



ggplot(all_ts, aes(x=chloride_predict, group = ID)) +
  geom_histogram() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ID, scales = "free") +
  labs(x = "Chloride Concentration"~(mg~L^-1), y = "Count", 
       caption = "Figure X. Histograms of chloride concentrations calculated using equations from linear 
regressions and continuous conductivity data.") +
  L_theme()
  
ggsave("Plots/chloride_loading/histograms.png", height = 4.25, width = 6.25, units = "in")











annualsalt_201920 <- function(data) {
  
  data1 <- data %>%
    filter(season_id == "2019-2020 Salting" |
             season_id == "2020 Non-Salting")
  
  total <- sum(data1$cl_load_g, na.rm = TRUE)
  
}

winter_201920 <- function(data) {
  
  data1 <- data %>%
    filter(season_id == "2019-2020 Salting")
  
  total <- sum(data1$cl_load_g, na.rm = TRUE)
  
}

winter_202021 <- function(data) {
  
  data1 <- data %>%
    filter(season_id == "2020-2021 Salting")
  
  total <- sum(data1$cl_load_g, na.rm = TRUE)
  
}

notwinter2020 <- function(data) {
  
  data1 <- data %>%
    filter(season_id == "2020 Non-Salting")
  
  total <- sum(data1$cl_load_g, na.rm = TRUE)
  
}

entire_load <-  function(data) {
  
  data1 <- data 
  
  total <- sum(data1$cl_load_g, na.rm = TRUE)
  
}

#total201920_load <- (annualsalt_201920(PBMS_ts_mass) + annualsalt_201920(SMC_ts_mass) + annualsalt_201920(DC_ts_mass) + annualsalt_201920(YN_ts_mass) + annualsalt_201920(YI_ts_mass) + annualsalt_201920(SH_ts_mass)) / 1000000 #total chloride in Mg 

names <- c("PBMS", "SMC", "DC", "YN", "YI", "SH", "PBSF")
annual_load_201920 <- c(annualsalt_201920(PBMS_ts_mass) , annualsalt_201920(SMC_ts_mass) , annualsalt_201920(DC_ts_mass) , annualsalt_201920(YN_ts_mass) , annualsalt_201920(YI_ts_mass) , annualsalt_201920(SH_ts_mass), annualsalt_201920(PBSF_ts_mass)) 
winter_201920 <- c(winter_201920(PBMS_ts_mass) , winter_201920(SMC_ts_mass) , winter_201920(DC_ts_mass) , winter_201920(YN_ts_mass) , winter_201920(YI_ts_mass) , winter_201920(SH_ts_mass), winter_201920(PBSF_ts_mass))
winter_202021 <- c(winter_202021(PBMS_ts_mass) , winter_202021(SMC_ts_mass) , winter_202021(DC_ts_mass) , winter_202021(YN_ts_mass) , winter_202021(YI_ts_mass) , winter_202021(SH_ts_mass), winter_202021(PBSF_ts_mass)) 
not_winter_2020 <- c(notwinter2020(PBMS_ts_mass) , notwinter2020(SMC_ts_mass) , notwinter2020(DC_ts_mass) , notwinter2020(YN_ts_mass) , notwinter2020(YI_ts_mass) , notwinter2020(SH_ts_mass), notwinter2020(PBSF_ts_mass)) 
entire <- c(entire_load(PBMS_ts_mass) , entire_load(SMC_ts_mass) , entire_load(DC_ts_mass) , entire_load(YN_ts_mass) , entire_load(YI_ts_mass) , entire_load(SH_ts_mass), entire_load(PBSF_ts_mass)) 
max <- c(max(PBMS_ts_mass$chloride_use_mgL, na.rm = TRUE) , max(SMC_ts_mass$chloride_use_mgL, na.rm = TRUE) , max(DC_ts_mass$chloride_use_mgL, na.rm = TRUE) , max(YN_ts_mass$chloride_use_mgL, na.rm = TRUE) , max(YI_ts_mass$chloride_use_mgL, na.rm = TRUE) , max(SH_ts_mass$chloride_use_mgL, na.rm = TRUE), max(PBSF_ts_mass$chloride_use_mgL, na.rm = TRUE))


loading <- as.data.frame(names) %>%
  #mutate(annual1920 = annual_load_201920 / 1000000) %>% #convert to Mg
  mutate(winter1920 = winter_201920 / 1000000) %>%
  mutate(notwinter20 = not_winter_2020 / 1000000) %>%
  mutate(winter2021 = winter_202021 / 1000000) %>%
  mutate(entireload = entire / 1000000) 

















library(pracma)
H <- c(237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258)
A <- c(24000, 43000, 120000, 263000, 472000, 767000, 1450000, 2230000, 3080000, 3900000, 4960000, 6000000, 6910000, 7220000, 7790000, 8370000, 8740000, 9420000, 10300000, 10700000, 12400000, 13700000)

morph <- as.data.frame(H) %>%
  mutate(A = A) %>%
  mutate(HH = abs(H - max(H)))

monona_volume <- trapz(x = rev(morph$HH), y = rev(morph$A))
YI2020_discharge <- YI_discharge %>%
  filter(year(date) == 2020) 
YI2020_discharge <- sum(YI2020_discharge$runningmeandis)

res_time <- monona_volume/(YI2020_discharge * 60 * 15) # 1.19
