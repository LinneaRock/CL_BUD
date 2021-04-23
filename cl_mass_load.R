
source("Functions/chloride_mass_tribs.R")
options(scipen = 999) #turn off scientific notation for graph


#YN chloride mass loading calculations
YN_ts_mass <- chloride_ts_mass(labYN, YN_cond_data, YN_discharge)
YN_daily_mass <- chloride_daily_mass(YN_ts_mass)
YN_monthly_mass <- chloride_monthly_load(YN_ts_mass)
YN_seasonal_mass <- chloride_seasonal_load(YN_ts_mass)
YN_annual_mass <- chloride_annual_load(YN_ts_mass)

#YN chloride mass loading plots
concentration_ts(YN_ts_mass, "Yahara North")
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
concentration_ts(YI_ts_mass, "Yahara Isthmus")
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
concentration_ts(YS_ts_mass, "Yahara South")
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
concentration_ts(SMC_ts_mass, "Sixmile Creek")
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
concentration_ts(DC_ts_mass, "Dorn Creek")
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
concentration_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
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
concentration_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
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
# concentration_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/concentration_ts.png", height = 15, width = 20, units = "cm")
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
concentration_ts(WIC_ts_mass, "Wingra Creek")
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
SH_daily_mass <- chloride_daily_mass(SH_ts_mass)
SH_monthly_mass <- chloride_monthly_load(SH_ts_mass)
SH_seasonal_mass <- chloride_seasonal_load(SH_ts_mass)
SH_annual_mass <- chloride_annual_load(SH_ts_mass)

#SH chloride mass loading plots
concentration_ts(SH_ts_mass, "Spring Harbor Storm Sewer")
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
  mutate(entireload = entire / 1000000) %>%
  mutate(maxconc = max) 







Mendota_load <- (salt_201920(PBMS_ts_mass) + salt_201920(SMC_ts_mass) + salt_201920(DC_ts_mass) + salt_201920(YN_ts_mass) + salt_201920(SH_ts_mass)) / 1000000 #total chloride in Mg 
Mendota_loss <-  (salt_201920(YI_ts_mass)) / 1000000 #total chloride in Mg 
Mendota_net <- Mendota_load - Mendota_loss






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
