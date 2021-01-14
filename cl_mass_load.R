
source("Functions/chloride_mass_tribs.R")



#YN chloride mass loading calculations
YN_ts_mass <- chloride_ts_mass(labYN, YN_cond_data, YN_discharge)
YN_daily_mass <- chloride_daily_mass(YN_ts_mass)
YN_monthly_mass <- chloride_monthly_load(YN_ts_mass)
YN_seasonal_mass <- chloride_seasonal_load(YN_ts_mass)
YN_annual_mass <- chloride_annual_load(YN_ts_mass)

#YN chloride mass loading plots
concentration_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(YN_ts_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(YN_daily_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(YN_daily_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(YN_monthly_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(YN_seasonal_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/seasonal_load.png", height = 4, width = 6, units = "in")




#YI chloride mass loading calculations
YI_ts_mass <- chloride_ts_mass(labYI, YI_cond_data, YI_discharge)
YI_daily_mass <- chloride_daily_mass(YI_ts_mass)
YI_monthly_mass <- chloride_monthly_load(YI_ts_mass)
YI_seasonal_mass <- chloride_seasonal_load(YI_ts_mass)
YI_annual_mass <- chloride_annual_load(YI_ts_mass)

#YI chloride mass loading plots
concentration_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(YI_ts_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(YI_daily_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(YI_daily_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(YI_monthly_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(YI_seasonal_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/seasonal_load.png", height = 4, width = 6, units = "in")





#YS chloride mass loading calculations
YS_ts_mass <- chloride_ts_mass(labYS, YS_cond_data, YS_discharge)
YS_daily_mass <- chloride_daily_mass(YS_ts_mass)
YS_monthly_mass <- chloride_monthly_load(YS_ts_mass)
YS_seasonal_mass <- chloride_seasonal_load(YS_ts_mass)
YS_annual_mass <- chloride_annual_load(YS_ts_mass)

#YS chloride mass loading plots
concentration_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(YS_ts_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(YS_daily_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(YS_daily_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(YS_monthly_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(YS_seasonal_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/seasonal_load.png", height = 4, width = 6, units = "in")





#SMC chloride mass loading calculations
SMC_ts_mass <- chloride_ts_mass(lab6MC, SMC_cond_data, SMC_discharge)
SMC_daily_mass <- chloride_daily_mass(SMC_ts_mass)
SMC_monthly_mass <- chloride_monthly_load(SMC_ts_mass)
SMC_seasonal_mass <- chloride_seasonal_load(SMC_ts_mass)
SMC_annual_mass <- chloride_annual_load(SMC_ts_mass)

#SMC chloride mass loading plots
concentration_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(SMC_ts_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(SMC_daily_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(SMC_daily_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(SMC_monthly_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(SMC_seasonal_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/seasonal_load.png", height = 4, width = 6, units = "in")





#DC chloride mass loading calculations
DC_ts_mass <- chloride_ts_mass(labDC, DC_cond_data, DC_discharge)
DC_daily_mass <- chloride_daily_mass(DC_ts_mass)
DC_monthly_mass <- chloride_monthly_load(DC_ts_mass)
DC_seasonal_mass <- chloride_seasonal_load(DC_ts_mass)
DC_annual_mass <- chloride_annual_load(DC_ts_mass)

#DC chloride mass loading plots
concentration_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(DC_ts_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(DC_daily_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(DC_daily_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(DC_monthly_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(DC_seasonal_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/YN/seasonal_load.png", height = 4, width = 6, units = "in")





#PBMS chloride mass loading calculations
PBMS_ts_mass <- chloride_ts_mass(labPBMS, PBMS_cond_data, PBMS_discharge)
PBMS_daily_mass <- chloride_daily_mass(PBMS_ts_mass)
PBMS_monthly_mass <- chloride_monthly_load(PBMS_ts_mass)
PBMS_seasonal_mass <- chloride_seasonal_load(PBMS_ts_mass)
PBMS_annual_mass <- chloride_annual_load(PBMS_ts_mass)

#PBMS chloride mass loading plots
concentration_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(PBMS_ts_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(PBMS_daily_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(PBMS_daily_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(PBMS_monthly_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(PBMS_seasonal_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/seasonal_load.png", height = 4, width = 6, units = "in")





#PBSF chloride mass loading calculations
PBSF_ts_mass <- chloride_ts_mass(labPBSF, PBSF_cond_data, PBSF_discharge)
PBSF_daily_mass <- chloride_daily_mass(PBSF_ts_mass)
PBSF_monthly_mass <- chloride_monthly_load(PBSF_ts_mass)
PBSF_seasonal_mass <- chloride_seasonal_load(PBSF_ts_mass)
PBSF_annual_mass <- chloride_annual_load(PBSF_ts_mass)

#PBSF chloride mass loading plots
concentration_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(PBSF_ts_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(PBSF_daily_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(PBSF_daily_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(PBSF_monthly_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(PBSF_seasonal_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/seasonal_load.png", height = 4, width = 6, units = "in")






# #SW chloride mass loading calculations
# SW_ts_mass <- chloride_ts_mass(labSW, SW_cond_data, SW_discharge)
# SW_daily_mass <- chloride_daily_mass(SW_ts_mass)
# SW_monthly_mass <- chloride_monthly_load(SW_ts_mass)
# SW_seasonal_mass <- chloride_seasonal_load(SW_ts_mass)
# SW_annual_mass <- chloride_annual_load(SW_ts_mass)
# 
# #SW chloride mass loading plots
# concentration_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/concentration_ts.png", height = 4, width = 6, units = "in")
# 
# rate_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/rate_ts.png", height = 4, width = 6, units = "in")
# 
# load_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/load_ts.png", height = 4, width = 6, units = "in")
# 
# cumulative_ts(SW_ts_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/cumulative_ts.png", height = 4, width = 6, units = "in")
# 
# daily_load(SW_daily_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/daily_load.png", height = 4, width = 6, units = "in")
# 
# daily_ave_conc(SW_daily_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/daily_ave_conc.png", height = 4, width = 6, units = "in")
# 
# monthly_load(SW_monthly_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/monthly_load.png", height = 4, width = 6, units = "in")
# 
# seasonal_load(SW_seasonal_mass, "Starkweather Creek")
# ggsave("Plots/chloride_loading/SW/seasonal_load.png", height = 4, width = 6, units = "in")







#WIC chloride mass loading calculations
WIC_ts_mass <- chloride_ts_mass(labWIC, WIC_cond_data, WIC_discharge)
WIC_daily_mass <- chloride_daily_mass(WIC_ts_mass)
WIC_monthly_mass <- chloride_monthly_load(WIC_ts_mass)
WIC_seasonal_mass <- chloride_seasonal_load(WIC_ts_mass)
WIC_annual_mass <- chloride_annual_load(WIC_ts_mass)

#WIC chloride mass loading plots
concentration_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/concentration_ts.png", height = 4, width = 6, units = "in")

rate_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/rate_ts.png", height = 4, width = 6, units = "in")

load_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/load_ts.png", height = 4, width = 6, units = "in")

cumulative_ts(WIC_ts_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/cumulative_ts.png", height = 4, width = 6, units = "in")

daily_load(WIC_daily_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/daily_load.png", height = 4, width = 6, units = "in")

daily_ave_conc(WIC_daily_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/daily_ave_conc.png", height = 4, width = 6, units = "in")

monthly_load(WIC_monthly_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/monthly_load.png", height = 4, width = 6, units = "in")

seasonal_load(WIC_seasonal_mass, "Wingra Creek")
ggsave("Plots/chloride_loading/WIC/seasonal_load.png", height = 4, width = 6, units = "in")











