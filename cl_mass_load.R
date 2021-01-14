# source("Functions/chloride_mass_tribs.R")
# 
# #function to get daily mass chloride loading
# format_daily <- function(massdata) {
#   new <- massdata %>%
#     mutate(cl_mass = cl_rate * 1800) %>% #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * 1800 s]
#     separate(date, c("date", "time"), sep = " ") %>%
#     mutate(date = as.Date(date)) %>%
#     group_by(date) %>% #steps above to create a column for date grouping
#     mutate(daily_mean_conc = mean(chloride_use)) %>% #daily average concentration [mg/L]
#     mutate(cl_load = sum(cl_mass)) %>% #total daily chloride mass [g]
#     mutate(cl_load = cl_load / 1000000) #daily chloride mass in metric tonnes [Mg]
# }
# 
# 
# #Mendota Loading####
# 
# ##YN
# YN_Load <- chloride_mass_load_rate(labYN, YN_cond_data, YN_discharge) #table containing chloride loading values in [g s^-1] per time step 
# plot_concentration(YN_Load, "Yahara North")
# plot_load(YN_Load, "Yahara North")
# splot("chloride_loading/", "YN_ts")
# YN_load_daily <- format_daily(YN_Load)
# plot_concentration_ave(YN_load_daily, "Yahara North")
# plot_load_daily(YN_load_daily, "Yahara North") #daily mass of chloride from tributary
# splot("chloride_loading/", "YN_daily")
# plot_cumulative(YN_load_daily, "Yahara North")
# splot("chloride_loading/", "YN_cumulative")
# 
# 
# 
# ##6MC
# SMC_Load <- chloride_mass_load_rate(lab6MC, SMC_cond_data, SMC_discharge)
# plot_load(SMC_Load, "Sixmile Creek")
# splot("chloride_loading/", "6MC_ts")
# SMC_load_daily <- format_daily(SMC_Load)
# plot_load_daily(SMC_load_daily, "Sixmile Creek")
# splot("chloride_loading/", "6MC_daily")
# plot_cumulative(SMC_load_daily, "Sixmile Creek")
# splot("chloride_loading/", "6MC_cumulative")
# 
# ggplot(YN_load_daily) +
#   geom_point(aes(date, chloride_predict)) +
#   geom_point(aes(date, chloride_mgL), color ="red")
# 
# ##DC
# DC_Load <- chloride_mass_load_rate(labDC, DC_cond_data, DC_discharge)
# plot_load(DC_Load, "Dorn Creek")
# splot("chloride_loading/", "DC_ts")
# DC_load_daily <- format_daily(DC_Load)
# plot_load_daily(DC_load_daily, "Dorn Creek")
# splot("chloride_loading/", "DC_daily")
# plot_cumulative(DC_load_daily, "Dorn Creek")
# splot("chloride_loading/", "DC_cumulative")
# 
# ##PBMS
# PBMS_Load <- chloride_mass_load_rate(labPBMS, PBMS_cond_data, PBMS_discharge)
# plot_load(PBMS_Load, "Pheasant Branch Main Stem")
# splot("chloride_loading/", "PBMS_ts")
# PBMS_load_daily <- format_daily(PBMS_Load)
# plot_load_daily(PBMS_load_daily, "Pheasant Branch Main Stem")
# splot("chloride_loading/", "PBMS_daily")
# plot_cumulative(PBMS_load_daily, "Pheasant Branch Main Stem")
# splot("chloride_loading/", "PBMS_cumulative")
# 
# ##PBSF
# PBSF_Load <- chloride_mass_load_rate(labPBSF, PBSF_cond_data, PBSF_discharge)
# plot_load(PBSF_Load, "Pheasant Branch South Fork")
# splot("chloride_loading/", "PBSF_ts")
# PBSF_load_daily <- format_daily(PBSF_Load)
# plot_load_daily(PBSF_load_daily, "Pheasant Branch South Fork")
# splot("chloride_loading/", "PBSF_ts")
# plot_cumulative(PBSF_load_daily, "Pheasant Branch South Fork")
# splot("chloride_loading/", "PBSF_cumulative")
# 
#    ###Total Mass Loading for Mendota
# #not including PBSF because PBMS is better representative of the water entering Mendota
# Total_ME_Mass <- rbind(PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
#   select(date, cl_load) %>% 
#   distinct() %>%
#   group_by(date) %>% 
#   summarise(tot.mass = sum(cl_load)) #total sum mass chloride [Mg] loading into ME from tribs
# 
# ggplot(Total_ME_Mass) +
#   geom_bar(aes(date, tot.mass), stat = "identity")
# 
# #Monona Loading####
# ##YI
# YI_Load <-chloride_mass_load_rate(labYI, YI_cond_data, YI_discharge)
# plot_load(YI_Load, "Yahara Isthmus")
# splot("chloride_loading/", "YI_ts")
# YI_load_daily <- format_daily(YI_Load)
# plot_load_daily(YI_load_daily, "Yahara Isthmus")
# splot("chloride_loading/", "YI_daily")
# plot_cumulative(YI_load_daily, "Yahara Isthmus")
# splot("chloride_loading/", "YI_cumulative")
# 
# #not sure how to do this for SW and YS yet
# 
# 
# 
# #Total Mass Loading all tribs####
# Total_Mass <- rbind(YI_load_daily, PBSF_load_daily, PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
#   select(date, cl_load) %>% 
#   distinct() %>%
#   group_by(date) %>% 
#   summarise(tot.mass = sum(cl_load)) #total sum mass chloride [Mg] loading into ME from tribs
# 
# 
# ggplot(Total_Mass) +
#   geom_bar(aes(date, tot.mass), stat = "identity")
#   
# 
# #call in Winter19 dataframe from road_salt_application.R
# Winter19 <- Winter19  #be sure date column is named date, not DATE
# 
# Chloride_by_date <- merge(Winter19, Total_Mass, by = "date", all = TRUE) %>% #join datasets together to directly compare chloride mass
#   select(date, total_salt, tot.mass) %>%
#   distinct()
# 
# 
# 
# ggplot() +
#   geom_line(Winter19, mapping = aes(date, total_salt), color = "#F24D29") +
#   geom_line(Total_Mass, mapping = aes(date, tot.mass), color = "#1C366B") +
#   L_theme() +
#   labs(x ="",
#        y= "Chloride Mass"~(Mg))
# 
# 
# 
# 
# #plotting modeled chloride and real values
# 
# ggplot(PBMS_Load) +
#   geom_point(aes(date, chloride_predict)) +
#   geom_point(aes(date, chloride_mgL), color = "red")










#YN chloride mass loading calculations
YN_ts_mass <- chloride_ts_mass(labYN, YN_cond_data, YN_discharge)
YN_daily_mass <- chloride_daily_mass(YN_ts_mass)
YN_monthly_mass <- chloride_monthly_load(YN_ts_mass)
#YN_seasonal_mass <- chloride_seasonal_load(YN_ts_mass)
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

monthly_load(YN_monthly_mass, "Yahara North")
ggsave("Plots/chloride_loading/YN/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(YN_seasonal_mass, "Yahara North")




#YI chloride mass loading calculations
YI_ts_mass <- chloride_ts_mass(labYI, YI_cond_data, YI_discharge)
YI_daily_mass <- chloride_daily_mass(YI_ts_mass)
YI_monthly_mass <- chloride_monthly_load(YI_ts_mass)
#YI_seasonal_mass <- chloride_seasonal_load(YI_ts_mass)
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

monthly_load(YI_monthly_mass, "Yahara Isthmus")
ggsave("Plots/chloride_loading/YI/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(YI_seasonal_mass, "Yahara Isthmus")






#YS chloride mass loading calculations
YS_ts_mass <- chloride_ts_mass(labYS, YS_cond_data, YS_discharge)
YS_daily_mass <- chloride_daily_mass(YS_ts_mass)
YS_monthly_mass <- chloride_monthly_load(YS_ts_mass)
#YS_seasonal_mass <- chloride_seasonal_load(YS_ts_mass)
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

monthly_load(YS_monthly_mass, "Yahara South")
ggsave("Plots/chloride_loading/YS/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(YS_seasonal_mass, "Yahara South")






#SMC chloride mass loading calculations
SMC_ts_mass <- chloride_ts_mass(lab6MC, SMC_cond_data, SMC_discharge)
SMC_daily_mass <- chloride_daily_mass(SMC_ts_mass)
SMC_monthly_mass <- chloride_monthly_load(SMC_ts_mass)
#SMC_seasonal_mass <- chloride_seasonal_load(SMC_ts_mass)
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

monthly_load(SMC_monthly_mass, "Sixmile Creek")
ggsave("Plots/chloride_loading/SMC/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(SMC_seasonal_mass, "Sixmile Creek")






#DC chloride mass loading calculations
DC_ts_mass <- chloride_ts_mass(labDC, DC_cond_data, DC_discharge)
DC_daily_mass <- chloride_daily_mass(DC_ts_mass)
DC_monthly_mass <- chloride_monthly_load(DC_ts_mass)
#DC_seasonal_mass <- chloride_seasonal_load(DC_ts_mass)
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

monthly_load(DC_monthly_mass, "Dorn Creek")
ggsave("Plots/chloride_loading/DC/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(DC_seasonal_mass, "Dorn Creek")




#PBMS chloride mass loading calculations
PBMS_ts_mass <- chloride_ts_mass(labPBMS, PBMS_cond_data, PBMS_discharge)
PBMS_daily_mass <- chloride_daily_mass(PBMS_ts_mass)
PBMS_monthly_mass <- chloride_monthly_load(PBMS_ts_mass)
#PBMS_seasonal_mass <- chloride_seasonal_load(PBMS_ts_mass)
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

monthly_load(PBMS_monthly_mass, "Pheasant Branch Main Stem")
ggsave("Plots/chloride_loading/PBMS/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(PBMS_seasonal_mass, "Pheasant Branch Main Stem")




#PBSF chloride mass loading calculations
PBSF_ts_mass <- chloride_ts_mass(labPBSF, PBSF_cond_data, PBSF_discharge)
PBSF_daily_mass <- chloride_daily_mass(PBSF_ts_mass)
PBSF_monthly_mass <- chloride_monthly_load(PBSF_ts_mass)
#PBSF_seasonal_mass <- chloride_seasonal_load(PBSF_ts_mass)
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

monthly_load(PBSF_monthly_mass, "Pheasant Branch South Fork")
ggsave("Plots/chloride_loading/PBSF/monthly_load.png", height = 4, width = 6, units = "in")

#seasonal_load(PBSF_seasonal_mass, "Pheasant Branch South Fork")























