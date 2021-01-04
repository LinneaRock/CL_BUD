source("Functions/chloride_mass_tribs.R")

#function to get daily mass chloride loading
format_daily <- function(massdata) {
  new <- massdata %>%
    mutate(cl_mass = cl_rate * 1800) %>% #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * 1800 s]
    separate(date, c("date", "time"), sep = " ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>% #steps above to create a column for date grouping
    mutate(daily_mean_conc = mean(chloride_use)) %>% #daily average concentration [mg/L]
    mutate(cl_load = sum(cl_mass)) %>% #total daily chloride mass [g]
    mutate(cl_load = cl_load / 1000000) #daily chloride mass in metric tonnes [Mg]
}


#Mendota Loading####

##YN
YN_Load <- chloride_mass_load_rate(labYN, YN_cond_data, YN_discharge) #table containing chloride loading values in [g s^-1] per time step 
plot_concentration(YN_Load, "Yahara North")
plot_load(YN_Load, "Yahara North")
splot("chloride_loading/", "YN_ts")
YN_load_daily <- format_daily(YN_Load)
plot_concentration_ave(YN_load_daily, "Yahara North")
plot_load_daily(YN_load_daily, "Yahara North") #daily mass of chloride from tributary
splot("chloride_loading/", "YN_daily")
plot_cumulative(YN_load_daily, "Yahara North")
splot("chloride_loading/", "YN_cumulative")



##6MC
SMC_Load <- chloride_mass_load_rate(lab6MC, SMC_cond_data, SMC_discharge)
plot_load(SMC_Load, "Sixmile Creek")
splot("chloride_loading/", "6MC_ts")
SMC_load_daily <- format_daily(SMC_Load)
plot_load_daily(SMC_load_daily, "Sixmile Creek")
splot("chloride_loading/", "6MC_daily")
plot_cumulative(SMC_load_daily, "Sixmile Creek")
splot("chloride_loading/", "6MC_cumulative")

ggplot(YN_load_daily) +
  geom_point(aes(date, chloride_predict)) +
  geom_point(aes(date, chloride_mgL), color ="red")

##DC
DC_Load <- chloride_mass_load_rate(labDC, DC_cond_data, DC_discharge)
plot_load(DC_Load, "Dorn Creek")
splot("chloride_loading/", "DC_ts")
DC_load_daily <- format_daily(DC_Load)
plot_load_daily(DC_load_daily, "Dorn Creek")
splot("chloride_loading/", "DC_daily")
plot_cumulative(DC_load_daily, "Dorn Creek")
splot("chloride_loading/", "DC_cumulative")

##PBMS
PBMS_Load <- chloride_mass_load_rate(labPBMS, PBMS_cond_data, PBMS_discharge)
plot_load(PBMS_Load, "Pheasant Branch Main Stem")
splot("chloride_loading/", "PBMS_ts")
PBMS_load_daily <- format_daily(PBMS_Load)
plot_load_daily(PBMS_load_daily, "Pheasant Branch Main Stem")
splot("chloride_loading/", "PBMS_daily")
plot_cumulative(PBMS_load_daily, "Pheasant Branch Main Stem")
splot("chloride_loading/", "PBMS_cumulative")

##PBSF
PBSF_Load <- chloride_mass_load_rate(labPBSF, PBSF_cond_data, PBSF_discharge)
plot_load(PBSF_Load, "Pheasant Branch South Fork")
splot("chloride_loading/", "PBSF_ts")
PBSF_load_daily <- format_daily(PBSF_Load)
plot_load_daily(PBSF_load_daily, "Pheasant Branch South Fork")
splot("chloride_loading/", "PBSF_ts")
plot_cumulative(PBSF_load_daily, "Pheasant Branch South Fork")
splot("chloride_loading/", "PBSF_cumulative")

   ###Total Mass Loading for Mendota
#not including PBSF because PBMS is better representative of the water entering Mendota
Total_ME_Mass <- rbind(PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
  select(date, cl_load) %>% 
  distinct() %>%
  group_by(date) %>% 
  summarise(tot.mass = sum(cl_load)) #total sum mass chloride [Mg] loading into ME from tribs

ggplot(Total_ME_Mass) +
  geom_bar(aes(date, tot.mass), stat = "identity")

#Monona Loading####
##YI
YI_Load <-chloride_mass_load_rate(labYI, YI_cond_data, YI_discharge)
plot_load(YI_Load, "Yahara Isthmus")
splot("chloride_loading/", "YI_ts")
YI_load_daily <- format_daily(YI_Load)
plot_load_daily(YI_load_daily, "Yahara Isthmus")
splot("chloride_loading/", "YI_daily")
plot_cumulative(YI_load_daily, "Yahara Isthmus")
splot("chloride_loading/", "YI_cumulative")

#not sure how to do this for SW and YS yet



#Total Mass Loading all tribs####
Total_Mass <- rbind(YI_load_daily, PBSF_load_daily, PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
  select(date, cl_load) %>% 
  distinct() %>%
  group_by(date) %>% 
  summarise(tot.mass = sum(cl_load)) #total sum mass chloride [Mg] loading into ME from tribs


ggplot(Total_Mass) +
  geom_bar(aes(date, tot.mass), stat = "identity")
  

#call in Winter19 dataframe from road_salt_application.R
Winter19 <- Winter19  #be sure date column is named date, not DATE

Chloride_by_date <- merge(Winter19, Total_Mass, by = "date", all = TRUE) %>% #join datasets together to directly compare chloride mass
  select(date, total_salt, tot.mass) %>%
  distinct()



ggplot() +
  geom_line(Winter19, mapping = aes(date, total_salt), color = "#F24D29") +
  geom_line(Total_Mass, mapping = aes(date, tot.mass), color = "#1C366B") +
  L_theme() +
  labs(x ="",
       y= "Chloride Mass"~(Mg))




#plotting modeled chloride and real values

ggplot(PBMS_Load) +
  geom_point(aes(date, chloride_predict)) +
  geom_point(aes(date, chloride_mgL), color = "red")
