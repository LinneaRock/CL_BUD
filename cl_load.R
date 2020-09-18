source("Functions/chloride_mass_tribs.R")

#function to get daily mass chloride loading
format_daily <- function(massdata) {
new <- massdata %>%    
    separate(date, c("date", "time"), sep = " ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>% #steps above to create a column for date for grouping
    summarise(sum(cl_load)) %>% #total daily chloride mass
    rename(cl_load = 'sum(cl_load)') %>%
    mutate(cl_load = cl_load / 1000000) #daily chloride mass in metric tonnes
}


format_daily <- function(massdata) {
  new <- massdata %>%
    mutate(cl_mass = cl_rate * 1800) %>% #integral to determine ~chloride mass during 1800 seconds or the 30 minute timestep
    separate(date, c("date", "time"), sep = " ") %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>% #steps above to create a column for date for grouping
    mutate(cl_load <- sum(cl_mass)) %>% #total daily chloride mass
    mutate(cl_load = cl_load / 1000000) #daily chloride mass in metric tonnes
    }

#Mendota Loading####

##YN
YN_Load <- chloride_mass_load_rate(labYN, loggerYN, d.YN) #table containing chloride loading values in [g s^-1] per time step 
plot_load(YN_Load, "Yahara North")
YN_load_daily <- format_daily(YN_Load)
plot_load_daily(YN_load_daily, "Yahara North") #daily mass of chloride from tributary

##6MC
SMC_Load <- chloride_mass_load_rate(lab6MC, logger6MC, d.6MC)
plot_load(SMC_Load, "Sixmile Creek")
SMC_load_daily <- format_daily(SMC_Load)
plot_load_daily(SMC_load_daily, "Sixmile Creek")

##DC
DC_Load <- chloride_mass_load_rate(labDC, loggerDC, d.DC)
plot_load(DC_Load, "Dorn Creek")
DC_load_daily <- format_daily(DC_Load)
plot_load_daily(DC_load_daily, "Dorn Creek")

##PBMS
PBMS_Load <- chloride_mass_load_rate(labPBMS, loggerPBMS, d.PBMS)
plot_load(PBMS_Load, "Pheasant Branch Main Stem")
PBMS_load_daily <- format_daily(PBMS_Load)
plot_load_daily(PBMS_load_daily, "Pheasant Branch Main Stem")

##PBSF
PBSF_Load <- chloride_mass_load_rate(labPBSF, loggerPBSF, d.PBSF)
plot_load(PBSF_Load, "Pheasant Branch South Fork")
PBSF_load_daily <- format_daily(PBSF_Load)
plot_load_daily(PBSF_load_daily, "Pheasant Branch South Fork")

###Total Mass Loading for Mendota
#not including PBSF because PBMS is better representative of the water entering Mendota
Total_ME_Mass <- rbind(PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
  group_by(date) %>% 
  mutate(tot.mass = sum(cl_load)) #total sum mass chloride loading into ME from tribs



#Monona Loading####
##YI
YI_Load <-chloride_mass_load_rate(labYI, loggerYI, d.YI)
plot_load(YI_Load, "Yahara Isthmus")
YI_load_daily <- format_daily(YI_Load)
plot_load_daily(YI_load_daily, "Yahara Isthmus")

#not sure how to do this for SW and YS yet



#Total Mass Loading all tribs####
Total_Mass <- rbind(YI_load_daily, PBSF_load_daily, PBMS_load_daily, DC_load_daily, SMC_load_daily, YN_load_daily) %>% #stack dataframes 
  group_by(date) %>% 
  mutate(tot.mass = sum(cl_load)) #total sum mass chloride loading into ME from tribs

ggplot(Total_Mass) +
  geom_bar(aes(date, tot.mass), stat = "identity")
  

#call in Winter19 dataframe from road_salt_application.R - apologies for the messy code in that script
Winter19 <- Winter19 %>%
  rename(date = DATE)

Chloride_by_date <- merge(Winter19, Total_Mass, by = "date", all = TRUE) %>% #join datasets together to directly compare chloride mass
  select(date, total_salt, tot.mass) %>%
  distinct()



#ggplot() +
#  geom_line(Winter19, mapping = aes(DATE, total_salt), color = "#F24D29") +
#  geom_line(Total_Mass, mapping = aes(date, tot.mass), color = "#1C366B") +
#  L_theme() +
#  labs(x ="",
#       y= "Chloride Mass"~(Mg))
