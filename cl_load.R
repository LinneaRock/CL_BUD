source("Functions/chloride_mass_tribs.R")



#Mendota Loading

##YN
YN_Load <- chloride_mass_load_rate(labYN, loggerYN, d.YN) #table containing chloride loading values in [g s^-1] per time step 

plot_load(YN_Load, "Yahara North")

YN_load_daily <- YN_Load %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  
