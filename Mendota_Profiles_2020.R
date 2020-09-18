library(tidyverse)
library(anytime)
library(viridisLite)
source("Functions/splot.R")
source("Functions/L_theme.R")
   
#read in the csv file rbind them and save as .rds in the git project.
#attach <- read.csv("C:/Users/Linne/OneDrive/Documents/ME_profile_091720.csv") %>%
#  mutate(sampledate = anytime::anydate(sampledate)) #works better than lubridate in this circumstance


#ME_profile <- read_rds("Data/ME_YSI_2020/ME_profiles.rds") 

#attach2 <- bind_rows(ME_profile, attach) #be sure that the previous version of ME_profiles is loaded

#write_rds(attach2, "Data/ME_YSI_2020/ME_profiles.rds")

ME_profile <- read_rds("Data/ME_YSI_2020/ME_profiles.rds") %>% 
  filter(sampledate != "2020-07-11") #sensors malfunctioned this date

ggplot(ME_profile, aes(sp_cond, depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~sampledate, ncol = 3) +
  geom_path() +
  L_theme() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nDepth"~(m))
#splot("conductance_time_series/", "ME_summer2020_perdate") dimensions are too small in this code

ggsave("Plots/conductance_time_series/ME_summer2020_perdate.png", height = 8, width = 12, units = "in")

ggplot(ME_profile, aes(sampledate, sp_cond, color = depth)) +
  geom_line(aes(group = depth)) +
  scale_color_viridis_c(direction = -1) +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       x = "") +
  L_theme()


splot("conductance_time_series/", "ME_summer2020_perdepth")


#heat map? not really what I was going for but kind of. Maybe there is not enough data for it to look great?
ggplot(ME_profile %>% filter(sampledate != "2020-07-11"), aes(sampledate, depth, fill = sp_cond)) +
  geom_tile() +
  scale_fill_viridis_c() +
  L_theme()+
  scale_y_reverse()
