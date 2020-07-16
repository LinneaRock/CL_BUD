library(tidyverse)
library(anytime)
library(viridisLite)
source("Functions/splot.R")
source("Functions/L_theme.R")
   
#read in the csv file rbind them and save as .rds in the git project.
#attach <- read.csv("C:/Users/Linne/OneDrive/Documents/ME_profile_070220.csv") %>%
#  mutate(sampledate = anytime::anydate(sampledate)) #works better than lubridate in this circumstance


#ME_profile <- read_rds("Data/ME_YSI_2020/ME_profiles.rds") 

#attach2 <- rbind(ME_profile, attach) #be sure that the previous version of ME_profiles is loaded

#write_rds(attach2, "Data/ME_YSI_2020/ME_profiles.rds")

ME_profile <- read_rds("Data/ME_YSI_2020/ME_profiles.rds") #July 11 data may be incorrect.. we'll reassess after next profile is in


ggplot(ME_profile, aes(sp_cond, depth)) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap(~sampledate, ncol = 3, scales = "free_x") +
  geom_path() +
  L_theme() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nDepth"~(m))
splot("conductance_time_series/", "ME_summer2020_perdate")



ggplot(ME_profile, aes(sampledate, sp_cond, color = depth)) +
  geom_line(aes(group = depth)) +
  scale_color_viridis_c(direction = -1) +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       x = "") +
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))


splot("conductance_time_series/", "ME_summer2020_perdepth")
