library(tidyverse)
library(anytime)
   
#read in the csv file rbind them and save as .rds in the git project.

ME_profile <- read_rds("DATA/ME_YSI_2020/ME_profiles.rds") %>%
  mutate(sampledate = anytime::anydate(sampledate)) #works better than lubridate in this circumstance

