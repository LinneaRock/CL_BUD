library(tidyverse)
library(anytime)
   
one <- read_csv("DATA/ME_YSI_2020/ME_profile_033120.csv")
two <- read_csv("DATA/ME_YSI_2020/ME_profile_042420.csv")
three <- read_csv("DATA/ME_YSI_2020/ME_profile_050120.csv")
four <- read_csv("DATA/ME_YSI_2020/ME_profile_050620.csv")
five <- read_csv("DATA/ME_YSI_2020/ME_profile_051220.csv")
six <- read_csv("DATA/ME_YSI_2020/ME_profile_052220.csv")
seven <- read_csv("DATA/ME_YSI_2020/ME_profile_052720.csv")
eight <- read_csv("DATA/ME_YSI_2020/ME_profile_060420.csv")
nine <- read_csv("DATA/ME_YSI_2020/ME_profile_061020.csv") 
ten <- read_csv("DATA/ME_YSI_2020/ME_profile_061820.csv")

ME_cond_profiles <- rbind(one, two, three, four)
ME_cond_profiles <- rbind(ME_cond_profiles, five, six, seven, eight, nine, ten)
write_rds(ME_cond_profiles, "DATA/ME_YSI_2020/ME_profiles.rds")


ME_profile <- read_rds("DATA/ME_YSI_2020/ME_profiles.rds") %>%
  mutate(sampledate = anytime::anydate(sampledate))

