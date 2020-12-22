library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpubr)
library(zoo)
library(viridisLite)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/find_outlier.R")
source("Functions/cond_compare.R")
source("Functions/clseries.R")



#RawHOBO conductivity data
# loggerME_Epi <- loggerME_Epi
# loggerME_Hypo <- loggerME_Hypo
# loggerMO_Epi <- loggerMO_Epi
# loggerMO_Hypo <- loggerMO_Hypo

# ####ME_Epi Conductance data cleaning####
# #flag outliers using anomalize package
# ME_Epi_outlier <- flagged_data(loggerME_Epi)
# #plot to inspect where to correct outliers
# plot_flagged(ME_Epi_outlier)
# #after inspecting, filter and clean anomalies
# ME_Epi_cleaned <- ME_Epi_outlier %>%
#   #filter(Year_Month != "2020-12") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(ME_Epi_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# ME_Epi_cond_data <- loggerME_Epi %>%
#   left_join(ME_Epi_cleaned, by = "date") %>%
#   left_join(ME_Epi_outlier %>% select(date, trend), by = "date") %>%
#   mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
#   select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
#   rename(trend = trend.y) %>%
#   mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
#   mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
#   mutate(runningmean = ifelse(row_number() >= (nrow(loggerME_Epi) - 5), mean(sp.cond[(nrow(loggerME_Epi) - 5):nrow(loggerME_Epi)]), runningmean))
# 
# write_rds(ME_Epi_cond_data, "Data/HOBO_Loggers/MENDOTA/ME_Epi_cond_data.rds")
# 
# 
# ####ME_Hypo Conductance data cleaning####
# #flag outliers using anomalize package
# ME_Hypo_outlier <- flagged_data(loggerME_Hypo)
# #plot to inspect where to correct outliers
# plot_flagged(ME_Hypo_outlier)
# #after inspecting, filter and clean anomalies
# ME_Hypo_cleaned <- ME_Hypo_outlier %>%
#   filter(Year_Month != "2020-1") %>%
#   filter(date < "2020-03-23 00:00:00" |
#            date > "2020-04-01 00:00:00") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(ME_Hypo_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# ME_Hypo_cond_data <- loggerME_Hypo %>%
#   left_join(ME_Hypo_cleaned, by = "date") %>%
#   left_join(ME_Hypo_outlier %>% select(date, trend), by = "date") %>%
#   mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
#   select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
#   rename(trend = trend.y) %>%
#   mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
#   mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
#   mutate(runningmean = ifelse(row_number() >= (nrow(loggerME_Hypo) - 5), mean(sp.cond[(nrow(loggerME_Hypo) - 5):nrow(loggerME_Hypo)]), runningmean))
# 
# write_rds(ME_Hypo_cond_data, "Data/HOBO_Loggers/MENDOTA/ME_Hypo_cond_data.rds")
# 
# 
# 
# ####MO_Hypo Conductance data cleaning####
# #flag outliers using anomalize package
# MO_Epi_outlier <- flagged_data(loggerMO_Epi)
# #plot to inspect where to correct outliers
# plot_flagged(MO_Epi_outlier)
# #after inspecting, filter and clean anomalies
# MO_Epi_cleaned <- MO_Epi_outlier %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(MO_Epi_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# MO_Epi_cond_data <- loggerMO_Epi %>%
#   left_join(MO_Epi_cleaned, by = "date") %>%
#   left_join(MO_Epi_outlier %>% select(date, trend), by = "date") %>%
#   mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
#   select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
#   rename(trend = trend.y) %>%
#   mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
#   mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
#   mutate(runningmean = ifelse(row_number() >= (nrow(loggerMO_Epi) - 5), mean(sp.cond[(nrow(loggerMO_Epi) - 5):nrow(loggerMO_Epi)]), runningmean))
# 
# write_rds(MO_Epi_cond_data, "Data/HOBO_Loggers/MENDOTA/MO_Epi_cond_data.rds")
# 
# 
# ####MO_Hypo Conductance data cleaning####
# #flag outliers using anomalize package
# MO_Hypo_outlier <- flagged_data(loggerMO_Hypo)
# #plot to inspect where to correct outliers
# plot_flagged(MO_Hypo_outlier)
# #after inspecting, filter and clean anomalies
# MO_Hypo_cleaned <- MO_Hypo_outlier %>%
#   filter(Year_Month == "2020-3" |
#            Year_Month == "2020-5") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(MO_Hypo_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# MO_Hypo_cond_data <- loggerMO_Hypo %>%
#   left_join(MO_Hypo_cleaned, by = "date") %>%
#   left_join(MO_Hypo_outlier %>% select(date, trend), by = "date") %>%
#   mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
#   select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
#   rename(trend = trend.y) %>%
#   mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
#   mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
#   mutate(runningmean = ifelse(row_number() >= (nrow(loggerMO_Hypo) - 5), mean(sp.cond[(nrow(loggerMO_Hypo) - 5):nrow(loggerMO_Hypo)]), runningmean))
# 
# write_rds(MO_Hypo_cond_data, "Data/HOBO_Loggers/MONONA/MO_Hypo_cond_data.rds")




##lake data import####
ME_Epi_cond_data <- read_rds("Data/HOBO_Loggers/MENDOTA/ME_Epi_cond_data.rds")
ME_Hypo_cond_data <- read_rds("Data/HOBO_Loggers/MENDOTA/ME_Hypo_cond_data.rds")

MO_Epi_cond_data <- read_rds("Data/HOBO_Loggers/MONONA/MO_Epi_cond_data.rds")
MO_Hypo_cond_data <- read_rds("Data/HOBO_Loggers/MONONA/MO_Hypo_cond_data.rds")

labME <- labME
labMO <- labMO
fieldcondME <- fieldcondME
fieldcondMO <- fieldcondMO


lakecond(ME_Epi_cond_data, ME_Hypo_cond_data, "Mendota 24m", "Mendota 1.5m") +
  capt_scseries("Lake Mendota", "deep hole of Lake Mendota")
splot("conductance_time_series/", "ME")

ME_Epi_cond_plot <- cond(ME_Epi_cond_data) +
  capt_scseries("Lake Mendota", "~2m below the surface in Lake Mendota")
splot("conductance_time_series/", "ME_Epi")
ME_Hypo_cond_plot <- cond(ME_Hypo_cond_data) +
  capt_scseries("Lake Mendota", "1m off the bottom of Lake Mendota")
splot("conductance_time_series/", "ME_Hypo")


lakecond(MO_Epi_cond_data, MO_Hypo_cond_data, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona")
splot("conductance_time_series/", "MO")

MO_Epi_cond_plot <- cond(MO_Epi_cond_data) +
  capt_scseries("Lake Monona", "~2m below the surface in Lake Monona")
splot("conductance_time_series/", "MO_Epi")
MO_Hypo_cond_plot <- cond(MO_Hypo_cond_data) +
  capt_scseries("Lake Monona", "1m off the bottom of Lake Monona")
splot("conductance_time_series/", "MO_Hypo")


cond_compare(fieldcondME %>% filter(depth == 24.0), loggerME_Hypo)
cond_compare(fieldcondME %>% filter(depth == 1.5), loggerME_Epi)
cond_compare(fieldcondMO %>% filter(depth == 20), loggerMO_Hypo)
cond_compare(fieldcondMO %>% filter(depth == 1.5), loggerMO_Epi)



ME_chloride_plot <- clseries(labME) + geom_point(aes(color = Depth_m)) + scale_color_viridis_c(direction = -1) +
  capt_clseries("Mendota", "Lake Mendota")
splot("chloride_time_series/", "ME")


MO_chloride_plot <- clseries(labMO) + geom_point(aes(color = Depth_m)) + scale_color_viridis_c(direction = -1) +
  capt_clseries("Monona", "Lake Monona")
splot("chloride_time_series/", "MO")


ggplot(labME, aes(Depth_m, chloride_mgL, group = Depth_m)) +
  geom_boxplot() 



#Calculating residence time
ME_vol <- 506880000 #volume in [m^3]
MO_vol <- 111520000 #volume in [m^3]


format_discharge <- function(d) {
    d %>%
    filter(year(date) == 2019) %>%
    group_by(yday(date)) %>%
    summarise(meandaily = mean(discharge * 3600 *24)) %>% 
    ungroup() %>%
    summarise(annual = sum(meandaily)) 
  
}


ME_discharge_out <- d.YI %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge *3600 * 24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily)) 

res.time <- ME_vol / ME_discharge_out

PBMS_in <- d.PBMS %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SMC_in <- d.6MC %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

YN_in <- d.YN %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SH_in <- d.sc.SH %>%
  filter(year(date) == 2019) %>%
  na.omit %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

ME_discharge_in <- SH_in + PBMS_in + SMC_in + YN_in

ME_discharge_in/ME_discharge_out

(ME_vol + ME_discharge_in) - ME_discharge_out






