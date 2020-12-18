library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)
library(imputeTS)

source("Functions/linreg.R")
source("Functions/splot.R")
source("Functions/cond.R")
source("Functions/clseries.R")
source("Functions/sccl.R")
source("Functions/cl_compare.R")
source("Functions/cond_compare.R")
source("Functions/find_outlier.R")
source("Functions/qsc.R")
source("Functions/qcl.R")
source("functions/discharge_ts.R")
source("functions/impute_missing.R")

# #HOBO conductivity data
# loggerWIC <- loggerWIC
# 
# #flag outliers using anomalize package
# WIC_outlier <- flagged_data(loggerWIC)
# #plot to inspect where to correct outliers
# plot_flagged(WIC_outlier)
# #after inspecting, filter and clean anomalies
# WIC_cleaned <- WIC_outlier %>%
#   filter(Year_Month != "2020-12") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(WIC_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# WIC_cond_data <- loggerWIC %>%
#   left_join(WIC_cleaned, by = "date") %>%
#   left_join(WIC_outlier %>% select(date, trend), by = "date") %>%
#   mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
#   select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
#   rename(trend = trend.y) %>%
#   mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
#   mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
#   mutate(runningmean = ifelse(row_number() >= (nrow(loggerWIC) - 5), mean(sp.cond[(nrow(loggerWIC) - 5):nrow(loggerWIC)]), runningmean)) 
# 
# write_rds(WIC_cond_data, "Data/HOBO_Loggers/WIC/WIC_cond_data.rds")


WIC_cond_data <- read_rds("Data/HOBO_Loggers/WIC/WIC_cond_data.rds")
fieldcondWIC <- fieldcondWIC #conductivity measured in the field
labWIC <- labWIC #IC data 
WIC_discharge <- read.csv("Data/WingraCreek_Data/discharge_WIC.csv") %>%
  mutate(date = ymd_hms(date))
WIC_discharge <- rolling_ave_discharge(loggerWIC, WIC_discharge)


#Conductivity time series
WIC_cond_plot <- cond(WIC_cond_data) +
  capt_scseries("Wingra Creek", "Wingra Creek at Monona Inlet")
splot("conductance_time_series/", "WIC")

#Chloride time series
WIC_cl_plot <- clseries(labWIC) +
  capt_clseries("Wingra Creek", "Wingra Creek at Monona Inlet")
splot("chloride_time_series/", "WIC")

#Discharge time series
WIC_discharge_plot <- discharge_ts(WIC_discharge)
splot("discharge_time_series/", "WIC")

#cQ - conductivity
q.sc(WIC_cond_data, WIC_discharge)+
  captqec('Wingra Creek',"Wingra Creek at Monona Inlet", WIC_cond_data, WIC_discharge)
splot("QC_plots/", "WIC_cond")

evalqec(WIC_cond_data, WIC_discharge)

#cQ - chloride
q.cl(labWIC, WIC_discharge) +
  captqc('Wingra Creek',"Wingra Creek at Monona Inlet", labWIC, WIC_discharge)
splot("QC_plots/", "WIC_cl")

evalq(labWIC, WIC_discharge)

#Linear Regression between Conductivity and Chloride
WIC_linreg_plot <- linreg(labWIC, WIC_cond_data) +
  captlm('Wingra Creek',"Wingra Creek at Monona Inlet", labWIC, WIC_cond_data)
splot("cl_cond_linear_regression/", "WIC")

eval(labWIC, WIC_cond_data)

#conductivity time series with chloride points overlain
sccl(WIC_cond_data, labWIC)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondWIC, loggerWIC)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclWIC, labWIC)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, WIC_discharge, WIC_cond_data, labWIC)
ggsave("Plots/TS_Grids/WIC.png", height = 12, width = 16)
