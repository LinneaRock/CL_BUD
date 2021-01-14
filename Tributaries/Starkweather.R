library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)
#library(imputeTS)
library(anomalize)

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
#source("functions/impute_missing.R")



# #flag outliers using anomalize package
# SW_outlier <- flagged_data(loggerSW)
# #plot to inspect where to correct outliers
# plot_flagged(SW_outlier)
# #after inspecting, filter and clean anomalies
# SW_cleaned <- SW_outlier %>%
#   filter(Year_Month != "2019-12" &
#            Year_Month != "2020-1" &
#            Year_Month != "2020-2" &
#            Year_Month != "2020-12") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(SW_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# SW_cond_data <- final_cond_data(loggerSW, SW_cleaned, SW_outlier)
# write_rds(SW_cond_data, "Data/HOBO_Loggers/SW/SW_cond_data.rds")


SW_cond_data <- read_rds("Data/HOBO_Loggers/SW/SW_cond_data.rds")
fieldcondSW <- fieldcondSW #conductivity measured in the field
labSW <- labSW #IC data 


#Conductivity time series
SW_cond_plot <- cond(SW_cond_data) +
  capt_scseries("Starkweather Creek", "Starkweather Creek at Olbrich Garden")
splot("conductance_time_series/", "SW")

#Chloride time series
SW_cl_plot <- clseries(labSW) +
  capt_clseries("Starkweather Creek", "Starkweather Creek at Olbrich Garden")
splot("chloride_time_series/", "SW")

#Discharge time series

#cQ - conductivity

#cQ - chloride


#Linear Regression between Conductivity and Chloride
SW_linreg_plot <- linreg(labSW, SW_cond_data) + labs(title = "Starkweather Creek")
  #captlm('Starkweather Creek',"Starkweather Creek at Olbrich Garden", labSW, SW_cond_data)
splot("cl_cond_linear_regression/", "SW")

eval(labSW, SW_cond_data)

#conductivity time series with chloride points overlain
sccl(SW_cond_data, labSW)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondSW, SW_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclSW, labSW)


#plotting a grid of timeseries data
ts_grid(precip_temp_data, SW_discharge, SW_cond_data, labSW)
ggsave("Plots/TS_Grids/SW.png", height = 12, width = 16)
