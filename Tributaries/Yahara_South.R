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


# 
# #flag outliers using anomalize package
# YS_outlier <- flagged_data(loggerYS)
# #plot to inspect where to correct outliers
# plot_flagged(YS_outlier)
# #after inspecting, filter and clean anomalies
# YS_cleaned <- YS_outlier %>%
#   filter(Year_Month != "2020-1") %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(YS_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# YS_cond_data <- final_cond_data(loggerYS, YS_cleaned, YS_outlier)
# write_rds(YS_cond_data, "Data/HOBO_Loggers/YS/YS_cond_data.rds")


YS_cond_data <- read_rds("Data/HOBO_Loggers/YS/YS_cond_data.rds")
fieldcondYS <- fieldcondYS #conductivity measured in the field
labYS <- labYS #IC data 
YS_discharge <- read.csv("Data/Monona_Outlet_Data/d_YS.csv") %>%
  mutate(date = ymd_hms(date))
YS_discharge <- rolling_ave_discharge(YS_cond_data, YS_discharge)


#Conductivity time series
YS_cond_plot <- cond(YS_cond_data) +
  capt_scseries("Yahara South", "Yahara River at Broadway St")
splot("conductance_time_series/", "YS")

#Chloride time series
YS_cl_plot <- clseries(labYS) +
  capt_clseries("Yahara South", "Yahara River at Broadway St")
splot("chloride_time_series/", "YS")

#Discharge time series
YS_discharge_plot <- discharge_ts(YS_discharge)
splot("discharge_time_series/", "YS")

#cQ - conductivity
q.sc(YS_cond_data, YS_discharge)+
  captqec('Yahara River @ Broadway',"Yahara River at Broadway St", YS_cond_data, YS_discharge)
splot("QC_plots/", "YS_cond")

evalqec(YS_cond_data, YS_discharge)

#cQ - chloride
q.cl(labYS, YS_discharge) +
  captqc('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_discharge)
splot("QC_plots/", "YS_cl")

evalq(labYS, YS_discharge)
  
#Linear Regression between Conductivity and Chloride
YS_linreg_plot <- linreg(labYS, YS_cond_data) + labs(title = "Yahara South")
  #captlm('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_cond_data)
splot("cl_cond_linear_regression/", "YS")

eval(labYS, YS_cond_data)

#conductivity time series with chloride points overlain
sccl(YS_cond_data, labYS)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYS, YS_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYS, labYS)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, YS_discharge, YS_cond_data, labYS)
ggsave("Plots/TS_Grids/YS.png", height = 12, width = 16)
