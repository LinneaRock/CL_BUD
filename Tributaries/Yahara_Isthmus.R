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

# #HOBO conductivity data, add missing dates
# loggerYI1 <- loggerYI %>% 
#   complete(date = seq.POSIXt(as.POSIXct("2020-10-22 10:00:00"), as.POSIXct("2020-10-30 9:00:00"), by = "30 mins")) %>%
#   arrange(date)
# #impute missing data
# loggerYI <- impute_missing(loggerYI1)
# 
# #flag outliers using anomalize package
# YI_outlier <- flagged_data(loggerYI)
# #plot to inspect where to correct outliers
# plot_flagged(YI_outlier)
# #after inspecting, filter and clean anomalies
# YI_cleaned <- YI_outlier %>%
#   clean_anomalies()
# #insepect cleaned points
# plot_cleaned(YI_cleaned)
# #final dataset with runningmean, trend, and corrected specific conductance data
# YI_cond_data <- final_cond_data(loggerYI, YI_cleaned, YI_outlier)
# write_rds(YI_cond_data, "Data/HOBO_Loggers/YI/YI_cond_data.rds")


YI_cond_data <- read_rds("Data/HOBO_Loggers/YI/YI_cond_data.rds")
fieldcondYI <- fieldcondYI #conductivity measured in the field
labYI <- labYI #IC data 
YI_discharge <- rolling_ave_discharge(YI_cond_data, d.YI)


#Conductivity time series
YI_cond_plot <- cond(YI_cond_data) +
  capt_scseries("Yahara River @ Main St", "Yahara River at Main St")
splot("conductance_time_series/", "YI")

#Chloride time series
YI_cl_plot <- clseries(labYI) +
  capt_clseries("Yahara River @ Main St", "Yahara River at Main St")
splot("chloride_time_series/", "YI")

#Discharge time series
YI_discharge_plot <- discharge_ts(YI_discharge)
splot("discharge_time_series/", "YI")

#cQ - conductivity
q.sc(YI_cond_data, YI_discharge) +
  captqec('Yahara River @ Main St',"Yahara River at Main St", YI_cond_data, YI_discharge)
splot("QC_plots/", "YI_cond")

evalqec(YI_cond_data, YI_discharge)

#cQ - chloride
q.cl(labYI, YI_discharge) +
  captqc('Yahara River @ Main St',"Yahara River at Main St", labYI, YI_discharge)
splot("QC_plots/", "YI_cl")

evalq(labYI, YI_discharge)

#Linear Regression between Conductivity and Chloride
YI_linreg_plot <- linreg(labYI, YI_cond_data) +
  captlm('Yahara River @ Main St',"Yahara River at Main St", labYI, YI_cond_data)
splot("cl_cond_linear_regression/", "YI")

eval(labYI, YI_cond_data)

#conductivity time series with chloride points overlain
sccl(YI_cond_data, labYI)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYI, YI_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYI, labYI)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, YI_discharge, YI_cond_data, labYI)
ggsave("Plots/TS_Grids/YI.png", height = 12, width = 16)


