library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)

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

#calling and naming raw data
loggerYN <- loggerYN  #HOBO conductivity data
fieldcondYN <- fieldcondYN #conductivity measured in the field
labYN <- labYN #IC data 
YN_discharge <- rolling_ave_discharge(loggerYN, d.YN)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
YN_cond_data <- find_outlier(loggerYN, fieldcondYN, "YNoutliers", "YNoutliers_month")

#Conductivity time series
YN_cond_plot <- cond(YN_cond_data) +
  capt_scseries("Yahara River @ 113", "Yahara River at Highway 113")
splot("conductance_time_series/", "YN")

#Chloride time series
YN_cl_plot <- clseries(labYN) +
  capt_clseries("Yahara River @ 113", "Yahara River at Highway 113")
splot("chloride_time_series/", "YN")

#Discharge time series
YN_discharge_plot <- discharge_ts(YN_discharge)
splot("discharge_time_series/", "YN")

#cQ - conductivity
q.sc(YN_cond_data, YN_discharge)+
  captqec('Yahara River @ 113',"Yahara River at Highway 113", YN_cond_data, YN_discharge)
splot("QC_plots/", "YN_cond")

evalqec(YN_cond_data, YN_discharge)

#cQ - chloride
q.cl(labYN, YN_discharge) +
  captqc('Yahara River @ 113',"Yahara River at Highway 113", labYN, YN_discharge)
splot("QC_plots/", "YN_cl")

evalq(labYN, YN_discharge)

#Linear Regression between Conductivity and Chloride
YN_linreg_plot <- linreg(labYN, YN_cond_data) +
  captlm('Yahara River @ 113',"Yahara River at Highway 113", labYN, YN_cond_data)
splot("cl_cond_linear_regression/", "YN")

eval(labYN, YN_cond_data)

#conductivity time series with chloride points overlain
sccl(YN_cond_data, labYN)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYN, loggerYN)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYN, labYN)


#plotting a grid of timeseries data
ts_grid(precip_temp_data, YN_discharge, YN_cond_data, labYN)
ggsave("Plots/TS_Grids/YN.png", height = 12, width = 16)


