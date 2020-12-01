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
loggerYI <- loggerYI  #HOBO conductivity data
fieldcondYI <- fieldcondYI #conductivity measured in the field
labYI <- labYI #IC data 
YI_discharge <- rolling_ave_discharge(loggerYI, d.YI)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
YI_cond_data <- find_outlier(loggerYI, fieldcondYI, "YIoutliers", "YIoutliers_month")

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
cond_compare(fieldcondYI, loggerYI)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYI, labYI)
