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
logger6MC <- logger6MC  #HOBO conductivity data
fieldcond6MC <- fieldcond6MC #conductivity measured in the field
lab6MC <- lab6MC #IC data 
SMC_discharge <- rolling_ave_discharge(logger6MC, d.6MC)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
SMC_cond_data <- find_outlier(logger6MC, fieldcond6MC, "6MCoutliers", "6MCoutliers_month")

#Conductivity time series
SMC_cond_plot <- cond(SMC_cond_data) +
  capt_scseries("Sixmile Creek", "Sixmile Creek at Highway M")
splot("conductance_time_series/", "6MC")

#Chloride time series
SMC_cl_plot <- clseries(lab6MC) +
  capt_clseries("Sixmile Creek", "Sixmile Creek at Highway M")
splot("chloride_time_series/", "6MC")

#Discharge time series
SMC_discharge_plot <- discharge_ts(SMC_discharge)
splot("discharge_time_series/", "6MC")

#cQ - conductivity
q.sc(SMC_cond_data, SMC_discharge)+
  captqec('Sixmile Creek',"Sixmile Creek at Highway M", SMC_cond_data, SMC_discharge)
splot("QC_plots/", "6MC_cond")

evalqec(SMC_cond_data, SMC_discharge)

#cQ - chloride
q.cl(lab6MC, SMC_discharge) +
  captqc('Sixmile Creek',"Sixmile Creek at Highway M", lab6MC, SMC_discharge)
splot("QC_plots/", "6MC_cl")

evalq(lab6MC, SMC_discharge)

#Linear Regression between Conductivity and Chloride
SMC_linreg_plot <- linreg(lab6MC, SMC_cond_data) +
  captlm('Sixmile Creek',"Sixmile Creek at Highway M", lab6MC, SMC_cond_data)
splot("cl_cond_linear_regression/", "6MC")

eval(lab6MC, SMC_cond_data)

#conductivity time series with chloride points overlain
sccl(SMC_cond_data, lab6MC)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcond6MC, logger6MC)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldcl6MC, lab6MC)


#plotting a grid of timeseries data
ts_grid(precip_temp_data, SMC_discharge, SMC_cond_data, lab6MC)
ggsave("Plots/TS_Grids/6MC.png", height = 12, width = 16)

