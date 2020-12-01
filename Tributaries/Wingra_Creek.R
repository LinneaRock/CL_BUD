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
loggerWIC <- loggerWIC  #HOBO conductivity data
fieldcondWIC <- fieldcondWIC #conductivity measured in the field
labWIC <- labWIC #IC data 
WIC_discharge <- read.csv("Data/WingraCreek_Data/discharge_WIC.csv") %>%
  mutate(date = ymd_hms(date))
WIC_discharge <- rolling_ave_discharge(loggerWIC, WIC_discharge)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
WIC_cond_data <- find_outlier(loggerWIC, fieldcondWIC, "WICoutliers", "WICoutliers_month")

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
