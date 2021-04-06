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
source("Functions/outlier_detect_remove.R")
source("Functions/qsc.R")
source("Functions/qcl.R")
source("functions/discharge_ts.R")
source("Functions/ts_grid.R")
 
# #HOBO conductivity data
 loggerWIC1 <- loggerWIC %>%
   mutate(sp.cond = ifelse(date > as.POSIXct('2021-02-06 10:00:00', tz = "ETC/GMT-7") & date < as.POSIXct('2021-02-26 19:00:00', tz = "ETC/GMT-7"), NA, sp.cond)) #logger was encased in ice during this part of the month


WIC_cond_data <- outlier_detect_remove(loggerWIC1, "WIC")
fieldcondWIC <- fieldcondWIC #conductivity measured in the field
labWIC <- labWIC #IC data 
WIC_discharge <- read.csv("Data/WingraCreek_Data/discharge_WIC.csv") %>%
  mutate(date = ymd_hms(date))
WIC_discharge <- rolling_ave_discharge(WIC_cond_data, WIC_discharge)


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
WIC_linreg_plot <- linreg(labWIC, fieldcondWIC, WIC_cond_data) + labs(title = "Wingra Creek")
  #captlm('Wingra Creek',"Wingra Creek at Monona Inlet", labWIC, WIC_cond_data)
splot("cl_cond_linear_regression/", "WIC")

eval(labWIC, WIC_cond_data)

#conductivity time series with chloride points overlain
sccl(WIC_cond_data, labWIC)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondWIC, WIC_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclWIC, labWIC)





#plotting a grid of timeseries data
ts_grid(precip_data, WIC_discharge, WIC_cond_data, labWIC)
ggsave("Plots/TS_Grids/WIC.png", height = 20, width = 15, units = "cm")
