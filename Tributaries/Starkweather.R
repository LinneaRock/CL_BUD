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
loggerSW <- loggerSW  #HOBO conductivity data
fieldcondSW <- fieldcondSW #conductivity measured in the field
labSW <- labSW #IC data 


#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
SW_cond_data <- find_outlier(loggerSW, fieldcondSW, "SWoutliers", "SWoutliers_month")

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
SW_linreg_plot <- linreg(labSW, SW_cond_data) +
  captlm('Starkweather Creek',"Starkweather Creek at Olbrich Garden", labSW, SW_cond_data)
splot("cl_cond_linear_regression/", "SW")

#conductivity time series with chloride points overlain
sccl(SW_cond_data, labSW)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondSW, loggerSW)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclSW, labSW)
