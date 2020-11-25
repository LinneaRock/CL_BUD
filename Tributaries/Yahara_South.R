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
loggerYS <- loggerYS  #HOBO conductivity data
fieldcondYS <- fieldcondYS #conductivity measured in the field
labYS <- labYS #IC data 
YS_discharge <- read.csv("Data/Monona_Outlet_Data/d_YS.csv") %>%
  mutate(date = ymd_hms(date))

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
YS_cond_data <- find_outlier(loggerYS, fieldcondYS, "YSoutliers", "YSoutliers_month")

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

#cQ - conductivity
q.sc(YS_cond_data, YS_discharge)+
  captqec('Yahara River @ Broadway',"Yahara River at Broadway St", YS_cond_data, YS_discharge)
splot("QC_plots/", "YS_cond")

#cQ - chloride
q.cl(labYS, YS_discharge) +
  captqc('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_discharge)
splot("QC_plots/", "YS_cl")
  
#Linear Regression between Conductivity and Chloride
YS_linreg_plot <- linreg(labYS, YS_cond_data) +
  captlm('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_cond_data)
splot("cl_cond_linear_regression/", "YS")

#conductivity time series with chloride points overlain
sccl(YS_cond_data, labYS)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYS, loggerYS)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYS, labYS)
