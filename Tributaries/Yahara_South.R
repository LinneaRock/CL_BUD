library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(anomalize)
library(zoo)
library(cowplot)

source("Functions/linreg.R")
source("Functions/splot.R")
source("Functions/cond.R")
source("Functions/clseries.R")
source("Functions/sccl.R")
source("Functions/cl_compare.R")
source("Functions/cond_compare.R")
source("Functions/histlinreg.R")
source("Functions/outlierdetection.R")

#calling and naming raw data
YS_logger_data <- loggerYS #HOBO conductivity data
YS_chloride <- labYS #IC data 
YS_discharge <- read.csv("Data/Monona_Outlet_Data/d_YS.csv")

#Preparing datasets through rolling averages and removing outliers that greatly impact the data
YS_runmean <- rollavg6hr(YS_logger_data)
YS_outlier <- find_outliers(YS_runmean)
plot_outliers(YS_runmean, YS_outlier)
anom_detect(YS_logger_data)
YS_runmean2 <- remove_outliers(YS_logger_data, YS_outlier)
plot_6hrave(YI_runmean2)

#Conductivity time series

#Chloride time series

#Discharge time series

#Linear Regression between Conductivity and Chloride

#conductivity time series with chloride points overlain

#Comparing conductivity collected with handheld meter and HOBO collected

#Comparing chloride concentrations collected with YSI and lab analyzed 