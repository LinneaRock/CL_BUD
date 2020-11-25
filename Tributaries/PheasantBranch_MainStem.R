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
loggerPBMS <- loggerPBMS  #HOBO conductivity data
fieldcondPBMS <- fieldcondPBMS #conductivity measured in the field
labPBMS <- labPBMS #IC data 
PBMS_discharge <- d.PBMS

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
PBMS_cond_data <- find_outlier(loggerPBMS, fieldcondPBMS, "PBMSoutliers", "PBMSoutliers_month")

#Conductivity time series
PBMS_cond_plot <- cond(PBMS_cond_data) +
  capt_scseries("Pheasant Branch Main Stem", "Pheasant Branch Main Stem")
splot("conductance_time_series/", "PBMS")

#Chloride time series
PBMS_cl_plot <- clseries(labPBMS) +
  capt_clseries("Pheasant Branch Main Stem", "Pheasant Branch Main Stem")
splot("chloride_time_series/", "PBMS")

#Discharge time series
PBMS_discharge_plot <- discharge_ts(PBMS_discharge)

#cQ - conductivity
q.sc(PBMS_cond_data, PBMS_discharge)+
  captqec('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", PBMS_cond_data, PBMS_discharge)
splot("QC_plots/", "PBMS_cond")

#cQ - chloride
q.cl(labPBMS, PBMS_discharge) +
  captqc('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", labPBMS, PBMS_discharge)
splot("QC_plots/", "PBMS_cl")

#Linear Regression between Conductivity and Chloride
PBMS_linreg_plot <- linreg(labPBMS, PBMS_cond_data) +
  captlm('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", labPBMS, PBMS_cond_data)
splot("cl_cond_linear_regression/", "PBMS")

#conductivity time series with chloride points overlain
sccl(PBMS_cond_data, labPBMS)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondPBMS, loggerPBMS)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclPBMS, labPBMS)
