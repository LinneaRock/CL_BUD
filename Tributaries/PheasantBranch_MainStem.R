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

#HOBO conductivity data, add missing dates
loggerPBMS1 <- loggerPBMS %>% 
  complete(date = seq.POSIXt(as.POSIXct("2020-10-22 11:30:00"), as.POSIXct("2020-10-30 10:30:00"), by = "30 mins")) %>%
  arrange(date)
#impute missing data
loggerPBMS <- impute_missing(loggerPBMS1)

#flag outliers using anomalize package
PBMS_outlier <- flagged_data(loggerPBMS)
#plot to inspect where to correct outliers
plot_flagged(PBMS_outlier)
#after inspecting, filter and clean anomalies
PBMS_cleaned <- PBMS_outlier %>%
  filter(Year_Month == "2020-5" & observed > 1250 |
           Year_Month == "2020-6" & observed > 1000 |
           Year_Month == "2020-7" & observed > 1000) %>%
  clean_anomalies()
#insepect cleaned points
plot_cleaned(PBMS_cleaned)
#final dataset with runningmean, trend, and corrected specific conductance data
PBMS_cond_data <- final_cond_data(loggerPBMS, PBMS_cleaned, PBMS_outlier)



fieldcondPBMS <- fieldcondPBMS #conductivity measured in the field
labPBMS <- labPBMS #IC data 
PBMS_discharge <- rolling_ave_discharge(loggerPBMS, d.PBMS)

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
splot("discharge_time_series/", "PBMS")

#cQ - conductivity
q.sc(PBMS_cond_data, PBMS_discharge)+
  captqec('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", PBMS_cond_data, PBMS_discharge)
splot("QC_plots/", "PBMS_cond")

evalqec(PBMS_cond_data, PBMS_discharge)

#cQ - chloride
q.cl(labPBMS, PBMS_discharge) +
  captqc('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", labPBMS, PBMS_discharge)
splot("QC_plots/", "PBMS_cl")

evalq(labPBMS, PBMS_discharge)

#Linear Regression between Conductivity and Chloride
PBMS_linreg_plot <- linreg(labPBMS, PBMS_cond_data) +
  captlm('Pheasant Branch Main Stem',"Pheasant Branch Main Stem", labPBMS, PBMS_cond_data)
splot("cl_cond_linear_regression/", "PBMS")

eval(labPBMS, PBMS_cond_data)

#conductivity time series with chloride points overlain
sccl(PBMS_cond_data, labPBMS)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondPBMS, loggerPBMS)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclPBMS, labPBMS)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, PBMS_discharge, PBMS_cond_data, labPBMS)
ggsave("Plots/TS_Grids/PBMS.png", height = 12, width = 16)


