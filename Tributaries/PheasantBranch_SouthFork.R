library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)
library(imputeTS)
library(anomalize)

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


#getting conductivity data ready
loggerPBSF1 <- loggerPBSF %>%  
  mutate(sp.cond = ifelse(sp.cond < 200, NA, sp.cond)) %>%
  complete(date = seq.POSIXt(as.POSIXct("2020-01-15 13:30:00"), as.POSIXct("2020-01-21 16:30:00"), by = "30 mins")) %>%
  complete(date = seq.POSIXt(as.POSIXct("2020-10-22 12:00:00"), as.POSIXct("2020-10-30 11:00:00"), by = "30 mins")) %>%
  arrange(date)

#impute missing data
loggerPBSF <- impute_missing(loggerPBSF1)


#flag outliers using anomalize package
PBSF_outlier <- flagged_data(loggerPBSF)
#plot to inspect where to correct outliers
plot_flagged(PBSF_outlier)
#after inspecting, filter and clean anomalies
PBSF_cleaned <- PBSF_outlier %>%
  filter(Year_Month != "2019-12" &
           Year_Month != "2020-1" &
           Year_Month != "2020-2") %>%
  clean_anomalies()
#insepect cleaned points
plot_cleaned(PBSF_cleaned)
#final dataset with runningmean, trend, and corrected specific conductance data
PBSF_cond_data <- final_cond_data(loggerPBSF, PBSF_cleaned, PBSF_outlier)



fieldcondPBSF <- fieldcondPBSF #conductivity measured in the field
labPBSF <- labPBSF #IC data 
PBSF_discharge <- rolling_ave_discharge(loggerPBSF2, d.PBSF)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
PBSF_cond_data <- find_outlier(loggerPBSF2, fieldcondPBSF, "PBSFoutliers", "PBSFoutliers_month")

#Conductivity time series
PBSF_cond_plot <- cond(PBSF_cond_data) +
  #geom_line(aes(date, imputed), color = "red") +
  capt_scseries("Pheasant Branch South Fork", "Pheasant Branch South Fork")
splot("conductance_time_series/", "PBSF")

#Chloride time series
PBSF_cl_plot <- clseries(labPBSF) +
  capt_clseries("Pheasant Branch South Fork", "Pheasant Branch South Fork")
splot("chloride_time_series/", "PBSF")

#Discharge time series
PBSF_discharge_plot <- discharge_ts(PBSF_discharge)
splot("discharge_time_series/", "PBSF")

#cQ - conductivity
q.sc(PBSF_cond_data, PBSF_discharge) +
  captqec('Pheasant Branch South Fork',"Pheasant Branch South Fork", PBSF_cond_data, PBSF_discharge)
splot("QC_plots/", "PBSF_cond")

evalqec(PBSF_cond_data, PBSF_discharge)

#cQ - chloride
q.cl(labPBSF, PBSF_discharge) +
  captqc('Pheasant Branch South Fork',"Pheasant Branch South Fork", labPBSF, PBSF_discharge)
splot("QC_plots/", "PBSF_cl")

evalq(labPBSF, PBSF_discharge)

#Linear Regression between Conductivity and Chloride
PBSF_linreg_plot <- linreg(labPBSF, PBSF_cond_data) +
  captlm('Pheasant Branch South Fork',"Pheasant Branch South Fork", labPBSF, PBSF_cond_data)
splot("cl_cond_linear_regression/", "PBSF")

eval(labPBSF, PBSF_cond_data)

#conductivity time series with chloride points overlain
sccl(PBSF_cond_data, labPBSF)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondPBSF, loggerPBSF)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclPBSF, labPBSF)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, PBSF_discharge, PBSF_cond_data, labPBSF)
ggsave("Plots/TS_Grids/PBSF.png", height = 12, width = 16)


