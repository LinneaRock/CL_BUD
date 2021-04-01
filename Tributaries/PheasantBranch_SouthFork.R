library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)
#library(imputeTS)
library(anomalize)
library(ggforce)

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
source("Functions/ts_grid.R")
#source("functions/impute_missing.R")


# # # #getting conductivity data ready
  loggerPBSF1 <- loggerPBSF %>%
    left_join(stage_PBSF, by = "date") %>%
    mutate(SC_orig = sp.cond) %>%
    mutate(sp.cond = ifelse(stage <= 2.11, NA, sp.cond)) %>%
    mutate(sp.cond = ifelse(is.na(stage), SC_orig, sp.cond)) %>%
    mutate(sp.cond = ifelse(sp.cond < 60, NA, sp.cond))
# # 
# # #
# # #
# #  #flag outliers using anomalize package
#  PBSF_outlier <- flagged_data(loggerPBSF1)
# #  #plot to inspect where to correct outliers
#   plot_flagged(PBSF_outlier)
# #  #after inspecting, filter and clean anomalies
#  PBSF_cleaned <- PBSF_outlier %>%
#     filter(Year_Month == "2020-11" |
#              Year_Month == "2020-4" |
#              Year_Month == "2020-12" |
#              Year_Month == "2021-1" |
#              Year_Month == "2020-8" |
#              Year_Month == "2020-5" |
#              Year_Month == "2020-6" & observed > 1750) %>%
#    #filter(Year_Month == "2020-6" & observed > 1750) %>%
#    clean_anomalies()
# 
# #  #insepect cleaned points
#   plot_cleaned(PBSF_cleaned)
# #  #final dataset with runningmean, trend, and corrected specific conductance data
#  PBSF_cond_data <- final_cond_data(loggerPBSF1, PBSF_cleaned, PBSF_outlier)
#   write_rds(PBSF_cond_data, "Data/HOBO_Loggers/PBSF/PBSF_cond_data.rds")

PBSF_cond_data <- read_rds("Data/HOBO_Loggers/PBSF/PBSF_cond_data.rds")
fieldcondPBSF <- fieldcondPBSF #conductivity measured in the field
labPBSF <- labPBSF #IC data 
 # PBSF_discharge <- rolling_ave_discharge(PBSF_cond_data, d.PBSF)
 # write_rds(PBSF_discharge, "Data/discharge/PBSF_discharge.rds")
PBSF_discharge <- read_rds("Data/discharge/PBSF_discharge.rds")

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

#discharge plot by logging data
PBSF_log_discharge <- PBSF_discharge %>%
  mutate(log_dis = log(runningmeandis, base =  10))
ggplot(PBSF_log_discharge) +
  geom_line(aes(date, log_dis))

#discharge plot with coord_cartesian
PBSF_discharge_plot_zoom <- PBSF_discharge_plot +
  coord_cartesian(ylim = c(0, 0.25))
splot("discharge_time_series/", "PBSF_zoomed")

#plot using ggforce
PBSF_discharge_plot + facet_zoom(ylim = c(0, 0.25))
splot("discharge_time_series/", "PBSF_final")

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
PBSF_linreg_plot <- linreg(labPBSF, PBSF_cond_data) + labs(title = "Pheasant Branch South Fork")
  #captlm('Pheasant Branch South Fork',"Pheasant Branch South Fork", labPBSF, PBSF_cond_data)
splot("cl_cond_linear_regression/", "PBSF")

eval(labPBSF, PBSF_cond_data)

#conductivity time series with chloride points overlain
sccl(PBSF_cond_data, labPBSF)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondPBSF, PBSF_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclPBSF, labPBSF)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, PBSF_discharge, PBSF_cond_data, labPBSF)
ggsave("Plots/TS_Grids/PBSF.png", height = 12, width = 16)


##################
stage_PBSF <- stage_PBSF %>%
  select(date, stage)

ggplot(stage_PBSF) + geom_point(aes(date, stage)) + geom_point(aes(date, nodata), color = "red") + geom_hline(yintercept = 2.11, color = "blue")

ggplot(loggerPBSF1) + geom_point(aes(date, sp.cond))



