library(tidyverse)
library(lubridate)
library(data.table)
library(ggpubr)
library(patchwork)
library(zoo)
#library(imputeTS)
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
source("Functions/L_theme.R")
#source("functions/impute_missing.R")
source("functions/ts_grid.R")

# # #calling and naming raw data
   loggerDC1 <- loggerDC %>% #HOBO conductivity data
     mutate(sp.cond = as.numeric(sp.cond)) %>%
     mutate(sp.cond = ifelse(date >= as.POSIXct("2020-09-22 16:00:00", tz = "Etc/GMT-6") & date <= as.POSIXct("2020-10-06 9:30:00", tz = "Etc/GMT-6"), NA, sp.cond)) %>%
     na.omit()
# 
# # # #flag outliers using anomalize package
#  DC_outlier <- flagged_data(loggerDC1)
# # #plot to inspect where to correct outliers
#  plot_flagged(DC_outlier)
# # #after inspecting, filter and clean anomalies
#  DC_cleaned <- DC_outlier %>%
#    filter(Year_Month == "2020-1" |
#             Year_Month == "2020-10" |
#             Year_Month == "2020-2" |
#             Year_Month == "2020-3" |
#             Year_Month == "2020-4") %>%
#    clean_anomalies()
# # #insepect cleaned points
#  plot_cleaned(DC_cleaned)
# # #final dataset with runningmean, trend, and corrected specific conductance data
#  DC_cond_data <- final_cond_data(loggerDC1, DC_cleaned, DC_outlier)
#  write_rds(DC_cond_data, "Data/HOBO_Loggers/DC/DC_cond_data.rds")


DC_cond_data <- read_rds("Data/HOBO_Loggers/DC/DC_cond_data.rds")
fieldcondDC <- fieldcondDC #conductivity measured in the field
labDC <- labDC #IC data 
 # DC_discharge <- rolling_ave_discharge(DC_cond_data, d.DC)
 # write_rds(DC_discharge, "Data/discharge/DC_discharge.rds")
DC_discharge <- read_rds("Data/discharge/DC_discharge.rds")



#Conductivity time series
DC_cond_plot <- cond(DC_cond_data) +
  capt_scseries("Dorn Creek", "Dorn Creek at Highway M")
splot("conductance_time_series/", "DC")

#Chloride time series
DC_cl_plot <- clseries(labDC) +
  capt_clseries("Dorn Creek", "Dorn Creek at Highway M")
splot("chloride_time_series/", "DC")

#Discharge time series
DC_discharge_plot <- discharge_ts(DC_discharge) +
  labs(title = "Dorn Creek")
splot("discharge_time_series/", "DC")

#cQ - conductivity
q.sc(DC_cond_data, DC_discharge) +
  captqec('Dorn Creek',"Dorn Creek at Highway M", DC_cond_data, DC_discharge)
splot("QC_plots/", "DC_cond")

evalqec(DC_cond_data, DC_discharge)


#cQ - chloride
q.cl(labDC, DC_discharge) +
  captqc('Dorn Creek',"Dorn Creek at Highway M", labDC, DC_discharge)
splot("QC_plots/", "DC_cl")


evalq(labDC, DC_discharge)


#Linear Regression between Conductivity and Chloride
DC_linreg_plot <- linreg(labDC, DC_cond_data) + labs(title = "Dorn Creek")
  #captlm('Dorn Creek',"Dorn Creek at Highway M", labDC, DC_cond_data)
splot("cl_cond_linear_regression/", "DC")

eval(labDC, DC_cond_data)

#conductivity time series with chloride points overlain
sccl(DC_cond_data, labDC)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondDC, DC_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclDC, labDC)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, DC_discharge, DC_cond_data, labDC)
ggsave("Plots/TS_Grids/DC.png", height = 12, width = 16)

