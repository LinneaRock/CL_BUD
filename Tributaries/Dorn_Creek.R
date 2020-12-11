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
source("functions/ts_grid.R")

#calling and naming raw data
loggerDC1 <- loggerDC %>% #HOBO conductivity data
  mutate(sp.cond = ifelse(date >= "2020-09-22 17:00:00" & date <= "2020-10-06 10:30:00", NA, sp.cond)) # logger was removed from water during this period
#creating dataset to perform imputations on missing data
to_imp <- loggerDC1 %>%
  select(date, sp.cond) %>%
  rename(imputed = sp.cond)
#imputing missing data and converting back to a dataframe
loggerDC2 <- to_imp %>%  
  as.ts() %>%
  na_ma(6, "exponential") %>%
  as.data.frame() %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00", tz = "GMT")) %>%
  left_join(loggerDC1, by = "date") %>%
  mutate(imputed = ifelse(is.na(sp.cond), imputed, NA)) %>%
  mutate(sp.cond = ifelse(is.na(sp.cond), imputed, sp.cond))


fieldcondDC <- fieldcondDC #conductivity measured in the field
labDC <- labDC #IC data 
DC_discharge <- rolling_ave_discharge(loggerDC2, d.DC)
  

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
DC_cond_data <- find_outlier(loggerDC2, fieldcondDC, "DCoutliers", "DCoutliers_month")

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
DC_linreg_plot <- linreg(labDC, DC_cond_data) +
  captlm('Dorn Creek',"Dorn Creek at Highway M", labDC, DC_cond_data)
splot("cl_cond_linear_regression/", "DC")

eval(labDC, DC_cond_data)

#conductivity time series with chloride points overlain
sccl(DC_cond_data, labDC)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondDC, loggerDC)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclDC, labDC)



#plotting a grid of timeseries data
ts_grid(precip_temp_data, DC_discharge, DC_cond_data, labDC)
ggsave("Plots/TS_Grids/DC.png", height = 12, width = 16)

