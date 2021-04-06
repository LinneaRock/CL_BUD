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

 loggerYS1 <- loggerYS %>%
   mutate(sp.cond = ifelse(date > as.POSIXct('2021-02-15 08:00:00', tz = "ETC/GMT-7") & date < as.POSIXct('2021-02-20 20:00:00', tz = "ETC/GMT-7"), NA, sp.cond)) %>% #logger was encased in ice during this part of the month
   mutate(sp.cond = ifelse(sp.cond > 800 | sp.cond <500, NA, sp.cond)) #very obvious extreme outliers in this dataset that will skew the running mean if not removed


YS_cond_data <- outlier_detect_remove(loggerYS1, "YS")
fieldcondYS <- fieldcondYS #conductivity measured in the field
labYS <- labYS #IC data 
YS_discharge <- read.csv("Data/Monona_Outlet_Data/d_YS.csv") %>%
  mutate(date = ymd_hms(date))
YS_discharge <- rolling_ave_discharge(YS_cond_data, YS_discharge)


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
splot("discharge_time_series/", "YS")

#cQ - conductivity
q.sc(YS_cond_data, YS_discharge)+
  captqec('Yahara River @ Broadway',"Yahara River at Broadway St", YS_cond_data, YS_discharge)
splot("QC_plots/", "YS_cond")

evalqec(YS_cond_data, YS_discharge)

#cQ - chloride
q.cl(labYS, YS_discharge) +
  captqc('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_discharge)
splot("QC_plots/", "YS_cl")

evalq(labYS, YS_discharge)
  
#Linear Regression between Conductivity and Chloride
YS_linreg_plot <- linreg(labYS, fieldcondYS, YS_cond_data) + labs(title = "Yahara South")
  #captlm('Yahara River @ Broadway',"Yahara River at Broadway St", labYS, YS_cond_data)
splot("cl_cond_linear_regression/", "YS")

eval(labYS, YS_cond_data)

#conductivity time series with chloride points overlain
sccl(YS_cond_data, labYS)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYS, YS_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYS, labYS)



#plotting a grid of timeseries data
ts_grid(precip_data, YS_discharge, YS_cond_data, labYS)
ggsave("Plots/TS_Grids/YS.png", height = 20, width = 15, units = "cm")

