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

#checking raw data
ggplot(loggerSW, aes(date, sp.cond)) + geom_point()

loggerSW1 <- loggerSW %>%
  mutate(sp.cond = ifelse(date > as.POSIXct('2021-01-28 09:00:00', tz = "ETC/GMT-7") & date < as.POSIXct('2021-01-31 20:00:00', tz = "ETC/GMT-7"), NA, sp.cond)) %>% #logger was encased in ice during this part of the month
  mutate(sp.cond = ifelse(date > as.POSIXct('2021-02-07 12:00:00', tz = "ETC/GMT-7") & date < as.POSIXct('2021-02-24 21:30:00', tz = "ETC/GMT-7"), NA, sp.cond)) %>% #logger was encased in ice during this part of the month
  mutate(sp.cond = ifelse(sp.cond < 0, NA, sp.cond))



SW_cond_data <- outlier_detect_remove(loggerSW1, "SW")
fieldcondSW <- fieldcondSW #conductivity measured in the field
labSW <- labSW #IC data 


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
SW_linreg_plot <- linreg(labSW, fieldcondSW, SW_cond_data)# + labs(title = "SW")
  #captlm('Starkweather Creek',"Starkweather Creek at Olbrich Garden", labSW, SW_cond_data)
splot("cl_cond_linear_regression/", "SW")

eval(labSW, SW_cond_data)

#conductivity time series with chloride points overlain
sccl(SW_cond_data, labSW)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondSW, SW_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclSW, labSW)



date = as.POSIXct(1:10, origin = "1970-01-01")
discharge = 1:10
discharge_placeholder <- as.data.frame(date) %>% mutate(runningmeandis = discharge)

#plotting a grid of timeseries data
ts_grid(precip_data, discharge_placeholder, SW_cond_data, labSW)
ggsave("Plots/TS_Grids/SW.png", height = 20, width = 15, units = "cm")
