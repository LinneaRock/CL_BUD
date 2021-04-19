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

#check raw data
ggplot(loggerYN, aes(date, sp.cond)) + geom_point()
#ggplot(loggerYN %>% filter(date > as.POSIXct("2021-02-19 00:00:00", tz = "ETC/GMT-6") & date < as.POSIXct("2021-02-27 00:00:00", tz = "ETC/GMT-6"))) + geom_point(aes(date, sp.cond))

#YN_cond_problem <- loggerYN %>% filter(date > as.POSIXct("2021-02-19 00:00:00", tz = "ETC/GMT-6") & date < as.POSIXct("2021-02-27 00:00:00", tz = "ETC/GMT-6"))

loggerYN1 <- loggerYN %>%
  mutate(sp.cond = ifelse(date > as.POSIXct("2021-02-19 13:50:00", tz = "ETC/GMT-6") & date < as.POSIXct("2021-02-25 22:00:00", tz = "ETC/GMT-6"), NA, sp.cond))
  

YN_cond_data <- outlier_detect_remove(loggerYN1, "YN")
fieldcondYN <- fieldcondYN #conductivity measured in the field
labYN <- labYN #IC data 
   # YN_discharge <- rolling_ave_discharge(YN_cond_data, d.YN)
   # write_rds(YN_discharge, "Data/discharge/YN_discharge.rds")
YN_discharge <- read_rds("Data/discharge/YN_discharge.rds")

#Conductivity time series
YN_cond_plot <- cond(YN_cond_data) +
  capt_scseries("Yahara River @ 113", "Yahara River at Highway 113")
splot("conductance_time_series/", "YN")

#Chloride time series
YN_cl_plot <- clseries(labYN) +
  capt_clseries("Yahara River @ 113", "Yahara River at Highway 113")
splot("chloride_time_series/", "YN")

#Discharge time series
YN_discharge_plot <- discharge_ts(YN_discharge)
splot("discharge_time_series/", "YN")

#cQ - conductivity
q.sc(YN_cond_data, YN_discharge)+
  captqec('Yahara River @ 113',"Yahara River at Highway 113", YN_cond_data, YN_discharge)
splot("QC_plots/", "YN_cond")

evalqec(YN_cond_data, YN_discharge)

#cQ - chloride
q.cl(labYN, YN_discharge) +
  captqc('Yahara River @ 113',"Yahara River at Highway 113", labYN, YN_discharge)
splot("QC_plots/", "YN_cl")

evalq(labYN, YN_discharge)

#Linear Regression between Conductivity and Chloride
YN_linreg_plot <- linreg(labYN, fieldcondYN, YN_cond_data) + labs(title = "YN")
  #captlm('Yahara River @ 113',"Yahara River at Highway 113", labYN, YN_cond_data)
splot("cl_cond_linear_regression/", "YN")

eval(labYN, fieldcondYN, YN_cond_data)

#conductivity time series with chloride points overlain
sccl(YN_cond_data, labYN)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYN, YN_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYN, labYN)


#plotting a grid of timeseries data
ts_grid(precip_data, YN_discharge, YN_cond_data, labYN)
ggsave("Plots/TS_Grids/YN.png", height = 20, width = 15, units = "cm")




