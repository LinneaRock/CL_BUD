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
ggplot(loggerYI, aes(date, sp.cond)) + geom_point()

loggerYI1 <- loggerYI %>%
  filter(sp.cond > 200) #deletes a couple measurements that were collected while the logger was outside of the water


YI_cond_data <- outlier_detect_remove(loggerYI1, "YI")
fieldcondYI <- fieldcondYI #conductivity measured in the field
labYI <- labYI #IC data 
  # YI_discharge <- rolling_ave_discharge(YI_cond_data, d.YI)
  # write_rds(YI_discharge, "Data/discharge/YI_discharge.rds")
YI_discharge <- read_rds("Data/discharge/YI_discharge.rds")

#Conductivity time series
YI_cond_plot <- cond(YI_cond_data) +
  capt_scseries("Yahara River @ Main St", "Yahara River at Main St")
splot("conductance_time_series/", "YI")

#Chloride time series
YI_cl_plot <- clseries(labYI) +
  capt_clseries("Yahara River @ Main St", "Yahara River at Main St")
splot("chloride_time_series/", "YI")

#Discharge time series
YI_discharge_plot <- discharge_ts(YI_discharge)
splot("discharge_time_series/", "YI")

#cQ - conductivity
q.sc(YI_cond_data, YI_discharge) +
  captqec('Yahara River @ Main St',"Yahara River at Main St", YI_cond_data, YI_discharge)
splot("QC_plots/", "YI_cond")

evalqec(YI_cond_data, YI_discharge)

#cQ - chloride
q.cl(labYI, YI_discharge) +
  captqc('Yahara River @ Main St',"Yahara River at Main St", labYI, YI_discharge)
splot("QC_plots/", "YI_cl")

evalq(labYI, YI_discharge)

#Linear Regression between Conductivity and Chloride
YI_linreg_plot <- linreg(labYI, fieldcondYI, YI_cond_data) #+ labs(title = "YI")
  #captlm('Yahara River @ Main St',"Yahara River at Main St", labYI, YI_cond_data)
splot("cl_cond_linear_regression/", "YI")

jpeg('Plots/cl_cond_linear_regression/residual_plots/Yahara_Isthmus.png',width = 6.25, height = 4.25, units = 'in', res = 300)
eval(labYI, fieldcondYI, YI_cond_data)
dev.off()

#conductivity time series with chloride points overlain
sccl(YI_cond_data, labYI)

#Comparing conductivity collected with handheld meter and HOBO collected
cond_compare(fieldcondYI, YI_cond_data)

#Comparing chloride concentrations collected with YSI and lab analyzed 
cl_compare(fieldclYI, labYI)



#plotting a grid of timeseries data
ts_grid(precip_data, YI_discharge, YI_cond_data, labYI)
ggsave("Plots/TS_Grids/YI.png", height = 20, width = 15, units = "cm")

#number is the ratio of chloride to conductivity 
for_gridYI <- sc_cl(YI_cond_data, labYI, 10) + 
  labs(caption = "Figure 26. Precipitation, discharge, and specific conductivity and chloride concentrations 
collected during the study period in the Yahara River on the isthmus.")

ts_grid2(precip_data, YI_discharge, YI_cond_data, for_gridYI)
ggsave("Plots/TS_Grids/YI_2.png", height = 7.25, width = 6.25, units = "in")

