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
source("Functions/discharge_ts.R")

#looking at raw data to find when logger was logging outside of the stream
# pbsf <- loggerPBSF %>%
#   mutate(Year_Month = paste(year(date), month(date), sep = "-"), #year month 
#          YMD = paste(year(date), month(date), day(date), sep = "-")) 
# ggplot(pbsf) +
#   geom_point(aes(date, Full.Range)) +
#   facet_wrap("Year_Month", scales = "free")
# 
# pbsf_dis <- PBSF_discharge %>%
#   mutate(Year_Month = paste(year(date), month(date), sep = "-"), #year month 
#          YMD = paste(year(date), month(date), day(date), sep = "-")) %>%
#   filter(date <= "2020-11-01 00:00:00")
# 
# ggplot(pbsf_dis) +
#   geom_point(aes(date, discharge)) +
#   facet_wrap("Year_Month", scales = "free")

#calling and naming raw data
loggerPBSF2 <- loggerPBSF %>%  #HOBO conductivity data - when water is low, sensor is not under water. Adding NAs where this occurs 
  mutate(sp.cond = ifelse(date >= "2020-04-18 17:30:00" & date <= "2020-04-22 18:00:00", NA, sp.cond)) %>%  
  mutate(sp.cond = ifelse(date >= "2020-05-05 21:30:00" & date <= "2020-05-13 21:00:00", NA, sp.cond)) %>%
  mutate(sp.cond = ifelse(date >= "2020-07-31 19:30:00" & date <= "2020-08-15 9:00:00", NA, sp.cond)) %>%
  mutate(sp.cond = ifelse(date >= "2020-08-18 17:00:00" & date <= "2020-08-26 12:30:00", NA, sp.cond))


fieldcondPBSF <- fieldcondPBSF #conductivity measured in the field
labPBSF <- labPBSF #IC data 
PBSF_discharge <- rolling_ave_discharge(loggerPBSF2, d.PBSF)

#Preparing conductivity data through rolling averages and removing outliers that greatly impact the data
#outlier figures automatically saved to plots folder
#use runningmean for analyses going forward
PBSF_cond_data <- find_outlier(loggerPBSF2, fieldcondPBSF, "PBSFoutliers", "PBSFoutliers_month")

#Conductivity time series
PBSF_cond_plot <- cond(PBSF_cond_data) +
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




