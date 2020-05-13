library(tidyverse)
library(lubridate)
library(ggpubr)
source("Functions/cond.R")

#plotting Mendota time series for 2019-2020

lakecond(loggerME_Epi, loggerME_Hypo, "Mendota 24m", "Mendota 1.5m")
splot("conductance_time_series/", "ME")




