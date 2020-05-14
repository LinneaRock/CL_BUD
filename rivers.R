library(tidyverse)
library(lubridate)
library(ggpubr)
library(patchwork)
source("Functions/linreg.R")
source("Functions/splot.R")
source("Functions/cond.R")
source("Functions/clseries.R")
source("Functions/sccl.R")
source("Functions/cl_compare.R")
source("Functions/cond_compare.R")


#Linear Regressions between Conductivity and Chloride

linreg(loggerYN, labYN)
splot("cl_cond_linear_regression/", "YN")

linreg(loggerYI, labYI)
splot("cl_cond_linear_regression/", "YI")

linreg(loggerYS, labYS)
splot("cl_cond_linear_regression/", "YS")

linreg(loggerSW, labSW)
splot("cl_cond_linear_regression/", "SW")

linreg(logger6MC, lab6MC)
splot("cl_cond_linear_regression/", "6MC")

linreg(loggerDC, labDC)
splot("cl_cond_linear_regression/", "DC")

linreg(loggerPBMS, labPBMS)
splot("cl_cond_linear_regression/", "PBMS")

#Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30 and I am having trouble finding a better solution
a <- loggerPBSF %>%
  mutate(date = round_date(date, "30 minutes")) 

linreg(a, labPBSF)
splot("cl_cond_linear_regression/", "PBSF")

#######################################################################

#Conductivity time series

cond(loggerYN)
splot("conductance_time_series/", "YN")

cond(loggerYI)
splot("conductance_time_series/", "YI")

cond(loggerYS)
splot("conductance_time_series/", "YS")

cond(loggerSW)
splot("conductance_time_series/", "SW")

cond(logger6MC)
splot("conductance_time_series/", "6MC")

cond(loggerDC)
splot("conductance_time_series/", "DC")

cond(loggerPBMS)
splot("conductance_time_series/", "PBMS")

cond(loggerPBSF)
splot("conductance_time_series/", "PBSF")


#######################################################################

#Chloride concentration time series 

clseries(labYN)
splot("chloride_time_series/", "YN")

clseries(labYI)
splot("chloride_time_series/", "YI")

clseries(labYS)
splot("chloride_time_series/", "YS")

clseries(labSW)
splot("chloride_time_series/", "SW")

clseries(lab6MC)
splot("chloride_time_series/", "6MC")

clseries(labDC)
splot("chloride_time_series/", "DC")

clseries(labPBMS)
splot("chloride_time_series/", "PBMS")

clseries(labPBSF)
splot("chloride_time_series/", "PBSF")


#######################################################################

#conductivity time series with chloride points overlain

sccl(loggerYN, labYN)
sccl(loggerYI, labYI)
sccl(loggerYS, labYS) 
sccl(loggerSW, labSW)
sccl(logger6MC, lab6MC)
sccl(loggerDC, labDC)
sccl(loggerPBMS, labPBMS)
sccl(loggerPBSF, labPBSF)


#######################################################################

#Comparing chloride concentrations collected with YSI and lab analyzed 

cl_compare(fieldclYN, labYN)
cl_compare(fieldclYI, labYI)
cl_compare(fieldclYS, labYS)
cl_compare(fieldclSW, labSW)
cl_compare(fieldcl6MC, lab6MC)
cl_compare(fieldclDC, labDC)
cl_compare(fieldclPBMS, labPBMS)
cl_compare(fieldclPBSF, labPBSF)

#######################################################################

#Comparing conductivity collected with handheld meter and HOBO collected

cond_compare(fieldcondYN, loggerYN)
cond_compare(fieldcondYI, loggerYI)
cond_compare(fieldcondYS, loggerYS)
cond_compare(fieldcondSW, loggerSW)
cond_compare(fieldcond6MC, logger6MC)
cond_compare(fieldcondDC, loggerDC)
cond_compare(fieldcondPBMS, loggerPBMS)
cond_compare(fieldcondPBSF, loggerPBSF)
