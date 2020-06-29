library(tidyverse)
library(lubridate)
library(ggpubr)
library(patchwork)
library(broom)
source("Functions/linreg.R")
source("Functions/splot.R")
source("Functions/cond.R")
source("Functions/clseries.R")
source("Functions/sccl.R")
source("Functions/cl_compare.R")
source("Functions/cond_compare.R")
source("Functions/histlinreg.R")



#Linear Regressions between Conductivity and Chloride

linreg(loggerYN, labYN) +
  captlm("Yahara River @ 113", "Yahara River at Highway 113", loggerYN, labYN) #from Functions/linreg.R
splot("cl_cond_linear_regression/", "YN")


linreg(loggerYI, labYI) + 
  captlm('Yahara River @ Main St.',"Yahara River at E. Main St", loggerYI, labYI)
splot("cl_cond_linear_regression/", "YI")


linreg(loggerYS, labYS) +
  captlm('Yahara River @ Broadway',"Yahara River at Broadway St", loggerYS, labYS)
splot("cl_cond_linear_regression/", "YS")


linreg(loggerSW, labSW) +
  captlm('Starkweather Creek @ Olbrich',"Starkweather Creek at Olbrich Garden", loggerSW, labSW)
splot("cl_cond_linear_regression/", "SW")


linreg(logger6MC, lab6MC) +
  captlm('Sixmile Creek @ M',"Sixmile Creek at Highway M", logger6MC, lab6MC)
splot("cl_cond_linear_regression/", "6MC")


linreg(loggerDC, labDC) +
  captlm('Dorn Creek @ M',"Dorn Creek at Highway M", loggerDC, labDC)
splot("cl_cond_linear_regression/", "DC")


linreg(loggerPBMS, labPBMS) +
  captlm('Pheasant Branch Main Stem',"Main Stem of Pheasant Branch Creek", loggerPBMS, labPBMS)
splot("cl_cond_linear_regression/", "PBMS")


#Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30 and I am having trouble finding a better solution
a <- loggerPBSF %>%
  mutate(date = round_date(date, "30 minutes")) 

linreg(a, labPBSF) +
  captlm('Pheasant Branch S.Fork',"South Fork of Pheasant Branch", a, labPBSF)
splot("cl_cond_linear_regression/", "PBSF")


#######################################################################

#Conductivity time series

cond(loggerYN) +
  capt_scseries("Yahara North", "Yahara River at Highway 113")
splot("conductance_time_series/", "YN")

cond(loggerYI) +
  capt_scseries("Yahara - Isthmus", "Yahara River at E. Main St")
splot("conductance_time_series/", "YI")

cond(loggerYI %>% filter(sp.cond < 750))

cond(loggerYS) +
  capt_scseries("Yahara South", "Yahara River at Broadway St")
splot("conductance_time_series/", "YS")

cond(loggerYS %>% filter(sp.cond < 750))

cond(loggerSW) +
  capt_scseries("Starkweather Creek", "Starkweather Creek at Olbrich Garden")
splot("conductance_time_series/", "SW")

cond(logger6MC) +
  capt_scseries("Sixmile Creek", "Sixmile Creek at Highway M")
splot("conductance_time_series/", "6MC")

cond(loggerDC) +
  capt_scseries("Dorn Creek", "Dorn Creek at Highway M")
splot("conductance_time_series/", "DC")

cond(loggerPBMS) +
  capt_scseries("Pheasant Branch Creek - Main Stem", "main stem of Pheasant Branch Creek")
splot("conductance_time_series/", "PBMS")

cond(loggerPBSF) +
  capt_scseries("Pheasant Branch Creek - South Fork", "south fork of Pheasant Branch Creek")
splot("conductance_time_series/", "PBSF")


#######################################################################

#Chloride concentration time series 

clseries(labYN %>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Yahara North", "Yahara River at Highway 113")
splot("chloride_time_series/", "YN")

clseries(labYI%>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Yahara - Isthmus", "Yahara River at E. Main St")
splot("chloride_time_series/", "YI")

clseries(labYS%>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Yahara South", "Yahara River at Broadway St")
splot("chloride_time_series/", "YS")

clseries(labSW%>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Starkweather Creek", "Starkweather Creek at Olbrich Garden")
splot("chloride_time_series/", "SW")

clseries(lab6MC %>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Sixmile Creek", "Sixmile Creek at Highway M")
splot("chloride_time_series/", "6MC")

clseries(labDC %>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Dorn Creek", "Dorn Creek at Highway M")
splot("chloride_time_series/", "DC")

clseries(labPBMS %>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Pheasant Branch Creek - Main Stem", "main stem of Pheasant Branch Creek")
splot("chloride_time_series/", "PBMS")

clseries(labPBSF %>%
           filter(datetime_collected <= "2020-02-01 00:00:00")) +
  capt_clseries("Pheasant Branch Creek - South Fork", "south fork of Pheasant Branch Creek")
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

#####################################################################

#watershed linear regression
YRW_cond <- rbind(loggerYN, loggerYI, loggerYS, loggerSW, logger6MC, loggerDC, loggerPBMS, a) #combining all conductivity datasets into one master dataset
YRW_cl <- rbind(labYN, labYI, labYS, labSW, lab6MC, labDC, labPBMS, labPBSF) #combining all chloride datasets into one master dataset

YRW <- left_join(YRW_cond, YRW_cl, by = c("date", "ID")) #joining the chloride and conductivity datasets by date and location so each chloride concentration matches up with the appropriate sp. conductivity concentration

#Plotting the entire watershed! I am using the functions from histlinreg.R because of the difference in functions using one or multiple datasets (I cannot figure out how to write the code to join multiple datasets like in readSP.R)
histlinreg(YRW %>%
             filter(sp.cond <= 3000)) +
  capthlm("Upper Yahara River Watershed", "study area sampling locations", YRW)
splot("cl_cond_linear_regression/", "YRW")


