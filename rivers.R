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


#Linear Regressions between Conductivity and Chloride

linreg(loggerYN, labYN)
splot("cl_cond_linear_regression/", "YN")
info(loggerYN, labYN)
eval(loggerYN, labYN)

glance(info(loggerYN, labYN))$p.value
glance(info(loggerYN, labYN))$r.squared
coef(info(loggerYN, labYN))[1,1] #get intercept
coef(info(loggerYN, labYN))[2,1] #get slope

linreg(loggerYI, labYI)
splot("cl_cond_linear_regression/", "YI")
info(loggerYI, labYI)
eval(loggerYI, labYI)

linreg(loggerYS, labYS)
splot("cl_cond_linear_regression/", "YS")
info(loggerYS, labYS)
eval(loggerYS, labYS)

linreg(loggerSW, labSW)
splot("cl_cond_linear_regression/", "SW")
info(loggerSW, labSW)
eval(loggerSW,labSW)

linreg(logger6MC, lab6MC)
splot("cl_cond_linear_regression/", "6MC")
info(logger6MC, lab6MC)
eval(logger6MC, lab6MC)

linreg(loggerDC, labDC)
splot("cl_cond_linear_regression/", "DC")
info(loggerDC, labDC)
eval(loggerDC, labDC)

linreg(loggerPBMS, labPBMS)
splot("cl_cond_linear_regression/", "PBMS")
info(loggerPBMS, labPBMS)
eval(loggerPBMS, labPBMS)

#Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30 and I am having trouble finding a better solution
a <- loggerPBSF %>%
  mutate(date = round_date(date, "30 minutes")) 

linreg(a, labPBSF)
splot("cl_cond_linear_regression/", "PBSF")
info(a, labPBSF)
eval(a, labPBSF)

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

#####################################################################

#watershed linear regression
YRW_cond <- rbind(loggerYN, loggerYI, loggerYS, loggerSW, logger6MC, loggerDC, loggerPBMS, a)
YRW_cl <- rbind(labYN, labYI, labYS, labSW, lab6MC, labDC, labPBMS, labPBSF)

d <- left_join(YRW_cond, YRW_cl, by = c("date", "ID"))
ggplot(d, aes(chloride_mgL, sp.cond)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "#7496D2") +
  labs(y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       x = "\nChloride Concentration"~(mg~L^-1)) +
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

#extracting p-value and r-squared from simple linear regression
info <- lm(chloride_mgL ~ sp.cond, d)
glance(info)$p.value
glance(info)$r.squared
summary(info)

#evaluating fit of model
layout(matrix(1:4,2,2))
plot(info)

e <- d %>%
  filter(chloride_mgL != "NA") %>%
  select(sp.cond, chloride_mgL)

