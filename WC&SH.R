library(tidyverse)
library(ggpubr)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/clseries.R")
source("Functions/linreg.R")
source("Functions/splot.R")
source("Functions/qcl.R")
source("Functions/qsc.R")

#Willow Creek conductivity time series

cond(upstream) +
  capt_scseries("WC - Upstream", "upstream location at Willow Creek")
splot("conductance_time_series/", "WC - Upstream")

cond(downstream) +
  capt_scseries("WC - Downstream", "downstream location at Willow Creek")
splot("conductance_time_series/", "WC - Downstream")


#chloride concentrations of Spring Harbor from 2014-2016

clseries(labSH) 
splot("chloride_time_series/", "SH")

ggplot(labSH, aes(date, chloride_mgL)) +
  geom_line() +
  labs(y = "Chloride Concentration"~(mg~L^-1)~"\n",
       x = "") +
  theme(panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

splot("chloride_time_series/", "SH_alt")

#linear regression for chloride study of Spring Harbor in 2014-2016
#the chloride data is daily,with no collection time, so I am going to try using the average daily sp. conductivity
SH.cond <- d.sc.SH %>%
  na.omit() %>%
  mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
  group_by(day) %>%
  summarise(sp.cond = mean(sp.cond)) %>%
  rename(date = day)

linreg(labSH, SH.cond) +
  captlm("Spring Harbor", "Spring Harbor USGS gage", labSH, SH.cond)
splot("cl_cond_linear_regression/", "SH")

#same thing as above for q - chloride concentration regression.
SH.discharge <- d.sc.SH %>%
  na.omit() %>%
  mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
  group_by(day) %>%
  summarise(discharge = mean(discharge)) %>%
  rename(date = day)

q.cl(labSH, SH.discharge) +
  captqc("Spring Harbor", "Spring Harbor storm sewer", labSH, SH.discharge)
splot("QC_plots/", "SH")

#q - sp. conductivity regression 
NAS <- d.sc.SH %>% #creating a garbage dataset to enable use of the function (it needs 2 datasets as the arguments and I can't figure out how to make this optional)
  mutate(sp.cond = NA,
         discharge = NA) %>%
  rename(garbage = sp.cond,
         garbage2 = discharge)

q.sc(d.sc.SH, NAS) +
  captqec("Spring Harbor", "Spring Harbor storm sewer", d.sc.SH, NAS)
splot("QC_plots/", "SH_cond")




SH_cond_discharge <- cl_data_flow_sc %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(sp.cond = sp_cond,
         date = dateTime) %>%
  na.omit() %>%
  mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
  group_by(day) %>%
  summarise(discharge = mean(discharge),
            sp.cond = mean(sp.cond)) %>%
  rename(date = day) %>%
  left_join(labSH)

SH.lm <- lm(chloride_mgL ~ sp.cond, SH_cond_discharge)
summary(SH.lm)

SH_load <- SH_cond_discharge %>%
  mutate(daily_rate = chloride_mgL * discharge) %>%
  mutate(daily_load = daily_rate * 3600 * 24)








