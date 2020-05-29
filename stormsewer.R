library(tidyverse)
library(ggpubr)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/clseries.R")
source("Functions/linreg.R")
source("Functions/splot.R")

#Willow Creek conductivity time series

cond(upstream)
splot("conductance_time_series/", "WC - Upstream")

cond(downstream)
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
SH.dishcharge <- d.sc.SH %>%
  na.omit() %>%
  mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
  group_by(day) %>%
  summarise(sp.cond = mean(sp.cond)) %>%
  rename(date = day)

linreg(labSH, SH.dishcharge)  
splot("cl_cond_linear_regression/", "SH")


