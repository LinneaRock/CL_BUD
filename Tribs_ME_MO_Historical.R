library(tidyverse)
library(readxl)
library(lubridate)
source("Functions/splot.R")
source("Functions/histlinreg.R")
source("Functions/clseries.R")
source("Functions/cond.R")

#function to read in data for chloride
hist <- function(X) {
  lab <- read_xlsx("Data/Historical_External/PHMDC.xlsx", sheet = X) %>%
    mutate(chloride_mgL = as.numeric(chloride_mgL))
  
}

HistME <- hist("Mendota") %>%
  mutate(lakeid = "ME")
HistMO <- hist("Monona")  %>%
  mutate(lakeid = "MO")
HistMarsh <- hist("1918 Marsh")
HistDC <- hist("Dorn Creek")
HistPB <- hist("Pheasant Branch Creek")
Hist6MC <- hist("Six Mile Creek")
HistUB <- hist("U Bay Creek")
HistWI <- hist("Wingra")
HistYR <- hist("Yahara River")


#linear regressions of chloride vs conductivity 

histlinreg(HistMarsh)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "1918 Marsh_cl")

histlinreg(HistDC)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "DC_cl")

histlinreg(HistPB)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "PB_cl")

histlinreg(Hist6MC)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "6MC_cl")

histlinreg(HistUB)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "UB_cl")

histlinreg(HistWI)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "WI_cl")

histlinreg(HistYR)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "YR_cl")


#time series of chloride concentrations

clseries(HistMarsh)
splot("Historical_Data_Viz/cl_time_series/", "1918 Marsh")

clseries(HistDC)
splot("Historical_Data_Viz/cl_time_series/", "DC")

clseries(HistPB)
splot("Historical_Data_Viz/cl_time_series/", "PB")

clseries(Hist6MC)
splot("Historical_Data_Viz/cl_time_series/", "6MC")

clseries(HistUB)
splot("Historical_Data_Viz/cl_time_series/", "UB")

clseries(HistWI)
splot("Historical_Data_Viz/cl_time_series/", "WI")

clseries(HistYR)
splot("Historical_Data_Viz/cl_time_series/", "YR")


#conductivity time series

cond(HistMarsh)
splot("Historical_Data_Viz/cond_time_series/", "1918 Marsh")

cond(HistDC)
splot("Historical_Data_Viz/cond_time_series/", "DC")

cond(HistPB)
splot("Historical_Data_Viz/cond_time_series/", "PB")

cond(Hist6MC)
splot("Historical_Data_Viz/cond_time_series/", "6MC")

cond(HistUB)
splot("Historical_Data_Viz/cond_time_series/", "UB")

cond(HistWI)
splot("Historical_Data_Viz/cond_time_series/", "WI")

cond(HistYR)
splot("Historical_Data_Viz/cond_time_series/", "YR")



#figure of Mendota/Monona historical data
data = rbind(HistME, HistMO)

ggplot(data, aes(date, chloride_mgL)) +
  geom_point(aes(color = lakeid)) +
  geom_smooth(aes(group = lakeid, color = lakeid), se = FALSE) +
  labs(y = "Chloride Concentration"~(mg~L^-1)~"\n",
       x = "" #,
      # caption = "Figure 3 The long-term increasing chloride concentration trend in the 
#upper Yahara River watershed lakes (Public Health Madison Dane County, 2020)."
       ) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size =11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 11, hjust = 0)) +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29"))

splot("Historical_Data_Viz/", "ME_MO")
  

