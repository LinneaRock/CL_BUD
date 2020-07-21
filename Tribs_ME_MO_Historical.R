library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(broom)
source("Functions/splot.R")
source("Functions/histlinreg.R")
source("Functions/clseries.R")
source("Functions/cond.R")
source("Functions/L_theme.R")

#function to read in data for chloride
hist <- function(X) {
  lab <- read_xlsx("Data/Historical_External/PHMDC.xlsx", sheet = X) %>%
    mutate(chloride_mgL = as.numeric(chloride_mgL)) %>%
    mutate(year = year(date))
  
}

#read in lake data
HistME <- hist("Mendota") %>%
  mutate(lakeid = "ME")
HistMO <- hist("Monona")  %>%
  mutate(lakeid = "MO")

#read in tributary data
HistMarsh <- hist("1918 Marsh")
HistDC <- hist("Dorn Creek")
HistPB <- hist("Pheasant Branch Creek")
Hist6MC <- hist("Six Mile Creek")
HistUB <- hist("U Bay Creek") 
HistWI <- hist("Wingra") 
HistYR <- hist("Yahara River")


#This is the plot I am trying to make, something turns out horribly wrong though...
ggplot(HistYR, aes(date, chloride_mgL)) +
  geom_point() +
  facet_wrap(~year, scales = "free_x") + 
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) +
  L_theme()



#linear regressions of chloride vs conductivity 

histlinreg(HistMarsh) +
  capthlm("1918 Marsh", "1918 Marsh", HistMarsh)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "1918 Marsh_cl")

histlinreg(HistDC) +
  capthlm("Dorn Creek", "Dorn Creek", HistDC)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "DC_cl")

histlinreg(HistPB) +
  capthlm("Pheasant Branch Creek", "Pheasant Branch Creek", HistPB)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "PB_cl")

histlinreg(Hist6MC) +
  capthlm("Sixmile Creek", "Sixmile Creek", Hist6MC)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "6MC_cl")

histlinreg(HistUB) +
  capthlm("University Bay Creek", "University Bay Creek", HistUB)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "UB_cl")

histlinreg(HistWI) + 
  capthlm("Wingra Creek", "Wingra Creek", HistWI)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "WI_cl")

histlinreg(HistYR) +
  capthlm("Yahara River", "Yahara River", HistYR)
splot("Historical_Data_Viz/cl_cond_linear_regression/", "YR_cl")



#time series of chloride concentrations

clseries(HistMarsh)  +
  capt_clseries("1918 Marsh", "1918 Marsh")
splot("Historical_Data_Viz/cl_time_series/", "1918 Marsh")

clseries(HistDC) +
  capt_clseries("Dorn Creek", "Dorn Creek")
splot("Historical_Data_Viz/cl_time_series/", "DC")

clseries(HistPB) +
  capt_clseries("Pheasant Branch Creek", "Pheasant Branch Creek")
splot("Historical_Data_Viz/cl_time_series/", "PB")

clseries(Hist6MC) +
  capt_clseries("Sixmile Creek", "Sixmile Creek")
splot("Historical_Data_Viz/cl_time_series/", "6MC")

clseries(HistUB) +
  capt_clseries("Univeristy Bay Creek", "University Bay Creek")
splot("Historical_Data_Viz/cl_time_series/", "UB")

clseries(HistWI) +
  capt_clseries("Wingra Creek", "Wingra Creek")
splot("Historical_Data_Viz/cl_time_series/", "WI")

clseries(HistYR) +
  capt_clseries("Yahara River", "Yahara River")
splot("Historical_Data_Viz/cl_time_series/", "YR")


#conductivity time series

hist_cond(HistMarsh) +
  capt_scseries("1918 Marsh", "1918 Marsh")
splot("Historical_Data_Viz/cond_time_series/", "1918 Marsh")

hist_cond(HistDC) +
  capt_scseries("Dorn Creek", "Dorn Creek")
splot("Historical_Data_Viz/cond_time_series/", "DC")

hist_cond(HistPB) +
  capt_scseries("Pheasant Branch Creek", "Pheasant Branch Creek")
splot("Historical_Data_Viz/cond_time_series/", "PB")

hist_cond(Hist6MC) +
  capt_scseries("Sixmile Creek", "Sixmile Creek")
splot("Historical_Data_Viz/cond_time_series/", "6MC")

hist_cond(HistUB) +
  capt_scseries("Univeristy Bay Creek", "University Bay Creek")
splot("Historical_Data_Viz/cond_time_series/", "UB")

hist_cond(HistWI) +
  capt_scseries("Wingra Creek", "Wingra Creek")
splot("Historical_Data_Viz/cond_time_series/", "WI")

hist_cond(HistYR) +
  capt_scseries("Yahara River", "Yahara River")
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
       ) + L_theme() +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29"))

splot("Historical_Data_Viz/", "ME_MO")
  

