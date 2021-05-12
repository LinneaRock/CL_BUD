library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpubr)
library(zoo)
library(viridisLite)
source("Functions/cond.R")
source("Functions/splot.R")
source("Functions/outlier_detect_remove.R")
source("Functions/cond_compare.R")
source("Functions/clseries.R")





MO_Epi_cond_plot <- cond(MO_Epi_cond_data) +
  capt_scseries("Lake Monona", "~2m below the surface in Lake Monona")
splot("conductance_time_series/", "MO_Epi")
MO_Hypo_cond_plot <- cond(MO_Hypo_cond_data) +
  capt_scseries("Lake Monona", "1m off the bottom of Lake Monona")
splot("conductance_time_series/", "MO_Hypo")


cond_compare(fieldcondME %>% filter(depth == 24.0), loggerME_Hypo)
cond_compare(fieldcondME %>% filter(depth == 1.5), loggerME_Epi)
cond_compare(fieldcondMO %>% filter(depth == 20), loggerMO_Hypo)
cond_compare(fieldcondMO %>% filter(depth == 1.5), loggerMO_Epi)



ME_chloride_plot <- clseries(labME) + geom_point(aes(color = Depth_m)) + scale_color_viridis_c(direction = -1) +
  capt_clseries("Mendota", "Lake Mendota")
splot("chloride_time_series/", "ME")





ggplot(labME, aes(Depth_m, chloride_mgL, group = Depth_m)) +
  geom_boxplot() 




##All chloride data####
#function to read in data for chloride from PHMDC
hist <- function(X) {
  lab <- read_xlsx("Data/Historical_External/PHMDC.xlsx", sheet = X) %>%
    mutate(chloride_mgL = as.numeric(chloride_mgL)) %>%
    mutate(year = year(date)) %>% 
    mutate(fakeDate = as.Date(paste(3000, format.POSIXct(date, "%m-%d"), sep = "-")))
  
}

#read in lake data
HistME <- hist("Mendota") %>%
  mutate(lakeid = "ME")
HistMO <- hist("Monona")  %>%
  mutate(lakeid = "MO")



#read in data for chloride from LTER
LTER <- read.csv("Data/LTER_ions.csv") %>%
  mutate(date = as.Date(as.character(sampledate))) %>%
  filter(lakeid == "ME" | lakeid == "MO" ) %>%
  rename(chloride_mgL = cl) %>%
  select(lakeid, date, depth, chloride_mgL)


#join datasets together
all_ME <- labME %>%
  bind_rows(LTER %>% filter(lakeid == "ME")) %>%
  bind_rows(HistME) %>%
  drop_na(chloride_mgL) %>%
  filter(chloride_mgL > 0) %>%
  mutate(lakeid = "ME")

all_MO <- labMO %>%
  bind_rows(LTER %>% filter(lakeid == "MO")) %>%
  bind_rows(HistMO) %>%
  drop_na(chloride_mgL) %>%
  filter(chloride_mgL > 0) %>%
  mutate(lakeid = "MO")

data <- rbind(all_ME, all_MO) %>%
  filter(chloride_mgL < 125)

ggplot(data, aes(date, chloride_mgL)) +
  geom_point(aes(color = lakeid)) +
  geom_smooth(aes(group = lakeid, color = lakeid), se = FALSE) +
  labs(y = "Chloride Concentration"~(mg~L^-1)~"\n",
       x = "" #,
       # caption = "Figure 3 The long-term increasing chloride concentration trend in the 
       #upper Yahara River watershed lakes (Public Health Madison Dane County, 2020)."
  ) + L_theme() +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29")) +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size =9)) +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29")) +
  plot_annotation(caption = "Data from PHMDC, LTER, and collected by Linnea Rock. Figure by Linnea Rock.")
ggsave("Plots/chloride_time_series/ME_MO_alldata.png")


#Calculating residence time
ME_vol <- 506880000 #volume in [m^3]
MO_vol <- 111520000 #volume in [m^3]


format_discharge <- function(d) {
    d %>%
    filter(year(date) == 2019) %>%
    group_by(yday(date)) %>%
    summarise(meandaily = mean(discharge * 3600 *24)) %>% 
    ungroup() %>%
    summarise(annual = sum(meandaily)) 
  
}


ME_discharge_out <- d.YI %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge *3600 * 24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily)) 

res.time <- ME_vol / ME_discharge_out

PBMS_in <- d.PBMS %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SMC_in <- d.6MC %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

YN_in <- d.YN %>%
  filter(year(date) == 2019) %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

SH_in <- d.sc.SH %>%
  filter(year(date) == 2019) %>%
  na.omit %>%
  group_by(yday(date)) %>%
  summarise(meandaily = mean(discharge * 3600 *24)) %>% 
  ungroup() %>%
  summarise(annual = sum(meandaily))

ME_discharge_in <- SH_in + PBMS_in + SMC_in + YN_in

ME_discharge_in/ME_discharge_out

(ME_vol + ME_discharge_in) - ME_discharge_out






