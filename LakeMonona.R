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


#conductivity data from loggers -- remove points that were collected outside of the water
loggerMO_Epi20_1 <- loggerMO_Epi20 %>% 
  filter(date <= as.POSIXct("2020-05-12 12:45:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Hypo20_1 <- loggerMO_Hypo20 %>% 
  filter(date <= as.POSIXct("2020-05-12 12:45:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Epi21_1 <- loggerMO_Epi21 %>%
  filter(date <= as.POSIXct("2021-04-10 06:30:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water
loggerMO_Hypo21_1 <- loggerMO_Hypo21 %>% 
  filter(date <= as.POSIXct("2021-04-10 06:30:00", tz = "ETC/GMT-6")) #last collected data before logger was removed from the water

#remove any outliers
MO_Epi_cond_data_20 <- outlier_detect_remove(loggerMO_Epi20_1, "MO_epi_20") %>% mutate(year = year(date))
MO_Hypo_cond_data_20 <- outlier_detect_remove(loggerMO_Hypo20_1, "MO_hypo_20") %>% mutate(year = year(date))
MO_Epi_cond_data_21 <- outlier_detect_remove(loggerMO_Epi21_1, "MO_epi_21") %>% mutate(year = year(date))
MO_Hypo_cond_data_21 <- outlier_detect_remove(loggerMO_Hypo21_1, "MO_hypo_21") %>% mutate(year = year(date))

#conductivity timeseries for 2019-2020
lakecond(MO_Epi_cond_data_20, MO_Hypo_cond_data_20, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona during the winter and spring 2019-2020")
splot("conductance_time_series/", "MO_19-20")

#conductivity timeseries for 2020-2021
lakecond(MO_Epi_cond_data_21, MO_Hypo_cond_data_21, "Monona 20m", "Monona 1.5m") +
  capt_scseries("Lake Monona", "deep hole of Lake Monona during the winter and spring 2020-2021")
splot("conductance_time_series/", "MO_20-21")


p1 <- cond(MO_Epi_cond_data_20) + ylim(500, 700) + labs(x = "", y = "Epilimnion",
                                                        title = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) 
  
p2 <- cond(MO_Epi_cond_data_21) + ylim(500, 700) + labs(x = "", y = "Epilimnion",
                                                        title = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) 

MO_epi <- (p1 | p2)

h1 <- cond(MO_Hypo_cond_data_20) + ylim(498, 1350) + labs(x = "", y = "Hypolimnion",
                                                        title = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) 

h2 <- cond(MO_Hypo_cond_data_21) + ylim(498, 1350) + labs(x = "", y = "Hypolimnion",
                                                        title = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C) 

MO_hypo <- (h1 | h2)

datemin_1 <- min(MO_Epi_cond_data_20$date)
datemax_1 <- max(MO_Epi_cond_data_20$date)
datemin_2 <- min(MO_Epi_cond_data_21$date)
datemax_2 <- max(MO_Epi_cond_data_21$date)

w1 <- ggplot(precip_data, aes(date, PRCP)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(datemin_1, datemax_1)) +
  labs(x = "", y = "",
       title = "Daily Precipitation (mm)")

w2 <- ggplot(precip_data, aes(date, PRCP)) +
  geom_bar(stat = "identity") +
  L_theme() +
  scale_x_datetime(limits = c(datemin_2, datemax_2)) +
  labs(x = "", y = "",
       title = "Daily Precipitation (mm)")

winter_prcp <- (w1 | w2)

T1 <- ggplot(precip_data, mapping = aes(date, TAVG)) +
  geom_line() +
  L_theme() +
  scale_x_datetime(limits = c(datemin_1, datemax_1)) +
  geom_hline(yintercept = 0, color = "#F24D29") +
  labs(x = "", y = "",
       title = "Average Daily Temperature"*~degree*C~"")

T2 <- ggplot(precip_data, mapping = aes(date, TAVG)) +
  geom_line() +
  L_theme() +
  scale_x_datetime(limits = c(datemin_2, datemax_2)) +
  geom_hline(yintercept = 0, color = "#F24D29") +
  labs(x = "", y = "",
       title = "Average Daily Temperature"*~degree*C~"")

temp <- (T1 | T2)

(MO_epi /
    MO_hypo / 
    winter_prcp /
    temp) +
  plot_annotation(caption = "Lake Monona specific conductivity in the epilimnion and hypolimnion along with daily preciptation and avearage daily temperatures. Figures on the left are from winter/spring 2019-2020 and on the right are from winter/spring 2020-2021.")

labMO2 <- labMO %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(date = as.Date(date))

#heatmap of chloride concentration
ggplot(labMO2, aes(x = date, y = Depth_m, z = chloride_mgL)) +
  geom_contour_filled(binwidth = 10) +
  geom_point(aes(date, Depth_m), color = "#C4CFD0") +
  #xlim(c(as.Date("2020-01-01"), as.Date("2021-04-15"))) +
  theme_minimal() +
  scale_y_reverse() +
  scale_fill_viridis_d(option = "inferno") +
  labs(x = "", y = "Depth (m)", fill = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Chloride concentrations (mg/L) in Lake Monona over study period. Grey points indicate dates and where 
in the water column water samples were taken and analyzed for chloride.") +
  theme(plot.caption = element_text(size = 10, hjust = 0), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))
  
ggsave("Plots/chloride_time_series/monona_heatmap.png", height = 15, width = 20, units = "cm")




