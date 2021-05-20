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


#Get multiple linear regression information
#field measurements of conductivity
fieldcondMO <- fieldcondMO %>%
  mutate(depth = ifelse(depth == 1.5, 2, depth)) %>%
  rename(Depth_m = depth) %>%
  mutate(Date = as.Date(date)) #formatting simple date for joinign

#chloride data 
labMO <- labMO  %>%
  mutate(Date = as.Date(date)) #formatting simple date for joinign

#joinign chloride with matching conductivity. Some manaul matching for 2-27-2020
chloride_cond_MO <- labMO %>%
  full_join(fieldcondMO, by = c("Date", "Depth_m", "ID")) %>%
  dplyr::select(Date, Depth_m, sp.cond, chloride_mgL, datetime_collected) %>%
  mutate(sp.cond = ifelse(Date == "2020-02-27" & Depth_m == 0, (MO_Epi_cond_data_20 %>% filter(date == as.POSIXct("2020-02-27 12:15:00", tz = "ETC/GMT-6")))$runningmean, sp.cond),
         sp.cond = ifelse(Date == "2020-02-27" & Depth_m == 20, (MO_Hypo_cond_data_20 %>% filter(date == as.POSIXct("2020-02-27 12:15:00", tz = "ETC/GMT-6")))$runningmean, sp.cond)) %>%
  drop_na()

#regression to predict chloride as a function of both specific conductivity and depth
summary(lm(chloride_mgL~sp.cond +Depth_m, chloride_cond_MO)) #r =0.6365, p = 8.832e-06

#multiple linear regression models to get coefficients, etc.
library(purrr)
library(broom)
MO_purrr <- chloride_cond_MO %>%
  nest(data = -Depth_m) %>%
  mutate(model = map(.x = data,
                     .f = ~ lm(chloride_mgL~sp.cond, data = .x)),
         tidy_model = map(.x = model,
                          .f = ~tidy(.x))) %>%
  unnest(tidy_model) %>%
  arrange(Depth_m)

#the regression figure
ggplot(chloride_cond_MO) +
  geom_point(aes(sp.cond, chloride_mgL)) +
  geom_smooth(aes(sp.cond, chloride_mgL, group = Depth_m, color = Depth_m), method = "lm", se = FALSE) +
  scale_color_viridis_c(option = "inferno") +
  theme_minimal() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"",
       y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Multiple linear regression of chloride concentration in Lake Monona as a function 
of both conductivity and depth.") +
  L_theme()

ggsave("Plots/cl_cond_linear_regression/Monona_multiplereg.png", width = 6.25, height = 4.25, units = "in")


#chloride estimate for epi and hypo during winter 2019-2020, 2020-2021
MO_winter_chloride <- rbind(MO_Epi_cond_data_20 %>% mutate(Depth_m = 0), MO_Hypo_cond_data_20 %>% mutate(Depth_m = 20), MO_Epi_cond_data_21 %>% mutate(Depth_m = 0), MO_Hypo_cond_data_21 %>% mutate(Depth_m = 20)) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 0,
                                   runningmean * (MO_purrr %>% filter(Depth_m == 0 &
                                                                        term == "sp.cond"))$estimate + (MO_purrr %>% filter(Depth_m == 0 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   NA
  )) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 20,
                                   runningmean * (MO_purrr %>% filter(Depth_m == 20 &
                                                                        term == "sp.cond"))$estimate + (MO_purrr %>% filter(Depth_m == 20 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  ))

ggplot(MO_winter_chloride) +
  geom_point(aes(date, chloride_use_mgL, color = Depth_m))


ggplot() +
  geom_point(labMO, mapping = aes(as.POSIXct(Date), Depth_m, color = chloride_mgL)) +
  geom_point(MO_winter_chloride, mapping = aes(date, Depth_m, color = chloride_use_mgL)) +
  scale_color_viridis_c(option = "inferno") +
  scale_y_reverse() +
  labs(x = "", y = "Depth (m)", color = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Chloride concentration profiles in Lake Monona throughout the study period.")  +
  L_theme() 


ggsave("Plots/chloride_time_series/Monona_alldata.png", height = 4.25, width = 6.25, units = "in")

















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



#formatting data to create a heatmap
labMO2 <- labMO %>%
  separate(date, c("date", "time"), sep = " ") %>%
  mutate(date = as.Date(date))

#heatmap of chloride concentration
ggplot(labMO2, aes(x = date, y = Depth_m, z = chloride_mgL)) +
  guides(fill = guide_colorsteps(barheight = unit(4, "cm"))) +
  geom_contour_filled(binwidth = 20) +
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


MO_chloride_plot <- clseries(labMO) + geom_point(aes(color = Depth_m)) + scale_color_viridis_c(direction = -1) +
  capt_clseries("Monona", "Lake Monona")
splot("chloride_time_series/", "MO")

MO_Epi_cond_data_20 <-MO_Epi_cond_data_20 %>%
  mutate(Depth_m = 2)
MO_Hypo_cond_data_20 <-MO_Hypo_cond_data_20 %>%
  mutate(Depth_m = 20)
MO_Epi_cond_data_21 <-MO_Epi_cond_data_21%>%
  mutate(Depth_m = 2)
MO_Hypo_cond_data_21 <- MO_Hypo_cond_data_21%>%
  mutate(Depth_m = 20)


ggplot() +
  geom_line(MO_Epi_cond_data_20, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  geom_line(MO_Hypo_cond_data_20, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  geom_line(MO_Epi_cond_data_21, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  geom_line(MO_Hypo_cond_data_21, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  geom_point(labMO, mapping = aes(as.Date(date), chloride_mgL*5, color = Depth_m)) +
  scale_color_viridis_c("Depth (m)", option = "inferno", direction = -1, begin = 0.25, end = 0.9) +
  scale_y_continuous(
    name = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C, 
    sec.axis = sec_axis(~./5, name = "Chloride Concentration"~(mg~L^-1))
  ) +
  labs(x = "", 
       caption = "Figure X. Specific Conductivity and chloride concentrations in Lake Monona over the 
entire study period. Lines are conductivity and points are chloride concentrations. Color 
indicates the measurement depth of the lake in meters.") + L_theme() 

ggsave("Plots/Monona_cond_chloride.png", height = 4.25, width = 6.25, units = "in")
