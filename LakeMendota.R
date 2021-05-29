#delete measurements that were collected outside of the water
loggerME_Epi1 <- loggerME_Epi %>%
  filter(date < as.POSIXct("2020-04-01 10:15:00", tz = "ETC/GMT-6"))
loggerME_Hypo1 <- loggerME_Hypo %>%
  filter(date < as.POSIXct("2020-04-01 10:15:00", tz = "ETC/GMT-6"))

#detect outliers
ME_Epi_cond_data <- outlier_detect_remove(loggerME_Epi1, "ME_epi") %>%
  mutate(Depth_m = 2)

ME_Hypo_cond_data <- outlier_detect_remove(loggerME_Hypo1, "ME_hypo") %>%
  mutate(Depth_m = 23.5)

#2019-20 Winter conductivity plot
lakecond(ME_Epi_cond_data, ME_Hypo_cond_data, "Mendota 23.5m", "Mendota 2m") +
  capt_scseries("Lake Mendota", "deep hole of Lake Mendota")
splot("conductance_time_series/", "ME")

#concentration plot
ME_chloride_plot <- clseries(labME) + geom_point(aes(color = Depth_m)) + scale_color_viridis_c("Depth (m)", option = "inferno",direction = -1, begin = 0.25, end = 0.9) +
  labs(caption = "Figure X. Chloride concentrations from grab samples in Lake Mendota over the entire study 
period. Color indicates the measurement depth of the lake in meters. Vertical dotted lines 
are ice-on and off dates with just the ice-off date in 2020 on this figure.") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-01-03 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2021-03-20 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-01-12 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2020-03-22 00:00:00")), linetype = "dotted")
  #capt_clseries("Mendota", "Lake Mendota")
splot("chloride_time_series/", "ME")






#Get multiple linear regression information
#field measurements of conductivity
fieldcondME <- fieldcondME %>%
  mutate(depth = ifelse(depth == 1.5, 2, depth),
         depth = ifelse(depth == 24, 23.5, depth)) %>%
  rename(Depth_m = depth)
fieldcondME <-fieldcondME  %>%
  mutate(Date = as.Date(date)) #formatting simple date for joinign

#chloride data 
labME <- labME  %>%
  mutate(Date = as.Date(date)) #formatting simple date for joinign

#conductivity from Mark's YSI sampling during summer 2020
ME_profile <- read_rds("Data/ME_YSI_2020/ME_profiles.rds") %>% 
  filter(sampledate != "2020-07-11") #sensors malfunctioned this date

ME_profile <- ME_profile %>%
  rename(Date = sampledate,
         Depth_m = depth,
         sp.cond = sp_cond) %>%
  dplyr::select(Date, Depth_m, sp.cond)

#joinign chloride with matching conductivity. Some manaul matching for 2-25-2020
chloride_cond_ME <- labME %>%
  full_join(fieldcondME, by = c("Date", "Depth_m", "ID")) %>%
  dplyr::select(Date, Depth_m, sp.cond, chloride_mgL, datetime_collected) %>%
  mutate(sp.cond = ifelse(Date == "2020-02-25" & Depth_m == 0, (ME_Epi_cond_data %>% filter(date == as.POSIXct("2020-02-25 12:15:00", tz = "ETC/GMT-6")))$runningmean, sp.cond),
         sp.cond = ifelse(Date == "2020-02-25" & Depth_m == 23.5, (ME_Hypo_cond_data %>% filter(date == as.POSIXct("2020-02-25 12:15:00", tz = "ETC/GMT-6")))$runningmean, sp.cond)) %>%
  left_join(ME_profile, by = c("Date", "Depth_m")) %>%
  mutate(sp.cond = ifelse(is.na(sp.cond.x), sp.cond.y, sp.cond.x)) %>%
  mutate(Depth_m = ifelse(Depth_m == 2, 0, Depth_m))

#regression to predict chloride as a function of both specific conductivity and depth
ME.lm <- summary(lm(chloride_mgL~sp.cond +Depth_m, chloride_cond_ME)) #r =0.17, p = 0.04


#multiple linear regression models to get coefficients, etc.
library(purrr)
library(broom)
ME_purrr <- chloride_cond_ME %>%
  nest(data = -Depth_m) %>%
  mutate(model = map(.x = data,
                     .f = ~ lm(chloride_mgL~sp.cond, data = .x)),
         tidy_model = map(.x = model,
                          .f = ~tidy(.x))) %>%
  unnest(tidy_model) %>%
  arrange(Depth_m)

#the regression figure
ggplot(chloride_cond_ME) +
  geom_point(aes(sp.cond, chloride_mgL, color = Depth_m)) +
  geom_smooth(aes(sp.cond, chloride_mgL, group = Depth_m, color = Depth_m), method = "lm", se = FALSE) +
  scale_color_viridis_c("Depth (m)", option = "inferno", direction = -1) +
  theme_minimal() +
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"",
       y = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Multiple linear regression of chloride concentration in Lake Mendota as a function 
of both conductivity and depth.") +
  L_theme()

ggsave("Plots/cl_cond_linear_regression/Mendota_multiplereg.png", width = 6.25, height = 4.25, units = "in")

#chloride estimate for epi and hypo during winter 2019-2020
ME_winter_chloride <- rbind(ME_Epi_cond_data, ME_Hypo_cond_data) %>%
  mutate(Depth_m = ifelse(Depth_m == 2, 0, Depth_m)) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 0,
    runningmean * (ME_purrr %>% filter(Depth_m == 0 &
                                         term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 0 &
                                                                                               term == "(Intercept)"))$estimate, 
    NA
  )) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 23.5,
                                   runningmean * (ME_purrr %>% filter(Depth_m == 23.5 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 23.5 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  ))

ggplot(ME_winter_chloride) +
  geom_line(aes(date, chloride_use_mgL, group = Depth_m))

ME_profile_chloride <- ME_profile %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 0,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 0 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 0 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   NA
  )) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 23.5,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 23.5 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 23.5 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  ))  %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 5,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 5 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 5 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  )) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 10,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 10 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 10 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  ))  %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 15,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 15 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 15 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  )) %>%
  mutate(chloride_use_mgL = ifelse(Depth_m == 20,
                                   sp.cond * (ME_purrr %>% filter(Depth_m == 20 &
                                                                        term == "sp.cond"))$estimate + (ME_purrr %>% filter(Depth_m == 20 &
                                                                                                                              term == "(Intercept)"))$estimate, 
                                   chloride_use_mgL
  ))


ggplot() +
  geom_point(labME, mapping = aes(as.POSIXct(Date), Depth_m, color = chloride_mgL)) +
  geom_point(ME_profile_chloride %>% filter(!is.na(chloride_use_mgL)), mapping = aes(as.POSIXct(Date), Depth_m, color = chloride_use_mgL)) +
  geom_point(ME_winter_chloride, mapping = aes(date, Depth_m, color = chloride_use_mgL)) +
  scale_color_viridis_c(option = "inferno") +
  scale_y_reverse() +
  labs(x = "", y = "Depth (m)", color = "Chloride Concentration"~(mg~L^-1),
       caption = "Figure X. Chloride concentration profiles in Lake Mendota throughout the study period.")  +
  L_theme() 
  

ggsave("Plots/chloride_time_series/Mendota_alldata.png", height = 4.25, width = 6.25, units = "in")


ggplot() +
  geom_line(ME_profile, mapping = aes(Date, sp.cond, color = Depth_m, group = Depth_m)) +
  geom_line(ME_Epi_cond_data, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  geom_line(ME_Hypo_cond_data, mapping = aes(as.Date(date), runningmean, color = Depth_m, group = Depth_m)) +
  scale_color_viridis_c(option = "inferno", begin = 0.25, end = 0.9) +
  #geom_point(labME, mapping = aes(as.Date(date), chloride_mgL*10, color = Depth_m)) +
  scale_color_viridis_c("Depth (m)", option = "inferno", direction = -1, begin = 0.25, end = 0.9) +
  labs(x = "", 
       y = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C,
       caption = "Figure X. Specific Conductivity in Lake Mendota over the entire study period. Color 
indicates the measurement depth of the lake in meters. Vertical dotted lines are ice-on
and off dates") + L_theme() +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-03 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2021-03-20 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-12 00:00:00")), linetype = "dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-22 00:00:00")), linetype = "dotted")
  

ggsave("Plots/conductance_time_series/Mendota_cond_chloride.png", height = 4.25, width = 6.25, units = "in")





