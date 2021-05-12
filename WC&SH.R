library(tidyverse)
library(ggpubr)
library(lubridate)
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

# #linear regression for chloride study of Spring Harbor in 2014-2016

#combine chloride and conductance datasets and get regression scatterplot and info
labSH1 <- labSH %>%
  left_join(cl_data_flow_sc)

ggplot(labSH1, aes(runningmean, chloride_mgL)) +
  geom_point(aes(color = season)) + 
  scale_color_manual(labels = c("April-October", "November-March"),
                     values = c("#1C366B", "#F24D29"))  +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #stat_cor() + 
  #stat_regline_equation() + 
  labs(x = "Specific Conductivity"~(mu~S~cm^-1)~"@ 25"*~degree*C~"\n", 
       y = "\nChloride Concentration"~(mg~L^-1),
       caption = "Figure X. Linear regression of chloride vs. conductivity in Spring Harbor from USGS 
sampling done 2014-2016. Colors of points indicate sampling season as 
April - October (non-salting) or November - March (salting).") +
  L_theme()
splot("cl_cond_linear_regression/", "SH")

SH.lm <- lm(chloride_mgL ~ runningmean, labSH1)
SH.slope <- round(coef(SH.lm)[2], 2)
SH.intercept <- round(coef(SH.lm)[1], 2)
SH.rsq <- round(summary(SH.lm)$r.squared, 2)
SH.p <- coef(summary(SH.lm))[2,4]
minobs <- min(labSH$chloride_mgL)

#use above info to create estimates for current Spring Harbor data
cumSkipNA <- function(x, FUNC)
{
  d <- deparse(substitute(FUNC))
  funs <- c("max", "min", "prod", "sum")
  stopifnot(is.numeric(x), d %in% funs)
  FUNC <- match.fun(paste0("cum", d))
  x[!is.na(x)] <- FUNC(x[!is.na(x)])
  x
}


SH_ts_mass <- d.sc.SH %>%
  mutate(chloride_predict = (SH.slope * sp.cond) + SH.intercept) %>% #estimate chloride [mg L^-1] for each specific conductivity measure
  mutate(chloride_use_mgL = NA) %>%#ifelse(is.na(chloride_mgL), chloride_predict, chloride_mgL)) %>% #use the actual data when we have it and estimated values in all other instances
  mutate(chloride_use_mgL = ifelse(chloride_predict <= 0, minobs, chloride_predict)) %>% #if concentration falls to or below zero, use the minimum observed value
  mutate(cl_rate_gs = chloride_use_mgL * discharge) %>% #load rate in [g s^-1] - 1000L and 1000mg unit coversions cancel out
  mutate(timestep = date - lag(date)) %>%
  mutate(cl_load_g = cl_rate_gs * timestep * 60) %>% #grams chloride every timestep (1-15 minutes) #integral to determine ~chloride mass [g] during the timestep [Chloride Rate g s^-1 * seconds]
  mutate(cl_load_g = as.numeric(cl_load_g)) %>%
  mutate(cumulative_cl_g = cumSkipNA(as.numeric(cl_load_g), sum)) %>% #grams chloride cumulative loading
  mutate(year = year(date)) %>%
  mutate(year_mon = paste(year(date), month(date), sep = "-")) %>%
  mutate(mon = months.POSIXt(date)) %>% #add months
  mutate(season = NA) %>% # add seasons
  mutate(season = ifelse(
                           mon == "November" |
                           mon == "December" |
                           mon == "January" |
                           mon == "February" |
                           mon == "March", "November - March", season),
         season = ifelse(is.na(season), "April - October", season)) %>%
  mutate(season_id = NA) %>%
  mutate(season_id = ifelse(
    year_mon == "2019-12" | year_mon == "2020-1" | year_mon == "2020-2" | year_mon == "2020-3",
    "2019-2020 Salting",
    season_id
  )) %>%
  mutate(season_id = ifelse(
    year_mon == "2020-4" | year_mon == "2020-5" | year_mon == "2020-6" | year_mon == "2020-7" | year_mon == "2020-8" | year_mon == "2020-9" | year_mon == "2020-10",
    "2020 Non-Salting",
    season_id)) %>%
  mutate(season_id = ifelse(
    year_mon == "2020-11" | year_mon == "2020-12" | year_mon == "2021-1" | year_mon == "2021-2" | year_mon == "2021-3" | year_mon == "2021-4",
    "2020-2021 Salting",
    season_id))











# 
# #same thing as above for q - chloride concentration regression.
# SH.discharge <- d.sc.SH %>%
#   na.omit() %>%
#   mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
#   group_by(day) %>%
#   summarise(discharge = mean(discharge)) %>%
#   rename(date = day)
# 
# q.cl(labSH, SH.discharge) +
#   captqc("Spring Harbor", "Spring Harbor storm sewer", labSH, SH.discharge)
# splot("QC_plots/", "SH")
# 
# #q - sp. conductivity regression 
# NAS <- d.sc.SH %>% #creating a garbage dataset to enable use of the function (it needs 2 datasets as the arguments and I can't figure out how to make this optional)
#   mutate(sp.cond = NA,
#          discharge = NA) %>%
#   rename(garbage = sp.cond,
#          garbage2 = discharge)
# 
# q.sc(d.sc.SH, NAS) +
#   captqec("Spring Harbor", "Spring Harbor storm sewer", d.sc.SH, NAS)
# splot("QC_plots/", "SH_cond")
# 
# 
# 
# 
# SH_cond_discharge <- cl_data_flow_sc %>%
#   rename(discharge = X_00060_00000,
#          sp_cond = X_00095_00000) %>%
#   select(dateTime, discharge, sp_cond) %>%
#   mutate(discharge = discharge * 0.028316847) %>%
#   rename(sp.cond = sp_cond,
#          date = dateTime) %>%
#   na.omit() %>%
#   mutate(day = as.Date(date, format = "%Y-%M-%D")) %>%
#   group_by(day) %>%
#   summarise(discharge = mean(discharge),
#             sp.cond = mean(sp.cond)) %>%
#   rename(date = day) %>%
#   left_join(labSH)
# 
# SH.lm <- lm(chloride_mgL ~ sp.cond, SH_cond_discharge)
# summary(SH.lm)
# 
# SH_load <- SH_cond_discharge %>%
#   mutate(daily_rate = chloride_mgL * discharge) %>%
#   mutate(daily_load = daily_rate * 3600 * 24)
# 
# 
# 
# 
# 
# 
# 
# 
