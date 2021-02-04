library(tidyverse)



library(EcoHydRology)

baseflow_DC <- BaseflowSeparation(DC_discharge$discharge, filter_parameter = 0.925, passes = 3) %>%
  mutate(obs = 1:n())

ggplot(baseflow_DC) + geom_line(aes(x = obs, y = bt)) + geom_line(aes(x = obs, y = qft), color = "red")

hydrograph(input = DC_discharge, streamflow2 = baseflow_DC[,1])











flow_prcp <- DC_discharge %>%
  separate(date, c("date", "time"), sep = " ") %>%
  group_by(date) %>%
  mutate(meandis = mean(discharge)) %>%
  select(date, meandis) %>%
  unique() %>%
  ungroup() %>%
  left_join(precip_temp_data, by = c("date" = "DATE"))


ggplot(flow_prcp) + geom_point(aes(PRCP, meandis))
