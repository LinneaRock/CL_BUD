library(tidyverse)
library(readxl)
source("Functions/splot.R")
source("Functions/L_theme.R")

statesalt <- read_xlsx("Data/Historical_External/DOT_Statewide.xlsx") %>%
  add_row(Winter = 2020, Tons = 329485)
statesalt2 <- statesalt %>%
  mutate(tonnes = Tons * 0.907185)

options(scipen = 999) #turn off scientific notation for graph

ggplot(statesalt2, aes(Winter, tonnes)) +
  geom_bar(stat = "identity", color = "black",  fill = "#1C3668") +
  geom_smooth(method = "lm", color = "#F24D29", se = FALSE) +
  labs(x = "Winter", y = "Salt applied in Wisconsin (Mg)",
  caption = "Figure X. Road salt use in metric tonnes (Mg) for the state of Wisconsin trunk highways 
from 1959 through winter 2021. There has been a significant increase in road salt use 
across the state (p < 0.0001). Data from Salt Inventory Reporting System – Wisconsin 
Department of Transportation (DOT, 2015, 2020a, 2020b).") +
  L_theme()

ggsave("Plots/Historical_Data_Viz/WI_roadsalt.png", width = 6.25, height = 4.25, units = "in")


fit <- lm(tonnes~Winter, statesalt2)
summary(fit)

earlydecade_ave_rate <- statesalt %>%
  filter(Winter < 1970) %>%
  summarise(mean(as.numeric(tons_per_lanemi)))

latedecade_ave_rate <- statesalt %>%
  filter(Winter > 2008) %>%
  summarise(mean(as.numeric(tons_per_lanemi)))

(latedecade_ave_rate / earlydecade_ave_rate) * 100
