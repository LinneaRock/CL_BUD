library(tidyverse)
library(readxl)
source("Functions/splot.R")
source("Functionsn/L_theme.R")

statesalt <- read_xlsx("Data/Historical_External/DOT_Statewide.xlsx")
statesalt2 <- statesalt %>%
  mutate(tonnes = Tons * 0.907185)

ggplot(statesalt2, aes(Winter, tonnes)) +
  geom_bar(stat = "identity", color = "#1C366B",  fill = "#1C3668") +
  geom_smooth(method = "lm", color = "#F24D29", se = FALSE) +
  labs(x = "Winter", y = "Salt applied in the state (Mg)",
  caption = "Figure X. History of road salt use in tonnes for the state of Wisconsin trunk highways from winter 1959-60 through 
winter 2019-20. There has been a significant increase in road salt use across the state (p < 2.2e-16). Data from 
Salt Inventory Reporting System – Wisconsin Department of Transportation (DOT, 2015, 2020a, 2020b).") +
  L_theme()

ggsave("Plots/Historical_Data_Viz/WI_roadsalt.png", width = 20, height = 15, units = "cm")
#splot("Historical_Data_Viz/", "WI Road Salt History")

fit <- lm(tonnes~Winter, statesalt2)
summary(fit)

earlydecade_ave_rate <- statesalt %>%
  filter(Winter < 1970) %>%
  summarise(mean(as.numeric(tons_per_lanemi)))

latedecade_ave_rate <- statesalt %>%
  filter(Winter > 2008) %>%
  summarise(mean(as.numeric(tons_per_lanemi)))

(latedecade_ave_rate / earlydecade_ave_rate) * 100
