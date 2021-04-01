library(tidyverse)
library(readxl)
source("Functions/L_theme.R")
source("Functions/splot.R")




upper <- read_xlsx("Data/Historical_External/wells_phmdc.xlsx", sheet = "upper") %>%
  drop_na(Year) %>%
  mutate(W6 = as.numeric(W6))
lower <- read_xlsx("Data/Historical_External/wells_phmdc.xlsx", sheet = "lower")


upper_5 <- upper %>%
  filter(Year == 1995 | Year == 2000 | Year == 2005 | Year == 2010 | Year == 2018) %>%
  pivot_longer(-Year, names_to = "well", values_to = "values")  %>%
  mutate(Aquifer = "Upper & Lower")

# a <- ggplot(upper_5, aes(Year, values, group = Year)) +
#   geom_boxplot(fill = "#1DACE8") +
#   stat_summary(fun.data = give.n, geom = "text", fun = median) +
#   labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) +
#   ggtitle(label = "a") + L_theme()

lower_5 <- lower %>%
  filter(Year == 1995 | Year == 2000 | Year == 2005 | Year == 2010 | Year == 2018) %>%
  pivot_longer(-Year, names_to = "well", values_to = "values") %>%
  mutate(Aquifer = "Lower Only")
# 
# b <- ggplot(lower_5, aes(Year, values, group = Year)) +
#   geom_boxplot(fill = "#1DACE8") +
#   stat_summary(fun.data = give.n, geom = "text", fun = median) +
#   labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + 
#   ggtitle(label = "b") + L_theme()
# 
# 
# (a | b) + 
#   plot_annotation(caption = "Figure X. Chloride concentrations in Madison's groundwater drinking wells every five years from 1995-2018. Numbers in boxes indicate how many wells were 
# included. a) Chloride in wells with short casings that draw from upper and lower aquifers. b) Chloride in wells with deep casing that draw from lower aquifers only 
# (Wenta, 2020; Wenta & Sorsa, 2011).",
#                   theme = theme(text = element_text(size = 10, hjust = 0))) 

#splot("Historical_Data_Viz/", "wells")



##############
all_wells <- upper_5 %>%
  full_join(lower_5) 

all_wells$Aquifer <- factor(all_wells$Aquifer, levels = c("Upper & Lower", "Lower Only"))

give.n <- function(x){
  return(c(y = median(x)*1.1, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

ggplot(all_wells, aes(Year, values, group = Year, fill = Aquifer)) +
  geom_boxplot(fatten = 0.5) +
  stat_summary(fun.data = give.n, geom = "text", fun = median, fontface = "bold") +
  labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + 
  facet_wrap(.~Aquifer) +
  L_theme() +
  theme(legend.position = "none",
        strip.background =element_rect(fill="white")) +
  scale_fill_manual(values = c("#C4CFD0","#1DACE8")) + 
  labs(caption = "Figure X. Chloride concentrations in Madison's groundwater drinking wells every five years from 1995-2018. Numbers 
in boxes indicate how many wells were included. Chloride in wells with short casings that draw from upper and 
lower aquifers show an increasing chloride trend. Chloride in wells with deep casing that draw from lower aquifers 
only show no trend (Wenta, 2020; Wenta & Sorsa, 2011).",
                  theme = theme(text = element_text(size = 10, hjust = 0))) 

splot("Historical_Data_Viz/", "wells")
