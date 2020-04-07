library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(ggpubr)

#function to convert actual conductivity to specific conductivity 
SC <- function(AC, t) {
  AC / (1 - (25 - t) * .019)
}

#Loading in data from surface conductivity logger:
ME_SURF <- read.csv("HOBO_Loggers/MENDOTA/SURFACE_2019-20/20758346.csv") %>%
  mutate(char = as.character(Date),
         Date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>% #formatting date/time
  select(Date, Low.Range, Full.Range, Temp) %>% #selecting necessary columns
  filter(Date < "2020-04-01 9:45:00 ") #filtering datapooints collected post removal from lake

#Loading in data from bottom conductivity logger:
ME_BOTT <- read.csv("HOBO_Loggers/MENDOTA/BOTTOM_2019-20/20758341.csv") %>%
  mutate(char = as.character(Date),
         Date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>% #formatting date/time
  select(Date, Low.Range, Full.Range, Temp) %>% #selecting necessary columns
  filter(Date < "2020-04-01 9:45:00 ") #filtering datapooints collected post removal from lake

#plotting time series of surface conductivity data
ggplot(ME_SURF) +
  geom_line(aes(Date, Full.Range), color = "black") +
  labs(x = "Date",
       y = "Conductivity (uS/cm)") 

#plotting time series of bottom conductivity data
ggplot(ME_BOTT) +
  geom_line(aes(Date, Full.Range), color = "black") +
  labs(x = "Date",
       y = "Conductivity (uS/cm)") 

#plotting time series of both depths
ggplot() +
  geom_line(ME_SURF, mapping = aes(Date, Full.Range, color = "#1DACE8")) +
  geom_line(ME_BOTT, mapping = aes(Date, Full.Range, color = "#1C366B")) +
  labs(x = "Date",
       y = "Conductivity (uS/cm)") +
  scale_color_manual(labels = c("Mendota 24m", "Mendota 1.5m"),
                     values = c("#1C366B", "#1DACE8")) +
  theme(legend.title = element_blank(), legend.position = "top",
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"))

#converting actual condutance from HOBO loggers to specifc conductance 
ME_S_SC <- ME_SURF %>%
  mutate(Sp.cond = SC(Full.Range, Temp))

ME_B_SC <- ME_BOTT %>%
  mutate(Sp.cond = SC(Full.Range, Temp))
