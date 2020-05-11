library(tidyverse)
library(readxl)
library(lubridate)

#function to read in data for chloride
hist <- function(X) {
  lab <- read_xlsx("Data/PHMDC.xlsx", sheet = X) %>%
    mutate(chloride_mgL = as.numeric(chloride_mgL))
  
}

HistME <- hist("Mendota") %>%
  mutate(lakeid = "ME")
HistMO <- hist("Monona")  %>%
  mutate(lakeid = "MO")
HistMarsh <- hist("1918 Marsh")
HistDC <- hist("Dorn Creek")
HistPB <- hist("Pheasant Branch Creek")
Hist6MC <- hist("Six Mile Creek")
HistUB <- hist("U Bay Creek")
HistWI <- hist("Wingra")
HistYR <- hist("Yahara River")
















#historical data of Yahara Chain Lakes 
watershed <- read_xlsx("Data/YaharaHist.xlsx") %>%
  mutate(KA = as.numeric(KA))
  
ggplot(watershed) +
  geom_point(aes(Date, ME, color = "#1C366B"), size = .5) +
  geom_point(aes(Date, MO, color = "#F24D29"), size = .5) +
  geom_point(aes(Date, WI, color = "#C4CFD0"), size = .5) +
  geom_point(aes(Date, KA, color = "#E5C4A1"), size = .5) +
  geom_point(aes(Date, WA, color = "#1DACE8"), size = .5) +
  geom_smooth(aes(Date, ME, color = "#1C366B"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, MO), color = "#F24D29", size = 1, se = FALSE) +
  geom_smooth(aes(Date, WI), color = "#C4CFD0", size = 1, se = FALSE) +
  geom_smooth(aes(Date, KA), color = "#E5C4A1", size = 1, se = FALSE) +
  geom_smooth(aes(Date, WA), color = "#1DACE8", size = 1, se = FALSE) +
  labs(y = "Chloride Concentration (mg/L)\n",
       x = "\nDate",
       caption = paste(strwrap("Figure 3 The long-term increasing chloride concentration trend in the Yahara River watershed lakes (Public Health Madison Dane County, 2020)."), collapse = "\n")) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size =13, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size =13, face = "bold"),
        plot.caption = element_text(size = 13)) +
        #plot.caption.position = "plot") +
  scale_color_identity(guide = "legend",
                       breaks = c("#1C366B", "#F24D29", "#C4CFD0", "#1DACE8", "#E5C4A1"),
                       labels = c("Mendota", "Monona", "Wingra", "Waubesa", "Kegonsa"))
ggsave("historicalyahara.png", height = 8, width = 11)
  



#figure of Mendota/Monona historical data
data = rbind(HistME, HistMO)

ggplot(data, aes(date, chloride_mgL)) +
  geom_point(aes(color = lakeid)) +
  geom_smooth(aes(group = lakeid, color = lakeid), se = FALSE) +
  labs(y = "Chloride Concentration (mg/L)\n",
       x = "\nDate",
       caption = paste(strwrap("Figure 3 The long-term increasing chloride concentration trend in the upper Yahara River watershed lakes (Public Health Madison Dane County, 2020)."), collapse = "\n")) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size =13, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size =13, face = "bold"),
        plot.caption = element_text(size = 15),
        plot.caption.position = "plot") +
  scale_color_manual(labels = c("Mendota", "Monona"),
                     values = c("#1C366B", "#F24D29"))
ggsave("longterm.png", height =10, width = 11)
