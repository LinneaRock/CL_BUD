library(tidyverse)
library(lubridate)
library(ggpubr)


#plotting time series of both depths
ggplot() +
  geom_line(loggerME_Epi, mapping = aes(date, sp.cond, color = "#1DACE8")) +
  geom_line(loggerME_Hypo, mapping = aes(date, sp.cond, color = "#1C366B")) +
  labs(x = "",
       y = "Specific Conductivity (µS/cm) @ 25°C\n") +
  scale_color_manual(labels = c("Mendota 24m", "Mendota 1.5m"),
                     values = c("#1C366B", "#1DACE8")) +
  theme(legend.title = element_blank(), legend.position = "top",
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        axis.text = element_text(size =11),
        axis.title = element_text(size =11),
        legend.text = element_text(size =11))
ggsave(filename = paste("Plots/conductance_time_series/","ME", ".png", sep = ""))


