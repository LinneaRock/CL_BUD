library(tidyverse)
library(readxl)
source("Functions/splot.R")

watershed <- read_xlsx("Data/Historical_External/YaharaHist.xlsx") %>%
  mutate(KA = as.numeric(KA))

ggplot(watershed) +
  geom_point(aes(Date, ME, color = "#1C366B"), size = .5) +
  geom_point(aes(Date, MO, color = "#F24D29"), size = .5) +
  geom_point(aes(Date, WI, color = "#C4CFD0"), size = .5) +
  geom_point(aes(Date, KA, color = "#E5C4A1"), size = .5) +
  geom_point(aes(Date, WA, color = "#1DACE8"), size = .5) +
  geom_smooth(aes(Date, ME, color = "#1C366B"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, MO, color = "#F24D29"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, WI, color = "#C4CFD0"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, KA, color = "#E5C4A1"), size = 1, se = FALSE) +
  geom_smooth(aes(Date, WA, color = "#1DACE8"), size = 1, se = FALSE) +
  labs(y = "Chloride Concentration (mg/L)\n",
       x = ""#,
       #caption = "Figure 3 The long-term increasing chloride concentration trend 
#in the Yahara River watershed lakes (Public Health Madison Dane County, 2020)."
) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size =11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size = 8),
        plot.caption = element_text(size = 11, hjust = 0)) +
  #plot.caption.position = "plot") +
  scale_color_identity(guide = "legend",
                       breaks = c("#1C366B", "#F24D29", "#C4CFD0", "#1DACE8", "#E5C4A1"),
                       labels = c("Mendota", "Monona", "Wingra", "Waubesa", "Kegonsa"))


splot("Historical_Data_Viz/", "ChainLakes")



