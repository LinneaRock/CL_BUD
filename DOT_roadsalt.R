library(tidyverse)
library(readxl)
source("Functions/splot.R")

statesalt <- read_xlsx("Data/Historical_External/DOT_Statewide.xlsx")

ggplot(statesalt, aes(Winter, Tons)) +
  geom_bar(stat = "identity", color = "#1C366B",  fill = "#1C3668") +
  #geom_smooth(method = "lm", color = "#F24D29", se = FALSE) +
  labs(x = "\nWinter", y = "Tons of Salt\n"#,
      #caption = "Figure by Linnea Rock using data provided by Wisconsin DOT"
  #caption = "Figure 1 History of road salt use in tons for the state of Wisconsin trunk highways 
  #from winter 1959-60 through winter 2018-19. Data from Salt Inventory Reporting 
  #System – Wisconsin Department of Transportation (DOT, 2015, 2020)."
  ) +
  theme(axis.text = element_text(size =11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        legend.text = element_text(size =11),
        plot.caption = element_text(size = 11, hjust = 0))

splot("Historical_Data_Viz/", "WI Road Salt History")
