library(tidyverse)
library(lubridate)
source("Functions/splot.R")

list <- read.csv("Data/Historical_External/impwtrs.csv")

state.cl <- list %>%
  select(Official.Waterbody.Name,WATERS.ID..AU., Water.Type, Counties, Start.Mile, End.Mile, Size, Units, DNR.Category, Date.Listed, Source.Category, Pollutant, Impairment.Indicator, Status, TMDL.Priority, Confirmed.Year, Listing.Detail, Watersheds) %>%
  filter(Pollutant == "Chloride")

n.county <- state.cl %>%
  count(Counties, sort = TRUE, name = "n_imp")

n.year <- state.cl %>%
  count(Confirmed.Year, sort = TRUE, name = "n_conf_imp")

ggplot(n.year, aes(Confirmed.Year, n_conf_imp)) +
  geom_bar(stat = "identity", color = "#1C366B",  fill = "#1C3668") +
  labs(x = "Year Confirmed Impaired for Chloride", 
       y = "Number of Waterbodies"#,
       #caption = "Figure 2 The number of waterbodies that have been added to Wisconsin's 
#impaired waters list for chloride. In 2012, 1 waterbody was confirmed impaired; 
#in 2015, 12 waterbodies were confirmed impaired; and in 2017, 27 waterbodies 
#were confirmed impaired. Data from 2018 Wisconsin Impaired Waters List – 
#Wisconsin Department of Natural Resources (DNR, 2018)."
  ) +
  theme(axis.text = element_text(size =11),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray88"),
        plot.caption = element_text(size = 11, hjust = 0))

splot("Historical_Data_Viz/", "impwtrs")
