library(tidyverse)
library(lubridate)
library(patchwork)
source("Functions/splot.R")

list <- read.csv("Data/Historical_External/impwtrs.csv")

state.cl <- list %>%
  #select(Official.Waterbody.Name,WATERS.ID..AU., Water.Type, Counties, Start.Mile, End.Mile, Size, Units, DNR.Category, Date.Listed, Source.Category, Pollutant, Impairment.Indicator, Status, TMDL.Priority, Confirmed.Year, Listing.Detail, Watersheds) %>%
  filter(Pollutant == "Chloride") %>%
  mutate(Confirmed.Year = mdy(Date.Listed),
         Confirmed.Year = year(Confirmed.Year))

n.county <- state.cl %>%
  count(County, sort = TRUE, name = "n_imp")

n.year <- state.cl %>%
  count(Confirmed.Year, sort = TRUE, name = "n_conf_imp")%>%
  arrange(Confirmed.Year)

n.cumu <- n.year %>%
  mutate(cumulative = ifelse(Confirmed.Year == 2012, 1, NA)) %>%
  mutate(cumulative = ifelse(Confirmed.Year == 2014, 5, cumulative)) %>%
  mutate(cumulative = ifelse(Confirmed.Year == 2016, 21, cumulative)) %>%
  mutate(cumulative = ifelse(Confirmed.Year == 2018, 40, cumulative)) %>%
  mutate(cumulative = ifelse(Confirmed.Year == 2020, 47, cumulative)) %>%
  mutate(cumulative = as.numeric(cumulative))

#why can't i figure out how to make a column with cumlulative sum? 
# n.cumu <- n.year %>%
#   mutate(cumulative = ifelse(Confirmed.Year == 2012, 1, NA)) %>%
#   rowwise() %>%
#   mutate(cumulative2 = ifelse(Confirmed.Year > 2012, n_conf_imp + lag(as.numeric(cumulative)), 1))
  

n.year.p <- ggplot(n.year, aes(Confirmed.Year, n_conf_imp)) +
  geom_bar(stat = "identity", color = "#1C366B",  fill = "#1C3668") +
  labs(x = "", 
       y = "Number of Waterbodies") +
 # ggtitle(label = "a") +
  L_theme()+
  scale_x_continuous(breaks =c(2012, 2014, 2016, 2018, 2020), labels = c(2012, 2014, 2016, 2018, 2020))

n.cumu.p <- ggplot(n.cumu, aes(Confirmed.Year, cumulative)) +
  geom_bar(stat = "identity", color = "#1C366B",  fill = "#1C3668") +
  labs(x = "", 
       y = "Number of Waterbodies") + 
 # ggtitle(label = "b") +
  L_theme() +
  scale_x_continuous(breaks =c(2012, 2014, 2016, 2018, 2020), labels = c(2012, 2014, 2016, 2018, 2020))
       
       
       
       
(n.year.p | n.cumu.p) + 
  plot_annotation(tag_levels = 'a',tag_suffix = ')',
                  caption = "Figure X. a) The number of waterbodies added to Wisconsin's impaired waters lists for chloride. 
The DNR publishes a new report every even year. b) The total amount of waterbodies impaired for 
chloride over time. There were 47 waterbodies actively impaired due to chloride concentrations 
exceeding toxicity thresholds in 2020. No waters have been removed from the list as of 2021. 
Data from 2020 Wisconsin Impaired Waters List – Wisconsin Department of Natural Resources 
(DNR, 2020).",
                  theme = theme(plot.tag = element_text(size = 10), 
                                plot.caption = element_text(size = 10, hjust = 0))) 


splot("Historical_Data_Viz/", "impwtrs")
