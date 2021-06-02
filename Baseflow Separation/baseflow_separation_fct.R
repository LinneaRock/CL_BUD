# Examples of a bunch of methods from 
# https://github.com/samzipper/GlobalBaseflow/blob/master/src/BaseflowSeparationFunctions.R
# https://github.com/samzipper/GlobalBaseflow

source('Baseflow Separation/Baseflow_Separation_Functions.R')
# packages required for sample data/examples
require(dataRetrieval)
require(lubridate)
library(tidyverse)

require(ggplot2)
require(magrittr)
require(reshape2)

get_eckhardt_bf <- function(siteno, discharge.df) {

  dv <- discharge.df 
  area_mi2 = readNWISsite(siteno)$drain_area_va

# Test with daily
# dv = dv %>% group_by(date = as.Date(date)) %>%
#   summarise(discharge = mean(discharge, na.rm = T))


# estimate recession constant
# The recession constant describes the rate at which baseflow recedes between storm events. 
# It is defined as the ratio of baseflow at the current time, to the baseflow one day earlier.
k <- baseflow_RecessionConstant(dv$runningmeandis, UB_prc=0.95, method="Brutsaert", min_pairs=30); k
BFImax <- baseflow_BFImax(Q=dv$runningmeandis, k=k)

## perform baseflow separations - using eckhardt for these analyses
# dv$HYSEP_fixed <- baseflow_HYSEP(Q = dv$runningmeandis, area_mi2 = area_mi2, method="fixed")
# dv$HYSEP_slide <- baseflow_HYSEP(Q = dv$runningmeandis, area_mi2 = area_mi2, method="sliding")
# dv$HYSEP_local <- baseflow_HYSEP(Q = dv$runningmeandis, area_mi2 = area_mi2, method="local")
# dv$UKIH <- baseflow_UKIH(Q = dv$runningmeandis, endrule="B")
# dv$BFLOW_1pass <- baseflow_BFLOW(Q = dv$runningmeandis, beta=0.95, passes=1)
# dv$BFLOW_3pass <- baseflow_BFLOW(Q = dv$runningmeandis, beta=0.95, passes=3)
dv$Eckhardt <- baseflow_Eckhardt(Q = dv$runningmeandis, BFImax=BFImax, k=k)

dv.melt <- 
  dv %>% 
  subset(select = c("date", "runningmeandis", "Eckhardt")) %>%
  #subset(select=c("date", "discharge", "HYSEP_fixed", "HYSEP_slide", "HYSEP_local", "UKIH", "BFLOW_1pass", "BFLOW_3pass", "Eckhardt")) %>% 
  melt(id=c("date", "runningmeandis")) %>%
  mutate(threshold = value + mean(dv$runningmeandis)/2)

}


calc_bfi <- function(data) {
## Calculate BFI
# Base flow is the component of streamflow that can be attributed to ground-water discharge into streams. 
# The BFI is the ratio of base flow to total flow, expressed as a percentage.
data %>% 
  group_by(variable) %>% 
  summarize(discharge.sum = sum(runningmeandis),
            baseflow.sum = sum(value),
            BFI = round(baseflow.sum/discharge.sum, 2))
}
  


## make plot
plot_bf_qt <- function(bfqt, name) {
  p.date.start = ymd("2019-12-15")
  p.date.end = ymd("2021-04-11")
  
  ggplot(subset(bfqt, date >= p.date.start & date <= p.date.end)) +
    geom_ribbon(data=subset(bfqt, date >= p.date.start & date <= p.date.end), 
                aes(x=date, ymin=0, ymax=runningmeandis), fill="black") +
    geom_ribbon(mapping = aes(x = date, ymin = 0, ymax = value),fill = "hot pink") +
    geom_line(mapping = aes(date, threshold), color = "blue") +
    #geom_line(aes(x=date, y=value, color=variable)) +
    scale_y_continuous(name="Discharge [m3]") +
    # scale_x_date(expand=c(0,0)) +
    scale_color_discrete(name="Method") +
    theme_bw() + 
    theme(panel.grid=element_blank(),
          legend.position=c(0.99, 0.99),
          legend.justification=c(1,1))
  
  ggsave(paste("Plots/QC_plots/Eckhardt_Method/separation/", name, ".png", sep = ""), height = 4.25, width =6.25, units = "in")
  
}


